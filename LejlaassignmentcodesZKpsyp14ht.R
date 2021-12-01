


library(psych) # for describe		
library(tidyverse) # for tidy code and ggplot		
library(cAIC4) # for cAIC		
library(r2glmm) # for r2beta		
library(lme4) # for lmer	
library(lmerTest) # to get singificance test in lmer	
library(MuMIn) # for r.squaredGLMM
library(lmtest)
library(car)
library(boot)

# let us load the data! :)


data_sample_1 = read_csv("https://tinyurl.com/yxm5rd89")

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

view(data_sample_1)


#_____________________________________________________________________
#First let's explore the data. We can do correlational plots between pain and the different variables and exclude the predicting variables that have a weak correlation since they are not good predictors of pain.
#(sex was excluded in my own last additional model 3. I know this was not part of the assignment but I just wanted to check if it made a difference).


data_sample_1 %>%
  mutate(rownum = row.names(data_sample_1)) %>%
  ggplot() + aes(x = pain, y = STAI_trait, label = rownum) +
  geom_label()




data_sample_1 %>%
  mutate(rownum = row.names(data_sample_1)) %>%
  ggplot() + aes(x = STAI_trait, y = pain_cat, label = rownum) +
  geom_label()


#lets do a plot! We can just change the variable in y and run the plot again and again to explore all the predicting variables. 

data_sample_1 %>%
  select(pain, sex) %>%
  ggplot() +
  aes(x = pain, y = sex) +
  geom_point()

#There is no correlation between pain and sex.



data_sample_1 %>%
  ggplot() +
  aes(x = pain,
      y = age) +
  geom_point(aes(color = mindfulness), shape = 21, fill = "blue") +
  geom_smooth()

data_sample_1 %>%
  ggplot() +
  aes(x = pain,
      y = cortisol_saliva) +
  geom_point(aes(color = pain), shape = 21, fill = "white") +
  geom_smooth()



data_sample_1_nooutliers %>%
  select(age, mindfulness, pain) %>%
  cor()

data_sample_1_nooutliers %>%
  select(pain, STAI_trait, pain_cat) %>%
  cor()


#we can use above code to explore the predicted variable (pain) with the other variables. 
#Age and mindfulness were negatively correlated with pain and pain_cat, cortisol saliva and STAI_trait were positively correlated with pain. 
#Sex had no correlation with pain.
#_______________________________________________________________________________


data_sample_1 %>% 
  summary()



mod_samdate2 <- lm(pain ~ age + sex + pain_cat + STAI_trait + cortisol_saliva + mindfulness, data = data_sample_1)


data_sample_1 %>%
  slice(c(88))
data_sample_1_nooutliers = data_sample_1 %>%
  slice(-c(88))

mod_samdate2 %>%
  plot(which = 2)

residuals_mod_samdate2 = enframe(residuals(mod_samdate2))
residuals_mod_samdate2 %>%
  ggplot() + aes(x = value) + geom_histogram()


#as you can see  the histogram is totally skewed, but since there is only one outlier it is alright to keep it since the rest of the data (many participants) countereffect the one outlier.
#But since there are only two outliers, let's exclude them.
#we want to have as reliable results as possible.

describe(residuals(mod_samdate2))

data_sample_1_nooutliers = data_sample_1 %>%
  slice(-c(88, 34))
mod_samdate2 = lm(pain ~ age + sex + pain_cat + STAI_trait + cortisol_saliva + mindfulness, data = data_sample_1_nooutliers)


#recheck the assumption of normality of residuals

describe(residuals(mod_samdate2))



residuals_mod_samdate2 = enframe(residuals(mod_samdate2))
residuals_mod_samdate2 %>%
  ggplot() + aes(x = value) + geom_histogram()

#Now the histogram looks much better and we have a normal distribution!

summary(mod_samdate2)

#I did not compare the new model without outliers to the last one with outliers. I just transformed the old data into a new model without outliers. 
#I did not make a comparison since I already know that the data was very skewed and not very realiable. The new data without outliers is much more reliable if you look at the histogram.
# Now lets get our confidence intervals by bootstrapping the dataset!


confint(mod_samdate2)


summary(mod_samdate2)$adj.r.squared


mod_samdate2 %>%
  residualPlots()


#we can see that the linerity seems to be violated (not a straight line) for some of the predictors: age, STAI_trait, mindfulness. But since it says roughly straight lines and the levels are not significant than we might keep them and interpret the plot as not violated.
#Now let's check the homoscedasticity. After running the test we can see that it's not violated. Run the ncvTest and the bptest to check for significance. It was not significant and that means that we can trust in our model.

mod_samdate2 %>%
  plot(which = 3)



mod_samdate2 %>%
  ncvTest() 


mod_samdate2 %>%
  bptest() 

#no multicollinearity, the vif test  variance inflation fator. In my dataset multiollinearity was not a problem.

mod_samdate2 %>%
  vif()

mod_samdate2 %>% 
  summary()


#Now let's create the first model with two predictors (sex and age), I did them in reverse order but since we are going to compare them with anova and AIC it does not matter in which order we create them!
#let's run some packages again just in case!

library(psych) # for describe		
library(tidyverse) # for tidy code and ggplot		
library(cAIC4) # for cAIC		
library(r2glmm) # for r2beta		
library(lme4) # for lmer	
library(lmerTest) # to get singificance test in lmer	
library(MuMIn) # for r.squaredGLMM
library(lmtest)
library(car)
library(boot)

# load data\t


data_sample_1 = read_csv("https://tinyurl.com/yxm5rd89")

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

view(data_sample_1)

data_sample_1 %>%
  mutate(rownum = row.names(data_sample_1)) %>%
  ggplot() + aes(x = pain, y = sex, label = rownum) +
  geom_label()


data_sample_1 %>%
  mutate(rownum = row.names(data_sample_1)) %>%
  ggplot() + aes(x = pain, y = age, label = rownum) +
  geom_label()


data_sample_1 %>% 
  summary()


#This is the first model prediting pain with age and sex as predictor variables. See below.



mod_data_sample_1 <- lm(pain ~ sex + age, data = data_sample_1)

# now check the residual error (influential cases) by running the cooks distance plot. See below:

mod_data_sample_1 %>%
  ggplot() + aes(x = pain, y = sex) + geom_point() +
  geom_smooth(method = "lm")

mod_data_sample_1 %>%
  ggplot() + aes(x = pain, y = age) + geom_point() +
  geom_smooth(method = "lm")

describe(residuals(mod_data_sample_1))

data_sample_1_nooutliers = data_sample_1 %>%
  slice(-c(88, 34))
mod_data_sample_1 = lm(pain ~ age + sex, data = data_sample_1_nooutliers)




#recheck the assumption of normality of residuals

describe(residuals(mod_data_sample_1))




residuals_mod_data_sample_1 = enframe(residuals(mod_data_sample_1))
residuals_mod_data_sample_1 %>%
  ggplot() + aes(x = value) + geom_histogram()

#Now the histogram looks much better and we have a normal distribution!

summary(mod_data_sample_1)

#I did not compare the new model without outliers to the last one with outliers. I just transformed the old data into a new model without outliers. 
#I did not make a comparison since I already know that the data was very skewed and not very realiable. The new data without outliers is much more reliable if you look at the histogram.
# Now lets get our confidence intervals by bootstrapping the dataset!


confint(mod_data_sample_1)

#confint.boot(mod_samdate2)  - could not get this function to work even after rerunning the packagas 


summary(mod_data_sample_1)$adj.r.squared


mod_data_sample_1 %>%
  residualPlots()


#we can see that the linerity seems to be violated (not a straight line) for some of the predictors: age. Run the ncvTest and the bptest to check for significance. 
#Linearity is broken in the this model because according to the tukey test it is significant at the 0.0571. This means that the model is not predicting accuretly. We can not trust the model.  

mod_data_sample_1%>%
  plot(which = 3)



mod_data_sample_1 %>%
  ncvTest() 


mod_data_sample_1 %>%
  bptest() 

#no multicollinearity, the vif test  variance inflation fator. In my dataset multiollinearity was not a problem.

mod_data_sample_1 %>%
  vif()

mod_data_sample_1 %>% 
  summary()



#by checking the summary of the two models we can see that the simpler model with only two predictors is significant and therefore probably a better model since the other more complex model with more predictors can lead to overfitting. But then again the simpler model broke linearity and that means that it is not good at predicting. We can create a thrid model and exclude some variables.
#But we could create a thrid model to see if we can choose better predictors. To do this we first have to explore the data and see wich predictors are highly correlated and which predictors have a weak correlation (they do not occur together with pain).
#And check the theory and literature. I would still use both models since it is interesting to explore and take into account more variables than just age and sex. Especially when choosing therapy forms that a potential patient might respond better to than another that for example does not have much training or experience with mindfulness.
#To understand postoperative pain we need both models but we could simplify the later model by reducing the number of predictors. I excluded sex since it has no correlation with pain.
#So let's just create a third model without sex as predictor. The more variables we have the more error we will have. 
# lets compare the two first models that we have by doing hierarchical regression.



summary(mod_data_sample_1)$adj.r.squared



summary(mod_samdate2)$adj.r.squared


AIC(mod_data_sample_1)

AIC(mod_samdate2)


anova(mod_data_sample_1, mod_samdate2)


#_________________________________________________________________________________

#creating a third model was not part of the first assignemnt but I just wanted to try and reate a simpler model with better predictors and see if it made any difference.

data_sample_1 %>% 
  summary()


mod_samdate3 <- lm(pain ~  age + STAI_trait + cortisol_saliva + mindfulness, data = data_sample_1)


data_sample_1 %>%
  slice(c(88))
data_sample_1_nooutliers = data_sample_1 %>%
  slice(-c(88))

mod_samdate3 %>%
  plot(which = 2)

residuals_mod_samdate3 = enframe(residuals(mod_samdate3))
residuals_mod_samdate3 %>%
  ggplot() + aes(x = value) + geom_histogram()


#as you can see  the histogram is totally skewed, but since there is only one outlier it is alright to keep it since the rest of the data (many participants) countereffect the one outlier.But since there are only two outliers, let's exclude them.
#we want to have as reliable results as possible.

describe(residuals(mod_samdate3))

data_sample_1_nooutliers = data_sample_1 %>%
  slice(-c(88, 34))
mod_samdate3 = lm(pain ~ age + STAI_trait + cortisol_saliva + mindfulness, data = data_sample_1_nooutliers)


#recheck the assumption of normality of residuals

describe(residuals(mod_samdate3))



residuals_mod_samdate3 = enframe(residuals(mod_samdate3))
residuals_mod_samdate3 %>%
  ggplot() + aes(x = value) + geom_histogram()

#Now the histogram looks much better and we have a normal distribution!

summary(mod_samdate3)


# The new data without outliers is much more reliable if you look at the histogram.
# Now lets get our confidence intervals by bootstrapping the dataset!


confint(mod_samdate3)
#Our model is nor the most confident since the confidence intervals are at 97,5 %. This means that we have to be careful when interpreting the results. 


summary(mod_samdate3)$adj.r.squared



mod_samdate3 %>%
  residualPlots()



mod_samdate3 %>%
  plot(which = 3)



mod_samdate3 %>%
  ncvTest() 


mod_samdate3 %>%
  bptest() 


#no multicollinearity, the vif test  variance inflation fator. In my dataset multiollinearity was not a problem. An linierity was not violated according to the tukey test.


mod_samdate3 %>%
  vif()

mod_samdate3 %>% 
  summary()



#______________________________________



#lets compare the three models



summary(mod_data_sample_1)$adj.r.squared



summary(mod_samdate2)$adj.r.squared


summary(mod_samdate3)$adj.r.squared


AIC(mod_data_sample_1)

AIC(mod_samdate2)

AIC(mod_samdate3)


anova(mod_data_sample_1, mod_samdate2, mod_samdate3)

# according to the AIC test model 2 and 3 are simmilar at prediting pain so we could go with either of these models. I would chose the second model since it was not part of the assignment to create a thrid model. I just wanted to try it and see if it made any difference when i excluded predictors (sex) that were not correlated with pain.
# In the simpler model with two predictor (age and sex) linerity was broken and therefore we can not trust that this model will predict well. Sex was not a good predictor since it had no correlation with pain, the relationship is offcourse not causal but it makes no sense to use sex as a predictor, also overfitting might be a problem.
#But let's chose the second model for prediction since the assignment was to decide which of the first two models were best to use. Note that for both the models the confidance intervalls were 97,5 and the models are not totally reliable.


#by checking the summary of the two models we can see that the simpler model with only two predictors is significant and therefore probably a better model since the other more complex model with more predictors can lead to overfitting, but then again in the simpler model the linearity was broken and the model can not be trusted, the confidence intervals were high aswell (97%) for both models.
#But we could create a thrid model to see if we can choose better predictors. To do this we first have to explore the data and see wich predictors are highly correlated.
#And check the theory and literature. I would still use both models since it is interesting to explore and take into account more variables than just age and sex. Especially when choosing therapy forms that a potential patient might respond better to than another; for example some patients have more training or experience with mindfulness other patiennts might have low on the anxiety trait and some patients high, this will affet the treatment.
#To understand postoperative pain we need both models but we could simplify the later model by reducing the number of predictors. 
#But lets not complicate this by creating another model lets just compare the two models that we have by doing hierarchical regression.
#In the assignment it did not specify if both salivary cortisol and blood cortisol should be included in the second model. I only included salivary cortisol sine it is supposed to be a better measuere than blood measures of cortisol
#(Vining., McGinley., Maksvytis., & Ho, 1983; El-Farhan., Rees., & Evans, 2017).  
#I created a thrid model just for fun. I would go with the second model because 
#The third model I excluded sex since sex did not have either a positive or negative correlation with pain in the correlation test.
#I created a fourth model where I excluded sex and pain_cat since it is basically very simillar to state trait anxiety. If one has high on anxiety they might be high on pain cathestrophizing. We dont need too many variables.
#In my own final model I excluded sex and pain_cat and that model had the lowest AIC. So I would personally go with that model. But since it was not part of the assignment and the point was to choose between model 1 and model 2 I would go with model 2 with more predictors since the first model broke linearity and second model did not break linearity.


summary(mod_data_sample_1)$adj.r.squared



summary(mod_samdate2)$adj.r.squared


summary(mod_samdate3)$adj.r.squared

AIC(mod_data_sample_1)

AIC(mod_samdate2)

AIC(mod_samdate3)

summary(mod_data_sample_1)
summary(mod_samdate2)
summary(mod_samdate3)

confint(mod_data_sample_1)
confint(mod_samdate2)
confint(mod_samdate3)


summary(mod_samdate3)


#_________________________________ Assignment part 2 - backward regression


mod_samdate4 <- lm(pain ~ age + sex + pain_cat + STAI_trait + cortisol_saliva + mindfulness + weight + IQ + household_income, data = data_sample_1)


data_sample_1 %>%
  slice(c(88))
data_sample_1_nooutliers = data_sample_1 %>%
  slice(-c(88))

mod_samdate4 %>%
  plot(which = 2)

residuals_mod_samdate4 = enframe(residuals(mod_samdate4))
residuals_mod_samdate4 %>%
  ggplot() + aes(x = value) + geom_histogram()

#as you can see  the histogram is totally skewed, but since there is only one outlier it is alright to keep it since the rest of the data (many participants) countereffect the one outlier.But since there are only two outliers, let's exclude them.
#we want to have as reliable results as possible.

describe(residuals(mod_samdate2))

data_sample_1_nooutliers = data_sample_1 %>%
  slice(-c(88, 34))
mod_samdate4 = lm(pain ~ age + sex + pain_cat + STAI_trait + cortisol_saliva + mindfulness + weight + IQ + household_income, data = data_sample_1_nooutliers)


#recheck the assumption of normality of residuals

describe(residuals(mod_samdate4))



residuals_mod_samdate4 = enframe(residuals(mod_samdate4))
residuals_mod_samdate4 %>%
  ggplot() + aes(x = value) + geom_histogram()

#Now the histogram looks much better and we have a normal distribution!

summary(mod_samdate4)

#I did not compare the new model without outliers to the last one with outliers. I just transformed the old data into a new model without outliers. 
#I did not make a comparison since I already know that the data was very skewed and not very realiable. The new data without outliers is much more reliable if you look at the histogram.
# Now lets get our confidence intervals by bootstrapping the dataset!


confint(mod_samdate4)


summary(mod_samdate4)$adj.r.squared


mod_samdate4 %>%
  residualPlots()


#we can see that the linerity according to the Tukey test is not significant. Liniearity is not broken.
#Now let's check the homoscedasticity. After running the test we can see that it's not violated. Run the ncvTest and the bptest to check for significance. It was not significant and that means that we can trust in our model.

mod_samdate4 %>%
  plot(which = 3)



mod_samdate4 %>%
  ncvTest() 


mod_samdate4 %>%
  bptest() 

#no multicollinearity, the vif test  variance inflation fator. In my dataset multiollinearity was not a problem.

mod_samdate4 %>%
  vif()

mod_samdate4 %>% 
  summary()



mod_samdate4_back = step(mod_samdate4, direction = "backward")


summary(mod_samdate4_back)$adj.r.squared

summary(mod_samdate4_back)$adj.r.squared


#______________________________________________________ creating two new models below: the backward model and the and the theory based model




data_sample_1 %>% 
  summary()



mod_backwardmodel <- lm(pain ~ pain_cat + STAI_trait + cortisol_saliva + mindfulness, data = data_sample_1)


data_sample_1 %>%
  slice(c(88))
data_sample_1_nooutliers = data_sample_1 %>%
  slice(-c(88))

mod_backwardmodel %>%
  plot(which = 2)

residuals_mod_backwardmodel = enframe(residuals(mod_backwardmodel))
residuals_mod_backwardmodel %>%
  ggplot() + aes(x = value) + geom_histogram()


#as you can see  the histogram is totally skewed, but since there is only one outlier it is alright to keep it since the rest of the data (many participants) countereffect the one outlier.But since there are only two outliers, let's exclude them.
#we want to have as reliable results as possible.

describe(residuals(mod_backwardmodel))

data_sample_1_nooutliers = data_sample_1 %>%
  slice(-c(88, 34))
mod_backwardmodel = lm(pain ~ pain_cat + STAI_trait + cortisol_saliva + mindfulness, data = data_sample_1_nooutliers)


#recheck the assumption of normality of residuals

describe(residuals(mod_backwardmodel))



residuals_mod_backwardmodel = enframe(residuals(mod_backwardmodel))
residuals_mod_backwardmodel%>%
  ggplot() + aes(x = value) + geom_histogram()

#Now the histogram looks much better and we have a normal distribution!

summary(mod_backwardmodel)

# Now lets get our confidence intervals by bootstrapping the dataset!


confint(mod_backwardmodel)


summary(mod_backwardmodel)$adj.r.squared


mod_backwardmodel %>%
  residualPlots()


#we can see that the linerity is not violated according to Tukey test.

mod_backwardmodel %>%
  plot(which = 3)



mod_backwardmodel %>%
  ncvTest() 


mod_backwardmodel%>%
  bptest() 

#no multicollinearity, the vif test  variance inflation fator. In my dataset multiollinearity was not a problem.

mod_backwardmodel %>%
  vif()

mod_backwardmodel %>% 
  summary()

#_______________________________________________________________________________________________________________ below is the theory based model




data_sample_1 %>% 
  summary()



mod_theory_based<- lm(pain ~ age + sex + pain_cat + STAI_trait + cortisol_saliva + mindfulness, data = data_sample_1)


data_sample_1 %>%
  slice(c(88))
data_sample_1_nooutliers = data_sample_1 %>%
  slice(-c(88))

mod_theory_based %>%
  plot(which = 2)

residuals_theory_based = enframe(residuals(mod_theory_based))
residuals_mod_theory_based %>%
  ggplot() + aes(x = value) + geom_histogram()


#as you can see  the histogram is totally skewed, but since there is only one outlier it is alright to keep it since the rest of the data (many participants) countereffect the one outlier.But since there are only two outliers, let's exclude them.
#we want to have as reliable results as possible.

describe(residuals(mod_theory_based))

data_sample_1_nooutliers = data_sample_1 %>%
  slice(-c(88, 34))
mod_theory_based = lm(pain ~ age + sex + pain_cat + STAI_trait + cortisol_saliva + mindfulness, data = data_sample_1_nooutliers)


#recheck the assumption of normality of residuals

describe(residuals(mod_theory_based))



residuals_mod_theory_based = enframe(residuals(mod_theory_based))
residuals_mod_theory_based %>%
  ggplot() + aes(x = value) + geom_histogram()

#Now the histogram looks much better and we have a normal distribution!

summary(mod_theory_based)

# Now lets get our confidence intervals by bootstrapping the dataset!


confint(mod_theory_based)

#confint.boot(mod_samdate2)  - could not get this function to work even after rerunning the packagas 


summary(mod_theory_based)$adj.r.squared


mod_theory_based %>%
  residualPlots()


#we can see that the linerity is not violated.
#Now let's check the homoscedasticity. After running the test we can see that it's not violated. Run the ncvTest and the bptest to check for significance. It was not significant and that means that we can trust in our model.

mod_theory_based %>%
  plot(which = 3)



mod_theory_based %>%
  ncvTest() 


mod_theory_based  %>%
  bptest() 

#no multicollinearity, the vif test  variance inflation fator. In my dataset multiollinearity was not a problem.

mod_theory_based %>%
  vif()

mod_theory_based %>% 
  summary()

#lets do the AIC test and compare the backward model with the theory based model.
#_______________________________________________________________________________________________________________




summary(mod_backwardmodel)$adj.r.squared



summary(mod_theory_based)$adj.r.squared



AIC(mod_backwardmodel)

AIC(mod_theory_based)


confint(mod_backwardmodel)
confint(mod_theory_based)




anova(mod_backwardmodel, mod_theory_based)


summary(mod_backwardmodel)


#_________________________ Mod samdate4 is the backwards regression model. Now lets compare the backwardsregression model to the backward model that we created. We will do this by running AIC and ANOVA.


AIC(mod_samdate4)
AIC(mod_backwardmodel)

anova(mod_samdate4, mod_backwardmodel)

#___________________________________________________________________________ I forgot to run anova for model 1 and 2 in the first assignment so let's just do that now.

anova(mod_data_sample_1, mod_samdate2, mod_samdate3)

summary(mod_samdate2)
summary(mod_data_sample_1)
AIC(mod_data_sample_1)
AIC(mod_samdate2)

#____________________________________________________ Now lets read the file data sample 2!




home_sample_2 = read.csv("https://tinyurl.com/ha-dataset1")



view(home_sample_2)


mod_backwardmodel <- lm(pain ~ pain_cat + STAI_trait + cortisol_saliva + mindfulness, data = home_sample_2)
mod_theory_based  <- lm(pain ~ age + sex + pain_cat + STAI_trait + cortisol_saliva + mindfulness, data = home_sample_2)


home_sample_2 %>%
  mutate(rownum = row.names(home_sample_2)) %>%
  ggplot() + aes(x = pain, y = mindfulness, label = rownum) +
  geom_label()



summary(mod_backwardmodel)$adj.r.squared

summary(mod_theory_based)$adj.r.squared



AIC(mod_backwardmodel)
AIC(mod_theory_based)

anova(mod_backwardmodel, mod_theory_based)

#Note that I did not exclude the outliers in the home_sample_2 dataset. Since there is basically only one outlier I kept it because the rest of the data counterbalances the effect of the one outlier.

#that is all for assignment 1 and 2. Unfortunetley I had no time or room to do the last part: assignment 3. 

summary(mod_backwardmodel)
summary(mod_theory_based)
