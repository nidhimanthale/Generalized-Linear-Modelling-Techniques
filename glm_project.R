#################### Reading data ####################
# Create variables
Gender <- rep(c(1,0) , each = 16)
Severe <- rep(c(1,0), each = 8, times = 2)
Information <- rep(c(1,0), each = 4, times = 4)
Age <- rep(1:4, times = 8)
n <- c(18, 23, 22, 17, 19, 35, 30, 22, 24, 37, 29, 24, 
       28, 42, 37, 30, 11, 25, 12, 8, 14, 34, 22, 21, 
       18, 28, 24, 8, 28, 47, 45, 30)
Behaviour <- c(11, 14, 11, 5, 4, 15, 8, 8, 10, 13, 8, 6,
               11, 14, 15, 9, 6, 13, 7, 8, 7, 15, 8, 5, 
               12, 15, 7, 1, 13, 21, 11, 6)

#################### Preliminary Analyses ####################
# Variable to represent proportion of changed behavious out of total people
Prop <- Behaviour/n

par(mfrow = c(2,2))

# Plot graphs of explanatory variables
plot(Gender, Prop)
# Females are more likely to change their behaviour related to hygiene 
# than males
plot(Severe, Prop)
# People who believe that the consequences of contracting swine flu 
# were severe are more likely change their behaviour related to hygiene
plot(Information, Prop)
# People who believe that the information available about swine flu 
# was adequate are more likely to change their behaviour related to hygiene
plot(Age, Prop)
# As the age increases people are less likely to change their
# behaviour related to hygiene

# From the plots, we see that there is a potential outlier

#################### Model Building and Selection #################### 
# Logistic Regression Models

# create response variable
y <- cbind(Behaviour, n-Behaviour)

# Additive Model
additive_model <- glm(y ~ factor(Gender) + factor(Severe) +
                    factor(Information) + factor(Age), 
                  family = binomial(link = logit))
summary(additive_model)
# Critical value for chi-squared distribution for 25 degrees of freedom
# at 5% significance level is 37.6525 
# Residual deviance of model is 37.813
# Residual deviance of model is greater than critical value 
# Model is not a good fit
# Consider adding interaction terms

# Forward Selection

# Multiplicative Model 1 
# Interaction between Gender and Severe
model1 <- glm(y ~ factor(Gender) + factor(Severe) +
                    factor(Information) + factor(Age) +
                    factor(Gender):factor(Severe),
                  family = binomial(link = logit))
summary(model1)
anova(additive_model, model1)
# Critical value for chi-squared distribution for 1 degree of freedom
# at 5% significance level is 3.8415
# Difference in deviance of models is 0.041994
# Difference deviance of model is lesser than critical value 
# Model is not a better fit than additive model
# p-value of interaction term is 0.83763 (> 0.05)
# Interaction term is not significant
# Delete this term
# Consider adding other interaction terms

# Multiplicative Model 2 
# Interaction between Gender and Information
model2 <- glm(y ~ factor(Gender) + factor(Severe) +
                    factor(Information) + factor(Age) +
                    factor(Gender):factor(Information),
                  family = binomial(link = logit))
summary(model2)
anova(additive_model, model2)
# Critical value for chi-squared distribution for 1 degree of freedom
# at 5% significance level is 3.8415
# Difference in deviance of models is 1.3182
# Difference deviance of model is lesser than critical value 
# Model is not a better fit than additive model
# p-value of interaction term is 0.25108 (> 0.05)
# Interaction term is not significant
# Delete this term
# Consider adding other interaction terms

# Multiplicative Model 3
# Interaction between Gender and Age
model3 <- glm(y ~ factor(Gender) + factor(Severe) +
                    factor(Information) + factor(Age) +
                    factor(Gender):factor(Age),
                  family = binomial(link = logit))
summary(model3)
anova(additive_model, model3)
# Critical value for chi-squared distribution for 3 degrees of freedom
# at 5% significance level is 7.8147
# Difference in deviance of models is 2.7202
# Difference deviance of model is lesser than critical value 
# Model is not a better fit than additive model
# p-values of interaction terms are 0.47992,  0.11974, 0.27927 
# (all > 0.05)
# Interaction term is not significant
# Delete this term
# Consider adding other interaction terms

# Multiplicative Model 4
# Interaction between Severe and Information
model4 <- glm(y ~ factor(Gender) + factor(Severe) +
                    factor(Information) + factor(Age) +
                    factor(Severe):factor(Information),
                  family = binomial(link = logit))
summary(model4)
anova(additive_model, model4)
# Critical value for chi-squared distribution for 1 degree of freedom
# at 5% significance level is 3.8415
# Difference in deviance of models is 5.2312
# Difference deviance of model is greater than critical value 
# Model is a better fit than additive model
# p-value of interaction term is 0.02263 (< 0.05)
# Interaction term is significant
# Retain this term

# Multiplicative Model 5
# Interaction between (Severe and Information) and (Severe and Age)
model5 <- glm(y ~ factor(Gender) + factor(Severe) +
                    factor(Information) + factor(Age) +
                    factor(Severe):factor(Information) +
                    factor(Severe):factor(Age),
                  family = binomial(link = logit))
summary(model5)
anova(model4, model5)
# Critical value for chi-squared distribution for 3 degrees of freedom
# at 5% significance level is 7.8147
# Difference in deviance of models is 2.9344
# Difference deviance of model is lesser than critical value 
# Model is not a better fit than previous model(glm_model5)
# p-value of interaction terms are 0.30534, 0.22490, 0.09795
# all > 0.05
# Interaction term is not significant
# Delete this term
# Consider adding other interaction terms

# Multiplicative Model 6
# Interaction between (Severe and Information) and (Information and Age)
model6 <- glm(y ~ factor(Gender) + factor(Severe) +
                    factor(Information) + factor(Age) +
                    factor(Severe):factor(Information) +
                    factor(Information):factor(Age),
                  family = binomial(link = logit))
summary(model5)
anova(model4, model6)
# Critical value for chi-squared distribution for 3 degrees of freedom
# at 5% significance level is 7.8147
# Difference in deviance of models is 0.85032
# Difference deviance of model is lesser than critical value 
# Model is not a better fit than previous model(glm_model5)
# p-value of interaction terms are 0.3963, 0.4322, 0.5658
# all > 0.05
# Interaction term is not significant
# Delete this term

# Multiplicative model 4 i.e. model with interaction term
# between Severe and Information variables is a better fit model
# than the additive model
summary(model4)
# Critical value for chi-squared distribution for 24 degrees of freedom
# at 5% significance level is 36.4150 
# Residual deviance of model is 32.582
# Residual deviance of model is lesser than critical value 
# Model is a good fit

#################### Diagnostics ####################

h <- lm.influence(model4)$hat
rpear <- residuals(model4, "pearson")/sqrt(1-h)
rdev <- residuals(model4, "deviance")/sqrt(1-h)

####### Checking the systematic component #######
par(mfrow = c(1,1))

# Plot of residuals versus case number
plot(rpear, main = "Index Plot of Pearson Residuals")
plot(rdev, main = "Index Plot of Deviance Residuals")

# Plot of residuals versus linear predictor
plot(model4$linear.predictors, rpear, main = "Plot of Pearson Residuals vs Linear Predictors")
plot(model4$linear.predictors, rdev, main = "Plot of Deviance Residuals vs Linear Predictors")

# We see a random scatter about zero and constant range.
# Thus no problem with systemic component.

####### Checking for outliers #######

# Plot of residuals versus case number
plot(rpear, main = "Index Plot of Pearson Residuals")
abline(h = c(-2, 2), col = "red")
identify(rpear)
rpear[20]
# Case 20 is an outlier

####### Checking for high leverage cases #######
# Calculate upper limit for h-values
# 2 * (p+1) / n
h_limit <- 2 * (7 + 1) / 32
# Plot of h values
plot(h, ylim = 0:1)
abline(h = h_limit, col = "red")
# No point lies above the line
# No cases with high leverage exist

####### Checking for high influence cases #######
# Calculate cook's distance for all cases 
D <- rpear * rpear * h / (2 * (1 - h))
# Plot cook's distances
plot(D)
identify(D)

# Cases 15 and 20 are high influence cases
rpear[15]
# Case 15 is almost an outlier
rpear[20]
# Case 20 is an outlier

# For case 20 all individuals changed their behaviour 

####### Checking for over-dispersion #######
# mean_deviance  = deviance / (n - p)
mean_deviance <- model4$deviance / (32 - 7)
# Mean deviance is close to 1 
# No over-dispersion exists

####### Checking for sparse data #######
summary(model4)
# Number of Fisher Scoring iterations = 4
# Standard errors are small
# No sparse data

#################### Interpretation of terms in the model ####################
summary(model4)

# Interpretation of Gender
exp(-0.20409)
1 - exp(-0.20409)
# Males are 18.5% less likely to change their behaviour than females
# p-value = 0.16863 (> 0.05)
# This difference is not significant

# Interpretation of Age
exp(-0.08784)
1 - exp(-0.08784)
# Individuals aged between 40-50 are 8.41% less likely to 
# change their behaviour than individuals aged less than 40
# p-value = 0.66543 (> 0.05)
# This difference is not significant

exp(-0.50742)
1 - exp(-0.50742)
# Individuals aged between 50-60 are 39.8% less likely to 
# change their behaviour than individuals aged less than 40
# p-value = 0.01856 (< 0.05)
# This difference is significant

exp(-0.68213)
1 - exp(-0.68213)
# Individuals aged above 60 are 49.45% less likely to 
# change their behaviour than individuals aged less than 40
# p-value = 0.00407 (< 0.05)
# This difference is significant

# Interpretation of Severe and Information
exp(0.68915)
exp(0.68915) - 1
# Individuals who believe that the consequences of contractign swine-flu are severe
# and the information available about swine-flu is adequate 
# are 99.2% more likely to change their behaviour than individuals 
# who believe that the consequences of contractign swine-flu are not severe
# and the information available about swine-flu is not adequate 
# p-value = 0.02263 (< 0.05)
# This difference is significant