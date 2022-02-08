#load packages
library(tidyverse) # Load the tidyverse packages
library(afex) # ANOVA functions
library(emmeans) # Needed for pairwise comparisons

#read our data
my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/12_glm_anova_pt2/master/data/ancova_data.csv")
head(my_data)

#We see Condition isn’t properly coded as a factor, so let’s fix that.
my_data <- my_data %>% 
  mutate(Condition = factor(Condition))
head(my_data)

#summarising our data
my_data %>%
  group_by(Condition) %>%
  summarise(mean_ability = mean(Ability))

#visualising our data 
set.seed(1234)
ggplot(my_data, aes(x = Gaming, y = Ability,  colour = Condition)) + 
  geom_point(size = 3, alpha = .9) +
  labs(x = "Gaming Frequency (hours per week)", 
       y = "Motor Ability") +
  theme_minimal() +
  theme(text = element_text(size = 11)) 

#first lets builid an ANOVA ignoring covariance
anova_model <-aov_4(Ability ~ Condition + (1 | Participant), data = my_data)
anova(anova_model)
#On the basis of this output, it appears we have an effect of Condition. 

#To explore this further, we would use the emmeans() function to run the pairwise comparisons.
emmeans(anova_model, pairwise ~ Condition)
#On the basis of this, we might conclude that we have an effect of Condition, and that each of our three groups differs significantly from the others. But would this be right? No, because we haven’t taken account of our covariate.

#we will build our ANCOVA model, adding our covariate before our experimental Condition manipulation. We set the factorize parameter to be FALSE so that it is treated as a continuous predictor, rather than an experimental factor in our model.
model_ancova <- aov_4(Ability ~ Gaming + Condition + (1 | Participant), data = my_data, factorize = FALSE)
anova(model_ancova)
#On this basis of this output, we see that we no longer have an effect of Condition, but we do have an effect of our covariate. 

#We can use the emmeans() function to produce the adjusted means (i.e., the means for each of our three groups taking into account the influence of our covariate).
emmeans(model_ancova, pairwise ~ Condition)
#the adjust mean differs significantly from the first model.

AN(C)OVA as a Special Case of Regression

#First we visualise the data with Condition on the x-axis.
my_data %>%
  ggplot(aes(x = Condition, y = Ability, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .05, alpha = .8) +
  labs(x = "Condition", 
       y = "Motor Ability") +
  stat_summary(fun.data = mean_cl_boot, colour = "black") +
  guides(colour = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 12)) 

#check how our Condition factor is currently coded in terms of its contrasts. 
contrasts(my_data$Condition)

#We want our Water group to be the reference level (thus corresponding to the intercept of our linear model), and dummy coded as (0, 0) but it’s not currently coded as such. Let’s fix that.
my_data <- my_data %>%
  mutate(Condition = fct_relevel(Condition, 
                                 levels = c("Water", "Double Espresso", "Single Espresso")))

ANOVA as a linear model
#Let’s model our linear model using the lm() function and examine the result.
model_lm <- lm(Ability ~ Condition, data = my_data)
model_lm

#We can see that the Intercept corresponds to the mean of our Water Condition. To work out the mean Ability of our Double Espresso Group, we use the coding for the Double Espresso group (1, 0) with our equation:
Ability = Intercept + β1(Double Espresso) + β2(Single Espresso)
Ability = 4.817 + 4.199(1) + 1.871(0)
Ability = 4.817 + 4.199
Ability = 9.016

#To work out the mean Ability of our Double Espresso Group, we use the coding for the Double Espresso group (0, 1) with our equation:
Ability = 4.817 + 4.199(0) + 1.871(1)
Ability = 4.817 + 1.871
Ability = 6.688

ANCOVA as a linear model
#OK, now to build our ANCOVA using the lm() function, we simply add the covariate (Gaming) to our model specification.
model_ancova <- lm(Ability ~ Gaming + Condition, data = my_data)
model_ancova

#We can work out the mean of our reference group (Water) by plugging in the values to our equation - note that Gaming is not a factor and we need to enter the mean of this variable.
#We can work it out with the following.
mean(my_data$Gaming)

#We add this mean (12.62296) to our equation alongside the co-efficients for each of our predictors. With our dummy coding scheme, we can work out the adjusted mean of our Water group.
Ability = Intercept + β1(Gaming) + β2(Double Espresso) + β3(Single Espresso)
Ability = -3.4498 + 0.8538(12.62296) + (- 1.0085)(0) + (-0.4563)(0)
Ability = -3.4498 + 10.777
Ability = 7.33
7.33 is the adjusted mean for the Water group, which is what we had from calling the emmeans() function following the ANCOVA. Have a go yourselves at working out the adjusted means of the other two Conditions using our dummy coding.

#we could also have scaled and centred our covariate. This standardises the variable (with the mean centred on zero) and removes the need to multiply the linear model coeffficient for the covariate by the covariate’s mean. Generally, it makes interpretation of the coefficients in our linear model easier.
#we can use the scale() function to create a new (scaled and centred) version of our covariate in our data frame.
my_scaled_data <- my_data %>%
  mutate(centred_gaming = scale(Gaming))

#we can look at both the uncentred and the centred covariate to see that nothing has changed in the data, other than the variable mean is now centred on zero and the distribution has been scaled.
plot(density(my_scaled_data$Gaming))
plot(density(my_scaled_data$centred_gaming))

#so let’s build our linear model with the scaled and centred covariate.
model_ancova_centred <- lm(Ability ~ centred_gaming + Condition, data = my_scaled_data)
model_ancova_centred
#We see that the Intercept now corresponds to the adjusted mean for the Water group. We can calculate the adjusted mean for the Double Espresso group by subtracting 1.0085 from 7.3280, and we can calculate the adjusted mean for the Single Espresso group by subtracting 0.4563 from 7.3280. Hopefully you see that scaling and centering the covariate makes it’s a lot easier to then interpret the coefficients of our linear model.
