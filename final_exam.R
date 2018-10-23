# install.package('ISLR')
library(ISLR)
library(car) # Anova()
library(AER) # coeftest()
data("College")

# Is the average graduation rate in private schools larger 
# than that in public schools?
boxplot(Grad.Rate ~ Private, data = College)

# which institution has the largest graduation rate
College[which(College$Grad.Rate == max(College$Grad.Rate)), ]
# which institution has the smallest graduation rate
College[which(College$Grad.Rate == min(College$Grad.Rate)), ]

# build a model with all predictors
mod_full <- lm(Grad.Rate ~ ., data = College)
# build a null model
mod_null <- lm(Grad.Rate ~ 1, data = College)
# use stepwise approach to find the best model
mod_step <- step(mod_full, scope = 
                     list(upper = mod_full, lower = mod_null))
# explain the meaning of coefficients
summary(mod_step)

# add an interaction term between Room.Board and Private
mod_update <- update(mod_step, . ~ . + Room.Board * Private)
# calculate the change of graduation rate when Room.Board is
# increased by 1000 for private and public schools
summary(mod_update)

# build a model using the following predictors 
# using 'probit' as the link function
mod_probit <- glm(Private ~ Room.Board + Apps + Enroll + 
                 Outstate + perc.alumni + Personal+ Books, 
             family = binomial('probit'), data = College)

# using 'probit' as the link function impiles that 
# assuming the error term is normally distributed

# use wald test to see which predictors are significant
coeftest(mod_probit)

# use Deviance Table to see which predictors are significant
Anova(mod_probit)

mod_prob_null <- glm(College$Private ~ 1, family = binomial('probit'))

# use chi square to see the difference between mod_probit and null model
anova(mod_probit, mod_prob_null, test = "Chisq")

# set prediction to 1 if the value is greater than 0.5
# calculate all misclassification rates
table(College$Private, mod_probit$fitted.values > 0.5)

# calculate the predicted probability 
# given the predictors with average values 
d <- data.frame(Room.Board = mean(College$Room.Board),
                Apps = mean(College$Apps),
                Enroll = mean(College$Enroll),
                Outstate = mean(College$Outstate),
                perc.alumni = mean(College$perc.alumni),
                Personal = mean(College$Personal),
                Books = mean(College$Books))
 
predict(mod_probit, newdata = d, type = 'response')

# calculate the average predicted probability 
# given the predictors with average values 
# and Outstate ranging from 3000 to 10000
d_all <- expand.grid(Room.Board = mean(College$Room.Board),
                     Apps = mean(College$Apps),
                     Enroll = mean(College$Enroll),
                     Outstate = 3000:10000,
                     perc.alumni = mean(College$perc.alumni),
                     Personal = mean(College$Personal),
                     Books = mean(College$Books))

mean(predict(mod_probit, newdata = d_all, type = 'response'))
