# ---
# jupyter:
#   jupytext:
#     formats: ipynb,Rmd,R
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.9.1
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# # Lab 2 - Linear Regression
# ## Lecture 2

# +
#install.packages('MASS')
# -

library('MASS')

data(Boston)

dim(Boston)

Boston[1:5,]

?Boston

# let's fit a regression to predict the median house value from the crime rate 

plot(log(Boston$crim),log(Boston$medv))

# `medv~crim` basically says `medv = beta0 + beta1*crim`

mod = lm(medv~crim,data=Boston)
mod

summary(mod)

mod$coef

plot(Boston$crim,Boston$medv)
abline(coef=mod$coef)

X = array(Boston$crim,c(506,1))
X = cbind(1,X)
X

y = array(Boston$medv,c(506,1))

beta_hat = ginv(t(X)%*%X)%*%t(X)%*%y

beta_hat

y_hat_mod = predict(mod)
head(y_hat_mod)

y_hat = X%*%beta_hat

head(y_hat)

plot(Boston$medv,y_hat)


