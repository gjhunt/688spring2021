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

# # Lab 3 - More Regression
# ## lecture 3

library('MASS')
data(Boston)

?Boston

mod = lm(medv~crim,data=Boston)
summary(mod)

y_hat = predict(mod)
plot(Boston$crim,y_hat)

plot(Boston$crim,Boston$medv)
abline(coef=coef(mod),col='red')

# # covariate transformations

plot(log(Boston$crim),log(Boston$medv))

# ## variable transformations

mod2 = lm(log(medv)~log(crim),data=Boston)

summary(mod2)

plot(log(Boston$crim),log(Boston$medv))
abline(coef=coef(mod2),col='red')

plot(log(Boston$crim),log(Boston$medv))

logmedv = array(log(Boston$medv),c(506,1))
transf_crim = log(Boston$crim)

mod3 = lm(logmedv~transf_crim)
summary(mod3)

X = model.matrix(mod3)
head(X)

head(log(Boston$crim))

beta_hat = ginv(t(X)%*%X)%*%t(X)%*%logmedv

coef(mod3)

# # categorical variables

data(birthwt)

?birthwt

head(birthwt)

head(birthwt$race)

racef = as.factor(birthwt$race)
head(racef)

levels(racef) = c("White","Black","Other")

head(racef)

birthwt$race = racef
mod = lm(bwt~race,data=birthwt)
summary(mod)

head(model.matrix(mod))


