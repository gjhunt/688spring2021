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

# # Lab 4 - KNN Regression and Evaluation
# ## lecture 4

install.packages('FNN')

# # simulation

?runif

x = runif(100,-1,1)
hist(x)

y = x^2 # f(x) = x^2
plot(x,y)

e = rnorm(100,0,1/10)
hist(e)

y = x^2 + e
plot(x,y)

# # using linear regression

mod = lm(y~x) # y = b0 + b1*x
summary(mod)

plot(x,y)
abline(coef(mod),col='red')
lines(x[x_ord],x[x_ord]^2,col='blue')
dim(model.matrix(mod))

# # KNN

library('FNN')
?knn.reg

X = array(x,c(100,1))
Y = array(y,c(100,1))
knn_mod = knn.reg(train=X,y=Y,test=X,k=10)

names(knn_mod)

length(knn_mod$pred) # these are the predictions

plot(x,y)
x_ord  = order(x)
lines(x[x_ord],knn_mod$pred[x_ord],col='red')
lines(x[x_ord],x[x_ord]^2,col='blue')

# ## k = 3

knn_mod = knn.reg(train=X,y=Y,test=X,k=3)
plot(x,y)
lines(x[x_ord],knn_mod$pred[x_ord],col='red')
lines(x[x_ord],x[x_ord]^2,col='blue')

# # k = 1

knn_mod = knn.reg(train=X,y=Y,test=X,k=1)
plot(x,y)
lines(x[x_ord],knn_mod$pred[x_ord],col='red')
lines(x[x_ord],x[x_ord]^2,col='blue')

# # k = 100

knn_mod = knn.reg(train=X,y=Y,test=X,k=100)
plot(x,y)
lines(x[x_ord],knn_mod$pred[x_ord],col='red')
lines(x[x_ord],x[x_ord]^2,col='blue')

mean(y)

head(knn_mod$pred)

# # k = 25

knn_mod = knn.reg(train=X,y=Y,test=X,k=25)
plot(x,y)
lines(x[x_ord],knn_mod$pred[x_ord],col='red')
lines(x[x_ord],x[x_ord]^2,col='blue')

# # Can do we do this with a linear model?

lin_mod = lm(y~I(x^2)) # y = b0+b1x^2
plot(x,y)
lines(x[x_ord],predict(lin_mod)[x_ord],col='red')
lines(x[x_ord],x[x_ord]^2,col='blue')

lin_mod = lm(y~x+I(x^4)) # y = b0+b1x^2
plot(x,y)
lines(x[x_ord],predict(lin_mod)[x_ord],col='red')
lines(x[x_ord],x[x_ord]^2,col='blue')
dim(model.matrix(lin_mod))
