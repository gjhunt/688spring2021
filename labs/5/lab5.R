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
#     display_name: ''
#     name: ''
# ---

# # Lab 5

# ## lecture 5 

library('MASS')
data(Boston)

# we can build a couple of models

mod1 = lm(medv~crim,data=Boston)
mod2 = lm(medv~crim+rm,data=Boston)
mod3 = lm(medv~crim+I(crim^2)+rm,data=Boston)

summary(mod1)$r.squared

summary(mod2)$r.squared

summary(mod3)$r.squared

# We should be carebul because we are always going to increase the $R^2$ if I add covariates. (This happens for other metrics too.) This is bad way of choosing which coviariates to include. 

# # KNN

library('FNN')
?knn.reg

x = array(sort(runif(100,-1,1)),c(100,1))
head(x)

y = x^2 + rnorm(100,0,1/5)

plot(x,y)

# let's split the data into testing and training

train_ss = sample(nrow(x),floor(nrow(x)/2))
test_ss = setdiff(1:nrow(x),train_ss)

# +
trainx = x[train_ss,,drop=FALSE]
testx = x[test_ss,,drop=FALSE]
trainy = y[train_ss,,drop=FALSE]
testy = y[test_ss,,drop=FALSE]

evaluate = function(pred_x,pred_y,K=10){
    knn_mod = knn.reg(train=trainx,test=pred_x,y=trainy,k=K)
    RSS = sum((knn_mod$pred - pred_y)^2)
    return(RSS)
}
# -

evaluate(testx,testy)

evaluate(trainx,trainy)

# How should we choose $K$?

K_seq = seq(1,46,2)
K_seq

train_RSSs = sapply(K_seq,function(K)evaluate(trainx,trainy,K=K))

plot(K_seq,train_RSSs)

evaluate(trainx,trainy,1)

evaluate(testx,testy,1)

test_RSSs = sapply(K_seq,function(K)evaluate(testx,testy,K=K))
plot(K_seq,test_RSSs)

K_hat = K_seq[which.min(test_RSSs)]
K_hat

best_knn = knn.reg(train=trainx,test=testx,y=trainy,k=K_hat)

plot(testx,testy)
lines(testx,best_knn$pred,col='red')
