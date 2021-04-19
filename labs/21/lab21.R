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

# # Lab 21 - Boosting
# # Lecture 21

# lets work with spambase again

library('bayesreg')
data(spambase)
colnames(spambase)[50:55] <- paste0('char.freq.',c('semic','paren','bracket','exclaim','dollar','pound'))

library('gbm')

?gbm

dim(spambase)

train_ss = sample(1:nrow(spambase),floor(nrow(spambase)/2))

head(train_ss)

train = spambase[train_ss,]
validate = spambase[-train_ss,]

dim(train)
dim(validate)

mod = gbm(is.spam~.,data=train,distribution="bernoulli",n.trees=100,
          shrinkage=0.1,interaction.depth=1,verbose=TRUE)

summary(mod)

class(head(spambase$is.spam))

preds = (predict(mod,n.tree=100,type="response")>.5)*1

table(preds,train$is.spam)

nseq = 1:100
pred_mtx = (predict(mod,n.tree=nseq,type="response")>.5)*1
head(pred_mtx)
dim(pred_mtx)

err = sapply(nseq,function(i)mean(pred_mtx[,i]!=train$is.spam))

head(err)

plot(nseq,err,type='b')

# + jupyter={"outputs_hidden": true}
nseq = 1:10000
mod = gbm(is.spam~.,data=train,distribution="bernoulli",n.trees=max(nseq),
          shrinkage=0.1,interaction.depth=1)
train_pred_mtx = (predict(mod,n.tree=nseq,type="response")>.5)*1
train_err = sapply(nseq,function(i)mean(train_pred_mtx[,i]!=train$is.spam))
test_pred_mtx = (predict(mod,validate,n.tree=nseq,type="response")>.5)*1
test_err = sapply(nseq,function(i)mean(test_pred_mtx[,i]!=validate$is.spam))
# -

plot(nseq,train_err,type='b')           
abline(v=which.min(train_err))
points(nseq,test_err,type='b',col='red')
abline(v=which.min(test_err),col='red')

nseq = 1:1000
mod = gbm(is.spam~.,data=train,distribution="bernoulli",n.trees=max(nseq),
          shrinkage=.5,interaction.depth=5)
train_pred_mtx = (predict(mod,n.tree=nseq,type="response")>.5)*1
train_err = sapply(nseq,function(i)mean(train_pred_mtx[,i]!=train$is.spam))
test_pred_mtx = (predict(mod,validate,n.tree=nseq,type="response")>.5)*1
test_err = sapply(nseq,function(i)mean(test_pred_mtx[,i]!=validate$is.spam))
plot(nseq,train_err,type='b')           
abline(v=which.min(train_err))
points(nseq,test_err,type='b',col='red')
abline(v=which.min(test_err),col='red')


