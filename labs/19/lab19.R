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

# # Lab 19 - Decision Trees
# ## Lecture 19

install.packages('ISLR')

library('ISLR')
data(Carseats)

head(Carseats)

library('rpart')

?rpart

set.seed(128321093)
ss_train = sample(1:nrow(Carseats),floor(nrow(Carseats)/2))
dim(Carseats)
length(ss_train)

train = Carseats[ss_train,]
validation = Carseats[-ss_train,]

dim(train)
dim(validation)

model = rpart(Sales~.,data=train,method="anova",
    control = rpart.control(maxdepth=30,minsplit=2,xval=10))

plot(model)
text(model)

ct = model$cptable
ct

plot(log(ct[,"CP"]),ct[,"xerror"])

opt_alpha = ct[which.min(ct[,"xerror"]),"CP"]
opt_alpha

plot(log(ct[,"CP"]),ct[,"xerror"])
abline(v=log(opt_alpha))

pruned = prune(model,cp=opt_alpha)

pruned

plot(pruned)
text(pruned)

RSS_train_full = sum((predict(model,train)-train$Sales)^2)
RSS_train_pruned = sum((predict(pruned,train)-train$Sales)^2)
RSS_train_full
RSS_train_pruned

RSS_val_full = sum((predict(model,validation)-train$Sales)^2)
RSS_val_pruned = sum((predict(pruned,validation)-train$Sales)^2)
RSS_val_full
RSS_val_pruned

# ### Classification

library('MASS')

head(iris)

classmod = rpart(Species~.,data=iris,method="class")

plot(classmod)
text(classmod)


