# ---
# jupyter:
#   jupytext:
#     formats: ipynb,Rmd,R
#     main_language: python
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.9.1
#   kernelspec:
#     display_name: ''
#     name: ''
# ---

# # Lab - KNN Classification
# ## Lecture 8 

data(iris)

iris

summary(iris)

library('class')

# ?knn

dset = iris[,c('Species','Sepal.Length','Sepal.Width')]
head(dset)

set.seed(12093)
randomized = sample(1:nrow(dset))
randomized

train_ss = randomized[1:floor(nrow(dset)/3)]
test_ss = setdiff(1:nrow(dset),train_ss)
head(train_ss)
head(test_ss)

trainx = dset[train_ss,-1]
trainy = dset[train_ss,1]
testx = dset[test_ss,-1]
testy = dset[test_ss,1]

# ### let's build our knn model!

pred_y = knn(train=trainx,testx,cl=trainy,k=5)
head(pred_y)

table(pred_y)

table(testy)

acc = mean(testy==pred_y)
acc

# # Evaluation metrics

# + jupyter={"outputs_hidden": true}
library('caret')
# ?confusionMatrix
# -

head(pred)

cmtx = confusionMatrix(data=pred_y,reference=testy)

names(cmtx)

cmtx$table

cmtx

#overall accuracy
sum(diag(cmtx$table))/sum(cmtx$table)

# per class accuracy 
round(diag(cmtx$table)/colSums(cmtx$table),5)

cmtx$byClass

cmtx$overall

cmtx$overall["Accuracy"]

# # Visualization

x1r = range(iris$Sepal.Length)
x2r = range(iris$Sepal.Width)
x1s = seq(x1r[1],x1r[2],.1)
x2s = seq(x2r[1],x2r[2],.1)
grd = expand.grid(x1s,x2s)
head(grd)

library('ggplot2')
ggplot(data=grd,mapping=aes(x=Var1,y=Var2))+geom_point()

pred_y = knn(train=trainx,test=grd,cl=trainy,k=5)
grd_pred = cbind(grd,pred_y)
colnames(grd_pred) <- c('Sepal.Length','Sepal.Width','Pred')
head(grd_pred)

ggplot(data=grd_pred,mapping=aes(x=Sepal.Length,y=Sepal.Width,fill=Pred))+geom_tile()

train_df = data.frame(Sepal.Length=trainx$Sepal.Length,
                      Sepal.Width=trainx$Sepal.Width,
                      y=trainy
)
head(train_df)

test_df = data.frame(Sepal.Length=testx$Sepal.Length,
                      Sepal.Width=testx$Sepal.Width,
                      y=testy
)
head(test_df)

ggplot(data=grd_pred,mapping=aes(x=Sepal.Length,y=Sepal.Width,fill=Pred))+geom_tile()+
geom_point(data=train_df,mapping=aes(x=Sepal.Length,y=Sepal.Width,shape=y),inherit.aes=FALSE)

ggplot(data=grd_pred,mapping=aes(x=Sepal.Length,y=Sepal.Width,fill=Pred))+geom_tile()+
geom_point(data=train_df,mapping=aes(x=Sepal.Length,y=Sepal.Width,shape=y),inherit.aes=FALSE)+
geom_point(data=test_df,mapping=aes(x=Sepal.Length,y=Sepal.Width,fill=y),inherit.aes=FALSE,
pch=21)

plot_K = function(K){
    pred_y = knn(train=trainx,test=grd,cl=trainy,k=K)
    grd_pred = cbind(grd,pred_y)
    colnames(grd_pred) <- c('Sepal.Length','Sepal.Width','Pred')

    ggplot(data=grd_pred,mapping=aes(x=Sepal.Length,y=Sepal.Width,fill=Pred))+geom_tile()+
    geom_point(data=train_df,mapping=aes(x=Sepal.Length,y=Sepal.Width,shape=y),inherit.aes=FALSE)+
    geom_point(data=test_df,mapping=aes(x=Sepal.Length,y=Sepal.Width,fill=y),inherit.aes=FALSE,
    pch=21)
}

plot_K(1)

plot_K(15)

plot_K(50)

plot_K(45)


