# ---
# jupyter:
#   jupytext:
#     formats: ipynb,Rmd,r
#     text_representation:
#       extension: .r
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.9.1
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# # Lab 11 - Logistic Regression
# ## Lecture 11

dset = iris[,c('Species','Sepal.Length','Sepal.Width')]
dset = dset[dset$Species %in% c("virginica","versicolor"),]

table(dset$Species)

levels(dset$Species) = c(NA,"versicolor","virginica")
dset$Species = relevel(dset$Species,ref="versicolor")
table(dset$Species)

head(dset$Species)
head(as.numeric(dset$Species))

?glm

log_mod = glm(formula=Species~.,data=dset,family="binomial")
summary(log_mod)

# $p = logistic(X\beta) \leftrightarrow logit(p) = log(p/(1-p)) = X\beta$

head(predict(log_mod))## predicts the log-odds = logit(p)

X_design = model.matrix(log_mod)
head(X_design)

beta_hat = log_mod$coefficients
beta_hat = array(beta_hat,c(3,1))
beta_hat

head(X_design%*%beta_hat)

p_hats = predict(log_mod,type="response")
head(p_hats)

head(predict(log_mod))
head(log(p_hats/(1-p_hats)))

train_pred = as.numeric(p_hats > .5)
head(train_pred)

train_pred = factor(train_pred,labels=c('versicolor','virginica'))
head(train_pred)

library('caret')
confusionMatrix(data=train_pred,reference=dset$Species)

plot_mod = function(mod){
    x1r = range(dset$Sepal.Length)
    x2r = range(dset$Sepal.Width)
    x1s = seq(x1r[1],x1r[2],.01)
    x2s = seq(x2r[1],x2r[2],.01)
    grd = expand.grid(x1s,x2s)
    colnames(grd) = c('Sepal.Length','Sepal.Width')
    p_hats = predict(mod,newdata=grd,type='response')
    train_pred = (p_hats>.5)*1
    grd_pred = factor(train_pred,labels=c('versicolor','virginica'))
    grd_df = cbind(grd,grd_pred)
    colnames(grd_df) = c('Sepal.Length','Sepal.Width','Pred')
    ggplot(data=grd_df,mapping=aes(x=Sepal.Length,y=Sepal.Width,fill=Pred))+geom_tile()
}

plot_mod(log_mod)+geom_point(data=dset,mapping=aes(x=Sepal.Length,y=Sepal.Width,shape=Species),inherit.aes=FALSE)

library('nnet')

dset2 = iris[,c("Sepal.Length","Sepal.Width","Species")]
table(dset2$Species)

?multinom

mod2 = multinom(Species~.,data=dset2)
summary(mod2)
predict(mod2)

predict(mod2,type="probs")

apply(predict(mod2,type="probs"),1,which.max)

plot_mod = function(mod){
    x1r = range(dset2$Sepal.Length)
    x2r = range(dset2$Sepal.Width)
    x1s = seq(x1r[1],x1r[2],.05)
    x2s = seq(x2r[1],x2r[2],.05)
    grd = expand.grid(x1s,x2s)
    colnames(grd) = c('Sepal.Length','Sepal.Width')
    grd_pred = predict(mod,newdata=grd,type='class')
    grd_df = cbind(grd,grd_pred)
    colnames(grd_df) = c('Sepal.Length','Sepal.Width','Pred')
    ggplot(data=grd_df,mapping=aes(x=Sepal.Length,y=Sepal.Width,fill=Pred))+geom_tile()
}

plot_mod(mod2)+geom_point(data=dset2,mapping=aes(x=Sepal.Length,y=Sepal.Width,shape=Species),inherit.aes=FALSE)

mod3 = multinom(Species~I(Sepal.Width^2)+Sepal.Width*Sepal.Length,data=dset2)
plot_mod(mod3)
