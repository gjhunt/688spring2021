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

# # Lab 10 - LDA and QDA
# ## Lecture 10

library('ggplot2')
library('reshape2')

g1 <- data.frame(group=1,x = rnorm(100,1,1))
g2 <- data.frame(group=2,x = rnorm(100,5,1))
g3 <- data.frame(group=3,x = rnorm(100,20,1))
d = rbind(g1,g2,g3)
d$group <- as.factor(d$group)

head(d[sample(nrow(d)),])

ggplot(data=d,mapping=aes(x=x,color=group,group=group))+geom_density()+geom_point(mapping=aes(x=x,y=0))

delta_lda2=function(x0,class){
  subd = d[d$group==class,]
  muhat = mean(subd$x)
  sighat = sqrt(mean(aggregate(x~group,data=d,FUN=var)[,2]))
  pi_hat = mean(d$group==class)
  muhat*x0/(sighat^2) - muhat^2/(2*sighat^2)+log(pi_hat)
}

lda_pred <- data.frame(
  '1'=delta_lda2(d$x,1),
  '2'=delta_lda2(d$x,2),
  '3'=delta_lda2(d$x,3)
)
lda_preds = apply(lda_pred,1,which.max)
sample(lda_preds,5)

ests = function(predfn){
  xs = seq(min(d$x),max(d$x),.01)
  lda_pred <- data.frame(
    '1'=predfn(xs,1),
    '2'=predfn(xs,2),
    '3'=predfn(xs,3)
  )
  mlda_pred = melt(cbind(lda_pred,xs),id.vars='xs')
  colnames(mlda_pred)<-c('x','group','value')
  levels(mlda_pred$group) <- 1:3
  lda_preds = data.frame('x'=xs,apply(lda_pred,1,which.max))
  colnames(lda_preds) <- c('x','group')
  lda_preds$group <- as.factor(lda_preds$group)
  return(list(pfn=mlda_pred,preds=lda_preds))
}

ests_lda2 = ests(delta_lda2)
ggplot(data=ests_lda2$pfn,mapping=aes(x=x,y=value/100,color=group))+geom_line(size=2)+geom_point(data=ests_lda2$preds,mapping=aes(x=x,y=0))+
  geom_density(data=d,mapping=aes(x=x,color=group,group=group),inherit.aes=FALSE)+ylim(-.5,.5)

# +
delta_lm=function(x0,class){
  d1 = d
  d1$ngroup=as.numeric(d1$group)
  d1$ngroup[d1$ngroup!=class] <- 0
  mod1 = lm(ngroup~x,data=d1)
  ggplot(data=d1,mapping=aes(x=x,y=ngroup,color=as.factor(group)))+geom_point()+
    geom_abline(intercept=coef(mod1)[1],slope=coef(mod1)[2])
  return(coef(mod1)[1] + coef(mod1)[2]*x0)
}

plot_lm=function(class){
  d1 = d
  d1$ngroup=as.numeric(d1$group)
  d1$ngroup[d1$ngroup!=class] <- 0
  mod1 = lm(ngroup~x,data=d1)
  print(ggplot(data=d1,mapping=aes(x=x,y=ngroup,color=as.factor(group)))+geom_point()+
    geom_abline(intercept=coef(mod1)[1],slope=coef(mod1)[2]))
}

plot_lm(1)
plot_lm(2)
plot_lm(3)

ests_lm = ests(delta_lm)
ggplot(data=ests_lm$pfn,mapping=aes(x=x,y=value,color=group))+geom_line()+geom_point(data=ests_lm$preds,mapping=aes(x=x,y=0))+ylim(0,1)+
  geom_density(data=d,mapping=aes(x=x,color=group,group=group),inherit.aes=FALSE)
# -

# # LDA for p > 1 

data(iris)
head(iris)

summary(iris)

library('MASS')

?lda

?qda

dset = iris[,c('Species','Sepal.Length','Sepal.Width')]
head(dset)

lda_mod = lda(Species~.,data=dset)

lda_mod

x1r = range(dset$Sepal.Length)
x2r = range(dset$Sepal.Width)
x1s = seq(x1r[1],x1r[2],.1)
x2s = seq(x2r[1],x2r[2],.1)
grd = expand.grid(x1s,x2s)
colnames(grd) = c('Sepal.Length','Sepal.Width')
ggplot(data=grd,mapping=aes(x=Sepal.Length,y=Sepal.Width))+geom_point()

grd_pred = predict(lda_mod,newdata=grd)
grd_pred$posterior[c(1,123,57),]
grd_pred$class[c(1,123,57)]

grd_df = cbind(grd,grd_pred$class)
colnames(grd_df) = c('Sepal.Length','Sepal.Width','Pred')
head(grd_df)

ggplot(data=grd_df,mapping=aes(x=Sepal.Length,y=Sepal.Width,fill=Pred))+geom_tile()

# +
plot_mod = function(mod){
    x1r = range(dset$Sepal.Length)
    x2r = range(dset$Sepal.Width)
    x1s = seq(x1r[1],x1r[2],.1)
    x2s = seq(x2r[1],x2r[2],.1)
    grd = expand.grid(x1s,x2s)
    colnames(grd) = c('Sepal.Length','Sepal.Width')
    grd_pred = predict(mod,newdata=grd)
    grd_df = cbind(grd,grd_pred$class)
    colnames(grd_df) = c('Sepal.Length','Sepal.Width','Pred')
    ggplot(data=grd_df,mapping=aes(x=Sepal.Length,y=Sepal.Width,fill=Pred))+geom_tile()
}

mod = lda(Species~Sepal.Length*Sepal.Width,data=dset)
plot_mod(mod)
# -

model.matrix(mod)

mod = qda(Species~Sepal.Length+Sepal.Width,data=dset)
plot_mod(mod)

mod = qda(Species~Sepal.Length*Sepal.Width+I(Sepal.Width^2)+I(Sepal.Length^2),data=dset)
plot_mod(mod)
head(model.matrix(mod))
