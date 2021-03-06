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

# # Lab 9 - LDA
# ## Lecture 9

library('ggplot2')
library('reshape2')

g1 <- data.frame(group=1,x = rnorm(100,1,1))
g2 <- data.frame(group=2,x = rnorm(100,5,1))
g3 <- data.frame(group=3,x = rnorm(100,20,1))
d = rbind(g1,g2,g3)
d$group <- as.factor(d$group)

head(d[sample(nrow(d)),])

ggplot(data=d,mapping=aes(x=x,color=group,group=group))+geom_density()+geom_point(mapping=aes(x=x,y=0))

# +
x0=1
class=1
  
subd = d[d$group==class,]
muhat = mean(subd$x)
# pooled sd estimate
sighat = sqrt(mean(aggregate(x~group,data=d,FUN=var)[,2]))
pi_hat = mean(d$group==class)
dnorm(x0,mean=muhat,sd=sighat)*pi_hat
# -

delta_lda=function(x0,class){
  subd = d[d$group==class,]
  muhat = mean(subd$x)
  sighat = sqrt(mean(aggregate(x~group,data=d,FUN=var)[,2]))
  pi_hat = mean(d$group==class)
  dnorm(x0,mean=muhat,sd=sighat)*pi_hat
}
y_hat = which.max(c(delta_lda(7,1),
    delta_lda(7,2),
    delta_lda(7,3)))
y_hat

delta_lda2=function(x0,class){
  subd = d[d$group==class,]
  muhat = mean(subd$x)
  sighat = sqrt(mean(aggregate(x~group,data=d,FUN=var)[,2]))
  pi_hat = mean(d$group==class)
  muhat*x0/(sighat^2) - muhat^2/(2*sighat^2)+log(pi_hat)
}
y_hat = which.max(c(delta_lda2(7,1),
    delta_lda2(7,2),
    delta_lda2(7,3)))
y_hat

lda_pred <- data.frame(
  '1'=delta_lda2(d$x,1),
  '2'=delta_lda2(d$x,2),
  '3'=delta_lda2(d$x,3)
)
lda_preds = apply(lda_pred,1,which.max)
sample(lda_preds,5)

library('MASS')
?lda

mod = lda(group~x,data=d)
mod

mod_pred = predict(mod,d)$class
sample(mod_pred,5)

# ### same answer as our own version

mean(as.integer(mod_pred)!=lda_preds)

# +
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

ggplot(data=d,mapping=aes(x=x,color=group,group=group))+geom_density()+geom_point(mapping=aes(x=x,y=0))
# -

# ### LDA discr functions

ests_lda = ests(delta_lda)
ggplot(data=ests_lda$pfn,mapping=aes(x=x,y=value,color=group))+geom_line(size=2)+geom_point(data=ests_lda$preds,mapping=aes(x=x,y=0))+
  geom_density(data=d,mapping=aes(x=x,color=group,group=group),inherit.aes=FALSE)+ylim(0,.5)

# ### discr funcs for second version

ests_lda2 = ests(delta_lda2)
ggplot(data=ests_lda2$pfn,mapping=aes(x=x,y=value/100,color=group))+geom_line(size=2)+geom_point(data=ests_lda2$preds,mapping=aes(x=x,y=0))+
  geom_density(data=d,mapping=aes(x=x,color=group,group=group),inherit.aes=FALSE)+ylim(0,.5)
