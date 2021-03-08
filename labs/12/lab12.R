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

# # Lab 12 - Variable Selection
# ## Lecture 

library('MASS')
library('leaps')

data(mtcars)

head(mtcars)

?mtcars

?regsubsets

dim(mtcars)

ss_mods = regsubsets(mpg~.,data=mtcars,method="forward",nvmax=10)

summary(ss_mods)

ss_mods = regsubsets(mpg~.,data=mtcars,method="backward",nvmax=10)

summary(ss_mods)

ss_mods = regsubsets(mpg~.,data=mtcars,method="exhaustive",nvmax=10)

summary(ss_mods)

smry = summary(ss_mods)

names(smry)

smry$which

smry$rsq

plot(smry$rsq)

plot(smry$rss)

smry$adjr2

plot(smry$adjr2)

ii = which.max(smry$adjr2)
ii

best_vars = colnames(smry$which)[smry$which[ii,]][-1]
best_vars

fmla = paste0("mpg~",paste(best_vars,collapse="+"))
fmla

best_mod = lm(as.formula(fmla),data=mtcars)

plot(mtcars$mpg,predict(best_mod))
abline(coef=c(0,1))

# ### Ted Talks Datset

ted = readRDS('data/ted/ted_talks.rds')

dim(ted)

head(ted)

head(colnames(ted))

hist(ted$views)

hist(log(ted$views))

tdm = ted[,grep("word",colnames(ted),value=TRUE)]
df = data.frame(views=ted$views,tdm)
df = head(df,n=100)

head(df)

dim(df)

lm(views~.,data=df)

?kappa

X = model.matrix(lm(views~.,data=df))
dim(X)

xtx = t(X)%*%X
dim(xtx)

kappa(xtx)

solve(xtx)

ted_mods = regsubsets(log(views)~.,data=df,method="forward",nvmax=100)

ar2 = summary(ted_mods)$adjr2

plot(ar2,type='l',ylab='r2_adj',xlab='num vars')

which.max(ar2)

fmla = paste0(colnames(df)[summary(ted_mods)$which[43,]][-1],collapse="+")
fmla = paste0("log(views)~",fmla)
fmla

best_mod = lm(fmla,data=df)

plot(log(df$views),predict(best_mod))
abline(coef=c(0,1))
