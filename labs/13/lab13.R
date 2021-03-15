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

# # Lab 13 - Ridge Regression
# # Lecture 13

ted = readRDS('data/ted/ted_talks.rds')

dim(ted)

head(ted)

tdm = ted[,grep("word",colnames(ted),value=TRUE)]

head(colnames(tdm))

df = data.frame(views=ted$views,tdm)
head(df)

mod = lm(log(views)~.,data=df)
beta_hat = mod$coef

sum(!is.finite(beta_hat))

qr(tdm)$rank

dim(tdm)

tail(svd(tdm)$d)

X = as.matrix(tdm)
xtx=t(X)%*%X
kappa(xtx)

max(eigen(xtx)$values)/abs(min(eigen(xtx)$values))

# # Ridge Regression Solution!

install.packages('glmnet')

library('glmnet')

# + jupyter={"outputs_hidden": true}
?glmnet
# -

dim(X)

y = ted$views

fit.ridge = glmnet(x=X,y=log(y),family="gaussian",alpha=0)

# + jupyter={"outputs_hidden": true}
fit.ridge
# -

dim(fit.ridge$beta)

length(fit.ridge$lambda)

dim(X)

matplot(log(fit.ridge$lambda),t(fit.ridge$beta),type='l',xlab="log(lam)",ylab="beta")

lam_seq = 10^seq(-10,10,length.out=100)
head(lam_seq)

fit.ridge = glmnet(x=X,y=log(y),family="gaussian",alpha=0,lambda=lam_seq)

matplot(log(fit.ridge$lambda),t(fit.ridge$beta),type='l',xlab="log(lam)",ylab="beta")

cv.ridge = cv.glmnet(x=X,y=log(y),family="gaussian",alpha=0,lambda=lam_seq)

plot(cv.ridge)

cv.ridge$lambda.min

best.ridge = glmnet(x=X,y=log(y),family="gaussian",alpha=0,lambda=cv.ridge$lambda.min)
