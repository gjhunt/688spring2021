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

# # Lab 14 - LASSO
# ## Lecture 14

ted = readRDS('data/ted/ted_talks.rds')
tdm = ted[,grep("word",colnames(ted),value=TRUE)]
df = data.frame(views=ted$views,tdm)

library('glmnet')

# +
#install.packages('glmnetUtils')
# -

library('glmnetUtils')

# + jupyter={"outputs_hidden": true}
?glmnet
# -

lasso.mod = glmnet(log(views)~.,family="gaussian",alpha=1,data=df)

length(lasso.mod$lambda)

# + jupyter={"outputs_hidden": true}
lasso.mod$beta

# + jupyter={"outputs_hidden": true}
as.matrix(lasso.mod$beta)
# -

matplot(log(lasso.mod$lambda),t(as.matrix(lasso.mod$beta)),type='l',xlab="log(lam)",ylab="beta")

ridge.mod = glmnet(log(views)~.,family="gaussian",alpha=0,data=df)

matplot(log(ridge.mod$lambda),t(as.matrix(ridge.mod$beta)),type='l',xlab="log(lam)",ylab="beta")

# + jupyter={"outputs_hidden": true}
lamb_seq=10^seq(-5,0,length.out=100)
lamb_seq
# -

lasso.cv = cv.glmnet(log(views)~.,family="gaussian",alpha=1,data=df,lambda=lamb_seq)

plot(lasso.cv)

best_lam = lasso.cv$lambda.min
best_lam

best.lasso = glmnet(log(views)~.,family="gaussian",alpha=1,data=df,lambda=best_lam)

dim(best.lasso$beta)

# + jupyter={"outputs_hidden": true}
best.lasso$beta
# -

sum(best.lasso$beta!=0)

plot(log(df$views),predict(best.lasso,newdata=df))

# # elastic net

en.mod = glmnet(log(views)~.,family="gaussian",alpha=.5,data=df)

matplot(log(en.mod$lambda),t(as.matrix(en.mod$beta)),type='l',xlab="log(lam)",ylab="beta")

en.mod$beta


