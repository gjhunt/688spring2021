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

# # Lab 16
# ## Lecture 16

d = read.csv('data/tcga/smaller.csv',row.names=1)

dim(d)

head(d)

d = as.matrix(d)

w0 = which(apply(d,2,sd)==0)

d = d[,-w0]
dim(d)

y = d[,5]
X = d[,-5]

sigma = apply(X,2,sd)
mus = colMeans(X)
X = scale(X,scale=TRUE,center=TRUE)
head(X)

sum(!is.finite(X))

lm(y~X)

library('pls')

?pcr

pcrmod = pcr(y~X,ncomp=10)

summary(pcrmod)

pcr_preds = predict(pcrmod,ncomp=10)

V = svd(X,nv=10)$v

XV = X%*%V

dim(XV)

hm = lm(y~XV)

summary(hm)

head(coef(hm))

hm_preds = predict(hm)

head(hm_preds)

plot(pcr_preds,hm_preds)

beta_pcr = V%*%array(coef(hm)[-1],c(10,1))

head(beta_pcr)

plot(predict(pcrmod,ncomp=10),mean(y)+X%*%beta_pcr)

newx = rnorm(ncol(X))
newx = data.frame(t(newx))
colnames(newx) = colnames(X)
head(newx)

predict(pcrmod,as.matrix(newx),ncomp=10)

as.numeric(mean(y)+as.matrix(newx)%*%beta_pcr)
