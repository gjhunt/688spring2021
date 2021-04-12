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

# # Lab 18 - Hierarchical Clustering
# ## Lecture 18 

data = iris
data = data[,-which(colnames(data)=="Species")]

head(data)

?hclust

?dist

D = dist(as.matrix(data))

class(D)

D_mat = as.matrix(D)
dim(D_mat)

D_mat[1:5,1:5]

all(D - t(D) == 0)

hc = hclust(D,method="average")

plot(hc)

hc = hclust(D,method="complete")
plot(hc)

hc = hclust(D,method="single")
plot(hc)

?cutree

hc.cut = cutree(hc,h=.5)

hc.cut

table(hc.cut)

plot(hc);abline(h=.5,col='red')

cutree(hc,h=1)
plot(hc);abline(h=1,col='red')

clusts = cutree(hc,k=3)
clusts

plot(svd(scale(data))$u[,1:2],col=clusts)

hc = hclust(D,method="average")
clusts = cutree(hc,k=3)
plot(svd(scale(data))$u[,1:2],col=clusts)
text(svd(scale(data))$u[,1:2],label=iris$Species,col=clusts)

plot(svd(scale(data))$u[,2:3],col=clusts)
text(svd(scale(data))$u[,2:3],label=iris$Species,col=clusts)


