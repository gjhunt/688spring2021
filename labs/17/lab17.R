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
#     display_name: R
#     language: R
#     name: ir
# ---

# # Lab 17 - K-means clustering
# ## Lecture 17 

data = iris
head(data)

data = data[-which(colnames(data)=="Species")]
head(data)

# ?kmeans

km.out = kmeans(data,centers=3,nstart=1,algorithm="Lloyd")

km.out

km.out$cluster

dim(data)

plot(svd(scale(data))$u[,1:2])

plot(svd(scale(data))$u[,1:2],xlab="PC1",ylab="PC2",col=km.out$cluster)

plot(svd(scale(data))$u[,1:2],xlab="PC1",ylab="PC2",col=km.out$cluster)
text(svd(scale(data))$u[,1:2],col=km.out$cluster,label=iris$Species)

km.out = kmeans(data,centers=2,nstart=1,algorithm="Lloyd")
plot(svd(scale(data))$u[,1:2],xlab="PC1",ylab="PC2",col=km.out$cluster)
text(svd(scale(data))$u[,1:2],col=km.out$cluster,label=iris$Species)

km.out = kmeans(data,centers=5,nstart=10)
plot(svd(scale(data))$u[,1:2],xlab="PC1",ylab="PC2",col=km.out$cluster)
text(svd(scale(data))$u[,1:2],col=km.out$cluster,label=iris$Species)


