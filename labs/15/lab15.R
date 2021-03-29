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

# # Lab 15 - PCA
# ## Lecture 15

pokemon = read.csv('data/poke/pokemon.csv')

head(pokemon)

colnames(pokemon)

head(pokemon$name)

dim(pokemon)

which_numeric = sapply(1:ncol(pokemon),function(i)is.numeric(pokemon[,i])&&all(is.finite(pokemon[,i])))

head(which_numeric)

sum(which_numeric)

poke = pokemon[,which_numeric]

head(poke)

# ## Method 1: prcomp in MASS

library('MASS')

?prcomp

pc.out = prcomp(x=poke,center=TRUE)

pc.out

W = pc.out$rotation

dim(W)

plot(W[,1])

which.max(W[,1])

poke$base_happiness

pc.out = prcomp(x=poke,center=TRUE,scale=TRUE)

W = pc.out$rotation

plot(W[,1])

boxplot(poke)

which.max(colMeans(poke))

vars = pc.out$sdev^2

plot(vars/sum(vars))

plot(cumsum(vars/sum(vars)))

X = scale(poke,center=TRUE,scale=TRUE)

Z = as.matrix(X) %*% W[,1:10]

head(Z)

summary(pc.out)

plot(Z[,1:2])

plot(Z[,2:3])

# ## Method 2: SVD

svdx = svd(X)

W2 = svdx$v[,1:10]

Z2 = X%*%W2

max(Z-Z2)

plot(Z[,1:2])

vars = svdx$d^2

plot(vars/nrow(X))


