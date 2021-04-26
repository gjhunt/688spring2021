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
#     display_name: ''
#     name: ''
# ---

# # Lab 23 - KPCA
# ## Lecture 23

library('kernlab')

?kpca

polar0 = data.frame(r=1+runif(100,.1),theta=seq(0,2*pi,length.out=100)+runif(100,.1))
polar1 = data.frame(r=5+runif(100,.1),theta=seq(0,2*pi,length.out=100)+runif(100,.1))
polar = rbind(polar0,polar1)
cart = data.frame(x=polar$r*cos(polar$theta),y=polar$r*sin(polar$theta))
plot(cart)

X = as.matrix(cart)
plot(svd(X)$u)

kpca.out = kpca(X,kernel='rbfdot',kpar=list(sigma=.2),features=2)

plot(kpca.out@rotated)

kpca.out = kpca(X,kernel='rbfdot',kpar=list(sigma=.1),features=3)
plot(kpca.out@rotated[,2:3])


