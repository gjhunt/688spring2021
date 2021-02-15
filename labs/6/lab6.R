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

# # Lab 6 - Evaluation

# ## lecture 6

    library('FNN')

x = array(sort(runif(100,-1,1)),c(100,1))
y = x^2 + rnorm(100,0,1/5)
plot(x,y)

# let's split the data into validation and training

train_ss = sample(nrow(x),floor(nrow(x)/2))
val_ss = setdiff(1:nrow(x),train_ss)

# +
trainx = x[train_ss,,drop=FALSE]
valx = x[val_ss,,drop=FALSE]
trainy = y[train_ss,,drop=FALSE]
valy = y[val_ss,,drop=FALSE]

evaluate = function(pred_x,pred_y,K=10){
    knn_mod = knn.reg(train=trainx,test=pred_x,y=trainy,k=K)
    RSS = sum((knn_mod$pred - pred_y)^2)
    return(RSS)
}
# -

K_seq = seq(1,46,2)

train_RSSs = sapply(K_seq,function(K)evaluate(trainx,trainy,K=K))
val_RSSs = sapply(K_seq,function(K)evaluate(valx,valy,K=K))

plot(K_seq,train_RSSs,col='red')
points(K_seq,val_RSSs)

K_hat = K_seq[which.min(val_RSSs)]
K_hat

min(val_RSSs)

testx = array(sort(runif(50,-1,1)),c(50,1))
testy = testx^2 + rnorm(50,0,1/5)
best_knn = knn.reg(train=trainx,test=testx,y=trainy,k=K_hat)
pred_y_ind = best_knn$pred
sum((pred_y_ind - testy)^2) #RSS

reps = replicate(100,{
    x_ind = array(sort(runif(50,-1,1)),c(100,1))
    y_ind = x_ind^2 + rnorm(50,0,1/5)
    best_knn = knn.reg(train=trainx,test=x_ind,y=trainy,k=K_hat)
pred_y_ind = best_knn$pred
    sum((pred_y_ind - y_ind)^2)
})
hist(reps)


