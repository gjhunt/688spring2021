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

# We can write notebooks using a couple different IDEs, popular being 
#
# 1. [jupyter](https://jupyter.org/install) which is a bit more advanced and requires installing [anaconda](https://www.anaconda.com/)
#
# 2. [rstudio](https://rstudio.com/products/rstudio/)
#
# either can be used to write code in [R](https://www.r-project.org/)

print('hello world')

# We can use R as a calculator

1+1

1+2*5

# there are also some built-in constants in R

pi
exp(1)

# there are also special infinite values

1/0
-1/0

0/0

# to denote nothing we have

NULL

# to denote a missing value

NA

# # Variable assignment and simple objects

x = 1
y <- 2
3 -> z
x;y;z
print(x)

# # Vectors

x = c(5,3,7)
x

x[1]

x[c(-1,-2)]

# we can make a vector out of anything

truths <- c(TRUE, FALSE, TRUE)

x[truths]

# we can make a matrix using the ``matrix`` function

1:25
X <- matrix(1:25,nrow=5,byrow=TRUE)
X

Y <- array(25:49,c(5,5))
Y

# matrix multiplication is done with the operator ``%*%``

X %*% Y

# # Flow Control

A = FALSE
if(A){
    print("TRUE")
} else{
    print("FALSE")
}

for(i in 1:10){
    print(i)
}

i = 1
while( i <= 10){
    print(i)
    i = i + 1
}

# there is a good tutorial by Dr. Leemis [here](http://www.math.wm.edu/~leemis/r.pdf)
