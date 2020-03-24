#install.packages('mcmcse', repos='http://cran.us.r-project.org')

#Set random seed
set.seed(1024)
library(mcmcse)

#store 1e5 samples from Normal Distribution (0,1)
foo <- rnorm(1e5)
#calculate sample size using ess function
ess(foo)

#Sample a random matrix
rmatrix <- matrix(rnorm(100), ncol = 10)
#Print eigen values
eigen(rmatrix)$values