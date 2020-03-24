#Import library mcmcse and set random seed
library('mcmcse')
set.seed(1024)

#Define AR(1) Function
AR_1 <- function(x_0, t, phi, mu, sigma) {
  X <- numeric(length = t)
  X[1] = x_0
  W <- rnorm(t, mu, sigma)
  
  for (i in 2:t) {
    X[i] = phi*X[i-1] + W[i]
  }
  
  return(X)
}
#AR(1) initial settings
x_0 = 0
t = 1e5
phi = 0.9
mu = 0
sigma = 1

#Sample AR(1) Data, store in X
X <- AR_1(x_0, t, phi, mu, sigma)

#Estimate ess of X
ess(X)