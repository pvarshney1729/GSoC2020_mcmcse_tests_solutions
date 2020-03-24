#Import Library and Set Random Seed
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

#Returns Batch Means for non-overlapping batches of X
batch_means <- function(X, t, m, B) {
  X_bar <- numeric(length = B)
  
  for (j in 1:B){
    X_bar[j] = mean(X[((j-1)*m+1):(j*m)])
  }
  
  return(X_bar)
}

#Point Estimator over a replication
point_estimator <- function(X_bar, B, k) {
  X_BM = mean(X_bar)
  
  return(X_BM)
}

#Calculate RBM Estimator
RBM <- function(k, B, m, X_bar){
  #Point Replication over all replications
  X_RBM = point_estimator(X_bar)
  
  V_RBM = (m/(k*B-1))*sum((X_bar - X_RBM)^2)
  
  return(V_RBM)
}

#Initialise variables
k <- 4
t <- 1e5
m = floor(sqrt(n))
B = m
x_0 = 0
phi = 0.9
mu = 0
sigma = 1

X <- matrix(nrow = k, ncol = t)
X_bar <- matrix(nrow = k, ncol = B)


for (i in 1:k) {
  X[i, ] <- AR_1(x_0, t, phi, mu, sigma)
  X_bar[i, ] <- batch_means(X[i, ], t, m, B)
}

RBM(k, B, m, X_bar)
