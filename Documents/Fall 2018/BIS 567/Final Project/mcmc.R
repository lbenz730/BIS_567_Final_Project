library(pgdraw)
library(MCMCpack)
library(dplyr)
library(Matrix)

matrix_data <- read.csv("matrix_data.csv", as.is = T)

### Time Subset of the Data
time_min <- 0
time_max <- 10

subset <- filter(matrix_data, secs_remaining > time_min, secs_remaining <= time_max)

y <- subset$win
x <- cbind(matrix(1, nrow = nrow(subset)), as.matrix(subset[,-1]))
colnames(x)[1] <- "intercept"
x <- as.matrix(subset[,-1])

### Initialize Memory to Store Samples 
n_draws <- 10000
n <- length(y)
p <- ncol(x)
beta <- matrix(NA, ncol = p, nrow = n_draws)
colnames(beta) <- colnames(x)
omega <- matrix(NA, ncol = n, nrow = n_draws)

### Save Covariance Matrix for Prior distribution for Betas
prior_covariance <- diag(1/10000, p)

### Vector of 1 for random PG draws
ones <- rep(1, n)

### Intialize values
beta[1,] <- 0
omega <- pgdraw(ones, rep(0, n)) ### Can Only store 1 round of omega

### MCMC Draws 
for(t in 2:n_draws) {
  if(t %% 100 == 0) {
   cat("MCMC Iteration:", t, "of", n_draws, "\n") 
  }
  ### Get mu, Sigma for beta full conditional
  omega_matrix <- Diagonal(n, omega)
  m <- (y - 0.5)/omega
  covariance_matrix <- solve(t(x) %*% omega_matrix %*% x + prior_covariance)
  mu <- covariance_matrix %*% t(x) %*% omega_matrix %*% m
  
  ### Update Betas
  beta[t,] <- as.vector(mvrnorm(n = 1, mu, covariance_matrix))
  
  ### Update Omegas
  omega <- pgdraw(ones, x %*% beta[t,])
  
}

write.csv(beta, paste0("mcmc_draws/beta_", time_min, "_", time_max, ".csv"), row.names = F)
