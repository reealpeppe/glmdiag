qresid_betabin <- function(model)  {
  
  n <- model@prior.weights
  y <- model@y * n
  mu <- model@fitted.values
  rho <- model@misc$rho
  a <- pbetabinom(y - 1, n, mu, rho)
  b <- pbetabinom(y, n, mu, rho)
  betabin.cdf <- runif(length(y), min = a, max = b)
  res <- qnorm(betabin.cdf)
  res
}