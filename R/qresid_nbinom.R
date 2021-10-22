qresid_nbinom <- function(model) {
  
  y <- model$y
  nobs <- length(y)
  size <- model$theta
  mu <- fitted(model)
  
  a <- pnbinom(y-1, size = size, mu = mu)
  b <- pnbinom(y, size = size, mu = mu)
  u <- runif(nobs, a, b)
  res <- qnorm(u)
  res
}