qresid_binom = function(model) {
  p.obs <- model$y
  n <- model$prior.weights
  y <- n * p.obs
  pi.hat <- fitted(model)
  u <- runif(n)
  binom.cdf <- pbinom(round(y)-1, size = n, prob = pi.hat) + u * dbinom(round(y), size = n, prob = pi.hat)
  res <- qnorm(binom.cdf)
  res
  
}