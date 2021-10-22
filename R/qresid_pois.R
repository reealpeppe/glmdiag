qresid_pois <- function(model) {
  y <- model$y
  u <- runif(n = length(y))
  mu <- fitted(model)
  cdf.pois <- ppois(y-1, lambda = mu) + u * dpois(y, lambda = mu)
  res <- qnorm(cdf.pois)
  res
}