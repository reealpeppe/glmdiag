qresid_gaussian <- function(model) {
  mu <- fitted(model)
  Sigma <- sigma(model)
  gaussian.cdf <- pnorm(model$y, mean = mu, sd = Sigma)
  res <- qnorm(gaussian.cdf)
  res
}
