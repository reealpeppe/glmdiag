qresid_invgauss <- function(model) {
  y <- model$y
  mu <- fitted(model)
  disp <- summary(model)$dispersion
  Lambda <- 1/disp
  invgauss.cdf <- pinv.gaussian(y, mu, Lambda)
  res <- qnorm(invgauss.cdf)
  res
}