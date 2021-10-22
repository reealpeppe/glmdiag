qresid_gamma <- function(model) {
  disp <- summary(model)$dispersion 
  betas <- 1/(fitted(model) * disp) 
  gamma.cdf <- pgamma(model$y, rate = betas, shape = 1/disp)
  res <- qnorm(gamma.cdf)
  res
}