varCheck.betareg <- function(model, xlab, ylab, pch, ...) {
  
  if(missing(xlab)) xlab <- expression(mu)
  if(missing(ylab)) ylab <- 'squared Pearson residuals'
  if(missing(pch)) pch <- 16
  
  r.p <- residuals(model, type = 'pearson')^2
  y.hat <- fitted(model)
  plot(y.hat, r.p, pch = 16, xlab = xlab, ylab = ylab, ...)
}
