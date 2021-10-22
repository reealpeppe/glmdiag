linkLin.betareg <- function(model, smooth = T, xlab, ylab, title, points.size, points.col) {
  y <- model$y
  mu <- fitted(model)
  eta <- model$link$mean$linkfun(mu)
  D.beta.inv <- model$link$mean$mu.eta(eta)
  wres <- (y-mu)/D.beta.inv
  zeta <- eta + wres
  DF <- data.frame(zeta = zeta, eta = eta)
  p <- linkLin_plot(DF, smooth = smooth, xlab, ylab, title, points.size, points.col)
  p
}




