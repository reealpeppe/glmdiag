linkLin.betareg <- function(model, smooth = T, xlab, ylab, main, pch, lcol, lwd, ...) {
  y <- model$y
  mu <- fitted(model)
  eta <- model$link$mean$linkfun(mu)
  phi <- predict(model, type = "precision")
  Ti <- model$link$mean$mu.eta(eta)
  y.star <- log(y / (1 - y))
  mu.star <- digamma(mu * phi) - digamma((1 - mu) * phi)
  v <- trigamma(mu * phi) + trigamma((1 - mu) * phi)
  w <- phi * v * Ti^2
  
  wres <- Ti * (y.star - mu.star)/w
  zeta <- eta + wres
  link.name <- model$link$mean$name
  linkLin_plot(zeta, eta, smooth = smooth, xlab, ylab, main, pch, lcol, lwd, link.name, ...)
}

