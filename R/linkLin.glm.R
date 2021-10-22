linkLin.glm <- function(model, smooth = T, xlab, ylab, title, points.size, points.col) {
  eta <- model$linear.predictor
  wres <- residuals(model, type = "working")
  zeta <- eta + wres
  DF <- data.frame(zeta = zeta, eta = eta)
  p <- linkLin_plot(DF, smooth = smooth, xlab, ylab, title, points.size, points.col)
  
  p
}
