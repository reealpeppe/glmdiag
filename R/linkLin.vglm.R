linkLin.vglm <- function(model, smooth = T, xlab, ylab, main, pch, lcol, lwd, link.name, ...) {
  fam <- model@family@vfamily
  if(fam != 'betabinomial') stop('Only betabinomial models are admitted within VGAM package')
  eta <- predict(model)[,1]
  wres <- residuals(model, type = 'working')[,1]
  zeta <- eta + wres
  link.name <- model@family@infos()$lmu

  linkLin_plot(zeta, eta, smooth = smooth, xlab, ylab, main, pch, lcol, lwd, link.name, ...)
}





