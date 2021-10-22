linkLin.vglm <- function(model, smooth = T, xlab, ylab, title, points.size, points.col) {
  fam <- model@family@vfamily
  if(fam != 'betabinomial') stop('Only betabinomial models are admitted within VGAM package')
  y <- model@y
  mu <- fitted(model)
  eta <- predict(model)[,1]
  link.name <- model@family@infos()$lmu
  link.name <- strsplit(link.name, "link")[[1]]
  link.call <- paste0("binomial(link = ",link.name,")")
  link <- eval(parse(text = link.call))
  D.beta.inv <- link$mu.eta(eta)
  wres <- (y - mu)/D.beta.inv
  zeta <- eta + wres
  
  DF <- data.frame(zeta = zeta, eta = eta)
  p <- linkLin_plot(DF, smooth = smooth, xlab, ylab, title, points.size, points.col)
  
  p
}





