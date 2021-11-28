influenceDiag.glm <- function(model, approx = F) {
  inf <- influence(model)
  db <- inf$coefficients
  cookd <- cooks.distance(model, infl = inf)
  out <- list(DFbeta = db, cookDist = cookd, leverage = inf$hat, full.beta = coef(model))
  attr(out, which = 'class') <- 'influence'
  out
  
}



