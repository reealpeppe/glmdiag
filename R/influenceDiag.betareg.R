influenceDiag.betareg <- function(model, approx = F) {
  Coefs <- coef(model)
  n.obs <- nobs(model)
  p <- length(Coefs)
  DFbetas <- matrix(NA, nrow = n.obs, ncol = p)
  colnames(DFbetas) <- names(Coefs)

  w <- model$weights
  if(is.null(w)) w <- rep(1, n.obs)
  
  epsilon <- model$control$fstol
  if(approx) epsilon <- 1e3
  
  pb <- txtProgressBar(min = 0, max = n.obs, style = 3, char = "*", width = 30)
  
  for(i in 1:n.obs) {
    w.i <- w
    w.i[i] <- 0
    mod.i <- update(model, start = Coefs, weights = w.i, fstol = epsilon)
    DFbetas[i,] <- Coefs - coef(mod.i)
    setTxtProgressBar(pb, i)
  }
  
  hii <- hatvalues(model)
  cookd <- cooks.distance(model)
  
  out <- list(DFbeta = DFbetas, cookDist = cookd, leverage = hii)
  attr(out, which = 'class') <- 'influence'
  out
}
