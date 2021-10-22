influenceDiag.vglm <- function(model, approx = F) {
  
  fam <- model@family@vfamily
  if(fam != 'betabinomial') stop('Only betabinomial models are admitted within VGAM package')
  
  Coefs <- coef(model)
  n.obs <- nobs(model)
  p <- model@rank
  DFbetas <- matrix(NA, nrow = n.obs, ncol = p)
  colnames(DFbetas) <- names(Coefs)
  w <- rep(1, n.obs)
  epsilon <- model@control$epsilon
  
  if(approx) epsilon <- 1e3
  
  pb <- txtProgressBar(min = 0, max = n.obs, style = 3, char = "*", width = 30)
  
  for(i in 1:n.obs) {
    w.i <- w
    w.i[i] <- 1e-7
    mod.i <- update(model, coefstart = Coefs, weights = w.i, epsilon = epsilon)
    DFbetas[i,] <- Coefs - coef(mod.i)
    setTxtProgressBar(pb, i)
  }
  
  hii <- hatvaluesvlm(model)[ ,1]
  res <- VGAM::residuals(model, type = 'pearson')[ ,1]
  cookd <- hii * res^2 / (p * (1 - hii)^2)
  
  out <- list(DFbeta = DFbetas, cookDist = cookd, leverage = hii)
  attr(out, which = 'class') <- 'influence'
  out
}
