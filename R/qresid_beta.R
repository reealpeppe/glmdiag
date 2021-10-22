qresid_beta <- function(model) {
  
  y <- model$y
  X.phi <- model.matrix(model, model = 'precision')
  coefs.phi <- model$coefficients$precision
  
  g.phi.hat <- X.phi %*% coefs.phi 
  phi.hat <- model$link$precision$linkinv(g.phi.hat)
  mu.hat <- fitted(model)
  
  shape1 <- mu.hat * phi.hat
  shape2 <- phi.hat - shape1
  beta.cdf <- pbeta(y, shape1, shape2)
  res <- qnorm(beta.cdf)
  res
}
