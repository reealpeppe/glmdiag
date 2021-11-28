Qresiduals.glm <- function(model, plot.it = T) {
  fam = model$family$family
  
  not.admitted <- c('quasi', 'quasibinomial', 'quasipoisson')
  if(fam %in% not.admitted) stop("Can't derive quantile residuals for quasi GLM")
  
  qres <- switch(fam,
              Gamma = qresid_gamma(model),
              poisson = qresid_pois(model),
              binomial = qresid_binom(model),
              inverse.gaussian = qresid_invgauss(model),
              gaussian = qresid_gaussian(model)
  )
  
  if(plot.it) qres_plot(qres)
  
  return(invisible(qres))
}

