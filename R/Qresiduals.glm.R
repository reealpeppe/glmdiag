Qresiduals.glm <- function(model, plot.it = T, global.title, qq.xlab, qq.ylab, qq.title, qqline.col, qq.points.size, 
                       dns.xlab, dns.ylab, dns.title, dns.lines.col) {
  fam = model$family$family
  
  not.admitted <- c('quasi', 'quasibinomial', 'quasipoisson')
  if(fam %in% not.admitted) stop("Can't derive quantile residuals for quasi GLM")
  
  qres <- switch(fam,
              Gamma = qresid_gamma(model),
              poisson = qresid_pois(model),
              binomial = qresid_binom(model),
              inverse.gaussian = qresid_invgauss(model)
  )
  
  if(plot.it) {
    if(missing(global.title)) global.title <- 'Quantile residuals'
    plots <- qres_plot(qres, qq.xlab, qq.ylab, qq.title, qqline.col, qq.points.size, 
                       dns.xlab, dns.ylab, dns.title, dns.lines.col)
    p <- grid.arrange(plots$density.plot, plots$QQplot, ncol = 2, top = global.title)
    p
  }
  return(invisible(qres))
}

