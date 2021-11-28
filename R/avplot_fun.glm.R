avplot_fun.glm <- function(model, variable, type, label.id, n.label.id, xlab, ylab, main,
                           pos, pch, cex, lcol, lwd, lty, ...) {
  
  n.obs <- nobs(model)
  index <- seq_len(n.obs)
  
  frm <- formula(model)
  y.name <- as.character(frm[2])
  
  if(missing(label.id)) label.id <- index
  if(missing(n.label.id)) n.label.id <- 2
  if(missing(xlab)) xlab <- paste(variable, '- residuals')
  if(missing(ylab)) ylab <- paste(y.name, '- residuals')
  if(missing(main)) main <- paste('Added variable plot -', variable)
  if(missing(pos)) pos <- 2
  if(missing(pch)) pch <- 16
  if(missing(cex)) cex <- .9
  if(missing(lcol)) lcol <- 2
  if(missing(lwd)) lwd <- 1.5
  if(missing(lty)) lty <- 2
  
  mm <- model.matrix(model)
  var.names <- colnames(mm)
  var.index <- which(var.names == variable)
  var <- mm[ , var.index]
  mm.red <- mm[, -var.index, drop = F]

  
  if(!missing(variable)) if(!(variable %in% var.names)) stop('variable not found')
  y <- model$y
  prior.w <- model$prior.weights
  mod.red <- glm.fit(x = mm.red, y = y, weights = prior.w, family = family(model))
  class(mod.red) <- 'glm'
  wres <- residuals(mod.red, type = 'working')  
  w <- if(type == 'Wang') sqrt(mod.red$weights)
      else sqrt(model$weights) 
  
  
  
  res.y <- w * wres
  res.x <- lm.wfit(x = mm.red, y = var, w = w * prior.w)$residuals
  
  res.lm <- lm(res.y ~ res.x, weights = w * prior.w)
  res.cd <- cooks.distance(res.lm)
  points.lab <- getMaxIndex(res.cd, label.id, k = n.label.id)

  plot(x = res.x, y = res.y, xlab = xlab, ylab = ylab,
       main = main, pch = pch, cex = cex, ...)
  abline(res.lm, col = lcol, lwd = lwd, lty = lty)
  text(x = res.x, y = res.y, label = points.lab, pos = pos)

    
}
