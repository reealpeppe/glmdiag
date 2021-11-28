variableOut <- function(model, k = 2, update.it = F, xlab, ylab, pch, col, lty, ylim, ...) {

  if(missing(xlab)) xlab <- 'Excluded variable'
  if(missing(ylab)) ylab <- 'AIC'
  if(missing(pch)) pch <- 16
  if(missing(lty)) lty <- 2
  
  
  Terms <- terms(model)
  vars <- colnames(attr(Terms, "factors"))
  formulae <- Terms ; attributes(formulae) <- NULL
  n.scores <- length(vars)
  scores <- rep(NA, n.scores)
  models <- list()
  
  scoref <- function(mod) {
    AIC(mod, k = k)
  }
  score.full <- scoref(model)
  
  for(i in 1:n.scores) {
    new.formulae <- update.formula(formulae, paste(". ~ . -", vars[i]))
    new.model <- update(model, new.formulae)
    scores[i] <- scoref(new.model)
    models[[i]] <- new.model
  }
  
  x <- seq_len(n.scores)
  y0 <- rep(score.full, n.scores)
  diffs <- y0 - scores
  
  ymax <- max(scores)
  ymin <- min(scores)
  if(missing(ylim)) ylim <- c(ymin, ymax + .17*(ymax - ymin))
  
  if(missing(col)) col <- ifelse(diffs < 0, 1, 2)
  plot(x = x, y = scores, xaxt = "n", xlab = xlab, ylab = ylab, 
       pch = pch, col = col, ylim = ylim, ...)
  axis(side = 1, at = x, labels = vars)
  segments(x0 = x, y0 = y0, y1 = scores, col = col, lwd = 2)
  abline(h = score.full, lty = lty)
  legend("topleft", col = 1, lty = 2, legend = 'Full model', bty = 'n')
  
  if(update.it) {
    scores <- c(scores, score.full)
    models[[n.scores + 1]] <- model
    mod.out <- models[[which.min(scores)]]
    mod.out
  }
}

