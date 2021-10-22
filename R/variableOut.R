variableOut <- function(model, k = 2, score.name = 'Score') {
  Terms <- terms(model)
  vars <- colnames(attr(Terms, "factors"))
  formulae <- Terms ; attributes(formulae) <- NULL
  n.scores <- length(vars) + 1
  scores <- rep(NA, n.scores)
  
  scoref <- function(mod) {
    AIC(mod, k = k)
  }
  
  scores[1] <- scoref(model)
  
  for(i in 2:n.scores) {
    new.formulae <- update.formula(formulae, paste(". ~ . -", vars[i - 1]))
    new.model <- update(model, new.formulae)
    scores[i] <- scoref(new.model)
  }
  
  score.name <- paste0(score.name, '\n')
  
  x = factor(1:n.scores)
  
  DF <- data.frame(x = x, scores = scores)
  p <- ggplot(DF, aes(x = x, xend = x, y = (min(scores) - 1), yend = scores)) + 
    geom_segment() + 
    geom_point(y = scores, x = factor(1:n.scores), size = 2) +
    scale_x_discrete(labels = c("Full", vars)) +
    geom_hline(yintercept = min(scores), col = "red", lty = 2) +
    ylab(score.name) + 
    xlab("") + 
    theme_classic() +
    theme( 
      axis.text.x = element_text(size = 12),
      plot.margin = margin(0.7,0.7,0.7,0.7, "cm")) 
  print(p)
}

