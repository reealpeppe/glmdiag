devAnalysis <- function(model, cex.pl = .8, cex.vars = 1, cex.vars2 = 1, 
                        layout.heights = c(1.2,0.8,4), pl.scale = 1,
                        xlab, ylab, title) {
  UseMethod("devAnalysis")
}