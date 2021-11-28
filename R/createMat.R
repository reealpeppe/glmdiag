createMat <- function(vars) { # crea la matrice con tutti i predittori
  out <- list()
  for(v in vars) {
    out[v] <- list(c(0, 1))
  }
  expand.grid(out)
}