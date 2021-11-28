VectDev <- function(model, mat) {
  nrow.mat <- nrow(mat)
  out <- rep(NA, nrow.mat)
  vars <- colnames(mat)
  p <- ncol(mat)
  for(i in 1:(nrow.mat-1)) {
    new.form <- '. ~ . '
    for(j in 1:p) {
      if(mat[i, j] == 0) new.form <- paste(new.form, '-', vars[j])
    }
    new.glm <- update(model, formula = as.formula(new.form))
    out[i] <- deviance(new.glm)
  }
  out[nrow.mat] <- deviance(model)
  out
}