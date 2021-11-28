avplot <- function(model, variables, type = c("Wang", "Hines-Carter"), label.id, n.label.id, xlab, 
                   ylab, main, pos, pch, cex, lcol, lwd, lty, ...) {
  
  cl <- match.call()
  type <- match.arg(type)
  
  mm <- model.matrix(model)
  var.names <- colnames(mm)
  
  if(missing(variables)) variables <- var.names
  
  else {
    model.name <- as.character(cl$model)
    error <- sprintf("Variables not found, run 'colnames(model.matrix(%s))' for the available names", model.name)
    if(!any(variables %in% var.names)) stop(error)
  }
  
  p <- length(variables)
  avplot_fun(model, variable = variables[1], type = type, label.id, n.label.id, xlab, ylab, main,
             pos, pch, cex, lcol, lwd, lty, ...)
  
  if(p > 1) {
    for(i in 2:p) {
      readline(prompt = "Type <Enter> to go to the next plot: ")
      avplot_fun(model, variable = variables[i], type = type, label.id, n.label.id, xlab, ylab, main,
                 pos, pch, cex, lcol, lwd, lty, ...)
    }
  }
}



