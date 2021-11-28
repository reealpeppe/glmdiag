\name{variableOut}

\alias{variableOut}

\title{Excluding a variable from the model}

\description{Graphically compares models in terms of Information Criterion, each one corresponds to a model where a specific variable is deleted from the linear predictor. }

\usage{variableOut(model, k = 2, update.it = F, xlab, ylab, pch, 
                   col, lty, ylim, ...)}

\arguments{
  \item{model}{a model supported by \code{glmdiag}.}
  \item{k}{numeric, the penalty per parameter to be used; the default \code{k = 2} is the classical AIC.}
  \item{update.it}{logical; if TRUE, model without the variable corresponding to the lowest score is returned.}
  \item{xlab}{title for the x axis.}
  \item{ylab}{title for the y axis, by default is 'AIC'; should be changed if a different k is chosen.}
  \item{pch}{type of points.}
  \item{col}{color of points and segments.}
  \item{lty}{type of horizontal line.}
  \item{ylim}{y limits of the plot.}
  \item{...}{further arguments passed to \code{plot.}}
}



\details{
Each plotted point corresponds to the score of the model where the variable indicated on x axis is excluded.
A dashed line is drawn in correspondence of the full model score.

Points and segments corresponding to variables whose deletion lead to a increment of the scored are black, those who lead to a decrement are red.

The output plot can be seen as a graphic version of the first step of \code{stepAIC} function inside \code{MASS} package.

Theory about Information Criterion suggests that if the minimum score doesn't belong to the full model, then the linear predictor may not be appropriate.

}


\author{
Giuseppe Reale
}


\examples{
data("moons")
model <- glm(Moons ~ Diameter * Mass + Distance, family = poisson, data = moons)
variableOut(model)

n <- nobs(model)
new.model <- variableOut(model, k = log(n), ylab = 'BIC', update.it = TRUE)
}
