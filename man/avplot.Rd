\name{avplot}

\alias{avplot}

\title{Added Variable Plot}

\description{Constructs added variable plots for generalized linear models and extensions admitted by \code{glmdiag}.
}

\usage{avplot(model, variables, type = c("Wang", "Hines-Carter"), label.id,
              n.label.id, xlab, ylab, main, pos, pch, cex, lcol, lwd, lty, ...)

}

\arguments{
  \item{model}{a model supported by \code{glmdiag}.}
  \item{variables}{a vector of characters containing names of regressors; must be included inside the model matrix. If nothing is specified, avplot is applied on all variables.}
  \item{type}{default is "\code{Wang}" developed by Wang (1985); if "\code{Hines-Carter}" the method developed by Hines and Carter (1993) is used.}
  \item{label.id}{labels of observations, should be a vector with n elements}
  \item{n.label.id}{number of observations to label in the plot that most influence the "clean" relationship of considered variables. Cook's distance is used as the measure of influence.}
  \item{xlab}{title for the x axis.}
  \item{ylab}{title for the y axis.}
  \item{main}{an overall title for the plot.}
  \item{pos}{position of observations labels. Values \code{1, 2, 3, 4} respectively indicate below, left, above, right.}
  \item{pch}{type of points.}
  \item{cex}{size of points.}
  \item{lcol}{color of line.}
  \item{lwd}{width of line.}
  \item{lty}{type of line.}
  \item{...}{further arguments passed to \code{plot}}
}

\details{
The aim of added variable plot is to isolate the relationship between a specific variable and the response one, i.e. fixing all the other variables included in the linear predictor. In the y-axis there are the \emph{working residuals} of the reduced model (i.e. a model where the variable we're interested in, say "x", is excluded) while on the x-axis there are the residuals of regression of x using all the other variables.
Values in both axis are weighted: Wang (1985) uses the \emph{working weights} of the reduced model while Hines and Carter (1993) suggest that using the weights of the full model is more reliable in indicating the presence of influential observations.
}

\references{
Wang, P C. (1985) Adding a variable in generalized linear models. Technometrics 27, 273-276.

R. J. O'Hara Hines, & Carter, E. M. (1993). Improved Added Variable and Partial Residual Plots for the Detection of Influential Observations in Generalized Linear Models. Journal of the Royal Statistical Society. Series C (Applied Statistics), 42(1), 3-20.
}

\author{
Giuseppe Reale
}


\examples{
data(moons)
m <- glm(Moons ~ Mass + Distance + Diameter, 
         family = poisson, data = moons)
summary(m)
avplot(m, 'Distance', label.id = moons$Name )

}
