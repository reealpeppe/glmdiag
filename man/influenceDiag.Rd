\name{influenceDiag}
\alias{influenceDiag}
\title{
Influence Diagnostic Measures
}
\description{
Calculates or extracts some influence diagnostic measures such as DFbetas, Cook's distance and leverage. 
}
\usage{
influenceDiag(model, approx = F)
}
\arguments{
  \item{model}{a model supported by \code{glmdiag}.}
  \item{approx}{logical, if TRUE the function is faster but returns approximated results.}
}


\details{
Leverage is extracted from the models using \code{hatvalues} function of each model class.

The elements for GLMs and negative binomial regression models are always approximated as they are extracted from \code{influence} function.

The argument \code{approx} can be useful only with models of class \emph{vglm} and \emph{betareg} since \code{influence} method function is not defined and items are fully calculated. It estimates \emph{n} models where observations are excluded one by one. When \code{approx = T} the \emph{tol} parameter within every fitting model function is set high such that the fitting process stops at the first iteration hence results are not exact but they are supposed to be close.

The aim of the function is to group these diagnostic measures in one list; one should use \code{influenceDiag} to calculate the measures and use the output object inside functions \code{DFbeta}, \code{cookDist} and \code{leverage} in order to graphically visualize results.
}
\value{
Returns a list of class "influence" with the following elements:
\item{DFbeta}{data frame containing dfbetas for all observations and variables.}
\item{cookDist}{a vector containing cook's distances.}
\item{leverage}{hat values, i.e. the diagonal of the hat matrix.}
\item{full.beta}{coefficients of the full model.}
}

\author{
Giuseppe Reale
}

\examples{
data("mtcars")
mod <- glm(mpg ~ cyl + hp + carb + wt, family = Gamma, data = mtcars)
inf <- influenceDiag(mod)
cookDist(inf)
leverage(inf)

}