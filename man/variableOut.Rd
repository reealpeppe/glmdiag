\name{variableOut}

\alias{variableOut}

\title{Excluding a variable from the model}

\description{Compares graphically models in terms of Information Criteria, each one corresponds to a model where a specific variable is deleted from the linear predictor. }

\usage{variableOut(model, k = 2, score.name = 'Score')}

\arguments{
  \item{model}{a model supported by \code{glmdiag}}
  \item{k}{numeric, the penalty per parameter to be used; the default \code{k = 2} is the classical AIC.}
  \item{score.name}{name of the score to be used as label for the y axis}
}


\author{
Giuseppe Reale
}

\details{
Each plotted line corresponds to the score of the model where the variable indicated on x axis is excluded, making exception for the first one which refers to the full model; a red dashed line is drawn in corrispondence of the minimum score.

The output plot can be seen as a graphic version of the first step of \code{stepAIC} function inside \code{MASS} package.

Theory about Information Criteria suggests that if the minimum score doesn't belong to the full model, then the linear predictor may not be appropriate.

}

\examples{
data(moons, package = 'glmdiag')
model <- glm(Moons ~ Diameter * Mass + Distance, family = poisson, data = moons)
variableOut(model, score.name = 'AIC')

n <- nobs(model)
variableOut(model, k = log(n), score.name = 'BIC')
}
