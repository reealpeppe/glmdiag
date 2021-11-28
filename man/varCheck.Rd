\name{varCheck}

\alias{varCheck}

\title{Checking adequacy of variance function}

\description{If variance function is right then the relationship between squared Pearson residuals and fitted values should be linear; otherwise it means that the chosen variance function increases more (or less) quickly than mu. Hence this plot suggests the direction of the error made by chosing the variance function.}

\usage{varCheck(model, xlab, ylab, pch, ...)}

\arguments{
  \item{model}{a model supported by \code{glmdiag}.}
  \item{xlab}{title for the x axis.}
  \item{ylab}{title for the y axis.}
  \item{pch}{type of points.}
  \item{...}{further arguments passed to \code{plot}}
  
}


\author{
Giuseppe Reale
}


\examples{

data(stopping, package = 'glmdiag')
mod <- glm(Distance ~ Speed, family = Gamma(link = 'sqrt'), data = stopping)
varCheck(mod)


}
