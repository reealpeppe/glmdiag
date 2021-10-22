\name{linkLin}

\alias{linkLin}

\title{Checking linearity of link function}

\description{If the link function is correctly specified then there is a linear relationship between the working response variable \emph{z} and the linear predictor \emph{eta}. This plot suggests if the link function is appropriate.}

\usage{linkLin(model, smooth = T, xlab, ylab, title, points.size, points.col)}

\arguments{
  \item{model}{a model supported by \code{glmdiag}}
  \item{smooth}{logical, whether to plot a smoothing spline}
  \item{xlab}{title for the x axis}
  \item{ylab}{title for the y axis}
  \item{title}{an overall title for the plot}
  \item{points.size}{size of the points}
  \item{points.col}{color of the points}
  
}


\author{
Giuseppe Reale
}

\seealso{ ???? }
\examples{

## Use real data! Moons is cute

set.seed(5)
x <- rgamma(50, shape = 3, rate = 2)
eta <- 1 + 5 * x + rnorm(50)
y <- 1/eta

par(mfrow = c(1, 2))
correct.mod <- glm(y ~ x, family = Gamma(link = 'inverse'))
wrong.mod <- glm(y ~ x, family = Gamma(link = 'log'))

linkLin(correct.mod, title = 'True link function')
linkLin(wrong.mod, title = 'Wrong link function')


}
