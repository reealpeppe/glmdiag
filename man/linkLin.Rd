\name{linkLin}

\alias{linkLin}

\title{Checking linearity of link function}

\description{According to Lovison (2014), if the link function is correctly specified then there is a linear relationship between the working response variable \emph{z} and the linear predictor \emph{eta}. This plot suggests if the link function is appropriate.}

\usage{linkLin(model, smooth = T, xlab, ylab, main, pch, lcol, lwd, ...)}

\arguments{
  \item{model}{a model supported by \code{glmdiag}.}
  \item{smooth}{logical, whether to plot a smoothing spline.}
  \item{xlab}{title for the x axis.}
  \item{ylab}{title for the y axis.}
  \item{main}{an overall title for the plot.}
  \item{pch}{type of points.}
  \item{lcol}{color of the smoothing line.}
  \item{lwd}{size of the smoothing line.}
  \item{...}{further arguments passed to \code{plot}}
  
}


\author{
Giuseppe Reale
}


\examples{

## Simulate the data


set.seed(5)
n.obs <- 100
x <- rnorm(n.obs)
shape <- 25
mu <- exp(1 + .5* x)
y <- rgamma(n.obs, rate = shape / mu, shape = shape)

true.mod <- glm(y ~ x, family = Gamma(link = 'log'))
wrong1 <- glm(y ~ x, family = Gamma(link = 'inverse'))
wrong2 <- glm(y ~ x, family = Gamma(link = 'identity'))

par(mfrow = c(2, 2))
linkLin(true.mod)
linkLin(wrong1)
linkLin(wrong2)
par(mfrow = c(1, 1))

}
