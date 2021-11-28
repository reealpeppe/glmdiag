\name{Qresiduals}

\alias{Qresiduals}

\title{Graphical normality testing for quantile residuals}

\description{Derives quantile residuals of the input model and creates two plots: the first compares density of a standard normal distribution to the residuals' empirical density while the other one is a \emph{QQplot}. 
}

\usage{Qresiduals(model, plot.it = T)
}

\arguments{
  \item{model}{a model supported by \code{glmdiag}.}
  \item{plot.it}{logical, whether to plot the results or not.}
}

\details{
Quantile residuals are defined on a continuous cumulative distribution function; for binomial, beta-binomial, poisson and negative binomial regression models the \emph{Randomized quantile residuals} are used.

Residuals have an important role inside the global diagnostic of a regression model. Any departure from the standard normal distribution can be considered as a warning that one or more aspects of the model are misspecified. Quantile residuals are particullary useful with models that do not have a continuous response variable, such as a binomial or poisson models; see examples.
}

\value{
invisibly returns the calculated quantile residuals.
}

\author{
Giuseppe Reale
}

\references{
Peter K. Dunn and Gordon K. Smyth (1996). Randomized Quantile Residuals. Journal of Computational and Graphical Statistics.
}

\examples{

# Simulate the data

set.seed(10)
n <- 100
x1 <- rt(n, df = 4)
x2 <- rnorm(n, sd = 2)
b <- c(1, .5, .5)
eta <- b[1] + b[2] * x1 + b[3] * x2
prob <- exp(eta)/(1 + exp(eta))
y <- rbinom(n, size = 1, prob = prob)

# The model is correctly specified
mod <- glm(y ~ x1 + x2, family = binomial)

res.p <- residuals(mod, type = 'pearson')
res.d <- residuals(mod, type = 'deviance')
qres <- Qresiduals(mod, plot.it = FALSE)

shapiro.test(res.p) # not normal
shapiro.test(res.d) # not normal
shapiro.test(qres) # normal

y.hat <- fitted(mod)

plot(y.hat, res.p) # uninformative 
plot(y.hat, res.d) # uninformative 
plot(y.hat, qres)

Qresiduals(mod)

}
