\name{Qresiduals}

\alias{Qresiduals}

\title{Graphical normality testing for quantile residuals}

\description{Derives quantile residuals of the input model and creates two plots: the first compares density of a standard normal distribution to the residuals' empirical density while the other one is a \emph{QQplot}. 
}

\usage{Qresiduals(model, plot.it = T, global.title, qq.xlab, qq.ylab, qq.title, qqline.col,
              qq.points.size, dns.xlab, dns.ylab, dns.title, dns.lines.col)
}

\arguments{
  \item{model}{a model supported by \code{glmdiag}}
  \item{plot.it}{logical, whether to plot the results or not}
  \item{global.title}{an overall title for the plot}
  \item{qq.xlab}{title for the x axis of qqplot}
  \item{qq.ylab}{title for the y axis of qqplot}
  \item{qq.title}{title of qqplot}
  \item{qqline.col}{color of the qqline}
  \item{qq.points.size}{size of the points of qqplot}
  \item{dns.xlab}{title for the x axis of density plot}
  \item{dns.ylab}{title for the y axis of density plot}
  \item{dns.title}{title of density plot}
  \item{dns.lines.col}{a vector of two elements, colors of density lines}
}

\details{
Quantile residuals are defined on a continuous cumulative distribution function; for binomial, beta-binomial, poisson and negative binomial regression models the \emph{Randomized quantile residuals} are used.

Residuals have an important role inside the global diagnostic of a regression model. Any departure from the standard normal distribution can be considered as a warning that one or more aspects of the model are misspecified. Quantile residuals are particullary useful with models that do not have a continuous response variable, such as a binomial or poisson models; see examples.
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
b <- rnorm(3)
eta <- b[1] + b[2] * x1 + b[3] * x2 + rnorm(n)
prob <- exp(eta)/(1 + exp(eta))
y <- ifelse(prob > .5, 1, 0)

# The model is correctly specified
mod <- glm(y ~ x1 + x2, family = binomial)

res.p <- residuals(mod, type = 'pearson')
res.d <- residuals(mod, type = 'deviance')
qres <- Qresiduals(mod)

shapiro.test(res.p) # not normal
shapiro.test(res.d) # not normal
shapiro.test(qres) # normal

y.hat <- fitted(mod)

plot(y.hat, res.p) # uninformative 
plot(y.hat, res.d) # uninformative 
plot(y.hat, res.p)

}
