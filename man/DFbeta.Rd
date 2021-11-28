\name{DFbeta}

\alias{DFbeta}

\title{Visualize DFbetas}

\description{Graphical visualization of DFbetas for each observation and chosen variables within input model.
}

\usage{DFbeta(object, variables, centered = T, label.id, n.label.id, 
              xlab, ylab, main, pos, pch, cex, lcol, lwd, lty, ...)
}

\arguments{
  \item{object}{object of class 'influence', see Details.}
  \item{variables}{a vector of characters containing names of variables; if nothing is specified, DFbetas for all variables are plotted.}
  \item{centered}{logical, if TRUE the plot is centered on the coefficients of the full model and each point corresponds to the coefficient if the i-th observation is excluded. if FALSE, usual DFbetas centered on zero are plotted.}
  \item{label.id}{labels of observations, should be a vector with n elements.}
  \item{n.label.id}{number of observations with highest DFbetas to label in the plot.}
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
This function only works with objects given as output from \code{influenceDiag}.

DFbetas are calculated as \emph{B - B(-i)} hence they are not standardized; if \code{centered} is set to TRUE then each plotted point corresponds to B(-i), otherwise to B - B(-i).
}

\author{
Giuseppe Reale
}


\examples{

data("mtcars")
mod <- glm(mpg ~ cyl + hp + carb, family = Gamma, data = mtcars)
inf <- influenceDiag(mod)
DFbeta(inf, label.id = rownames(mtcars))
DFbeta(inf, variables = 'cyl', label.id = rownames(mtcars), pos = 2)

# to visualize all of them in a single plot
par(mfrow = c(2, 2))
DFbeta(inf, label.id = rownames(mtcars))
# press <Enter> four times
par(mfrow = c(1, 1))



}
