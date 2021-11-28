\name{cookDist}
\alias{cookDist}

\title{
Visualize Cook's distances
}
\description{
Graphical visualization of Cook's distances for each observation within input model.
}
\usage{
cookDist(object, label.id, n.label.id, xlab, ylab, pos, ...)
}
\arguments{
  \item{object}{object of class 'influence', see Details.}
  \item{label.id}{labels of observations, should be a vector with n elements.}
  \item{n.label.id}{number of observations with highest Cook's distance to label in the plot.}
  \item{xlab}{title for the x axis.}
  \item{ylab}{title for the y axis.}
  \item{pos}{position of observations labels. Values \code{1, 2, 3, 4} respectively indicate below, left, above, right.}
  \item{...}{further arguments passed to \code{plot}}


}

\details{
This function only works with objects given as output from \code{influenceDiag}, it takes Cook's distances and plot them as vertical segments.

High values are associated to high influence within the model.
}


\author{
Giuseppe Reale
}

\examples{
data("mtcars")
mod <- glm(mpg ~ cyl + hp + carb, family = Gamma, data = mtcars)
inf <- influenceDiag(mod)
cookDist(inf, label.id = rownames(mtcars))

}
