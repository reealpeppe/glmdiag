\name{cookDist}
\alias{cookDist}

\title{
Visualize Cook's distances
}
\description{
Graphical visualization of Cook's distances for each observation within input model.
}
\usage{
cookDist(object, label.id, n.label.id, xlab, ylab, title, vjust, hjust)
}
\arguments{
  \item{object}{object of class 'influence', see Details}
  \item{label.id}{labels of observations, should be a vector with n elements}
  \item{n.label.id}{number of observations with highest Cook's distance to label in the plot}
  \item{xlab}{title for the x axis}
  \item{ylab}{title for the y axis}
  \item{title}{an overall title for the plot}
  \item{vjust}{vertical alignment of labels}
  \item{hjust}{horizontal alignment of labels}

}

\details{
This function only works with objects given as output from \code{influenceDiag}, it takes Cook's distances and plot them as vertical segments.

High values are associated to high influence within the model.
}


\author{
Giuseppe Reale
}

\examples{

## Cambiare dataset: visualizzazione troppo brutta con questo!

data("moons", package = "glmdiag")
model <- glm(Moons ~ Distance + Diameter + Mass, family = poisson, data = moons)
inf <- influenceDiag(model)
cookDist(inf, n.label.id = 3)
}
