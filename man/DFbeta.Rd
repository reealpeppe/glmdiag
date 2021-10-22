\name{DFbeta}

\alias{DFbeta}

\title{Visualize DFbetas}

\description{Graphical visualization of DFbetas for each observation and variable within input model.
}

\usage{DFbeta(object, label.id, n.label.id, xlab, ylab, vjust, hjust, points.size, points.col)
}

\arguments{
  \item{object}{object of class 'influence', see Details}
  \item{label.id}{labels of observations, should be a vector with n elements}
  \item{n.label.id}{number of observations with highest DFbetas to label in the plot}
  \item{xlab}{title for the x axis}
  \item{ylab}{title for the y axis}
  \item{vjust}{vertical alignment of labels}
  \item{hjust}{horizontal alignment of labels}
  \item{points.size}{size of the points}
  \item{points.col}{color of the points}
}

\details{
This function only works with objects given as output from \code{influenceDiag}.

DFbetas are calculated as \emph{B - B(-i)} hence they are not standardized.

Title is set as the name of current variable so it cannot be changed.
}

\author{
Giuseppe Reale
}


\examples{

## Cambiare dataset: visualizzazione troppo brutta con questo!

data("moons", package = "glmdiag")
model <- glm(Moons ~ Distance + Diameter + Mass, family = poisson, data = moons)
inf <- influenceDiag(model)
DFbeta(inf, label.id = moons$Name, xlab = 'Planet ID')


}
