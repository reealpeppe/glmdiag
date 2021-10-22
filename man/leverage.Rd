\name{leverage}
\alias{leverage}

\title{
Visualize Leverages
}
\description{
Graphical visualization of leverages for each observation within input model. Corresponds to the main diagonal of the hat matrix, i.e \emph{H[i,i]}. 
}
\usage{
leverage(object, label.id, n.label.id, xlab, ylab, title, 
         vjust, hjust, points.size, points.col)
}
\arguments{
  \item{object}{object of class 'influence', see Details}
  \item{label.id}{labels of observations, should be a vector with n elements}
  \item{n.label.id}{number of observations with highest leverage to label in the plot}
  \item{xlab}{title for the x axis}
  \item{ylab}{title for the y axis}
  \item{title}{an overall title for the plot}
  \item{vjust}{vertical alignment of labels}
  \item{hjust}{horizontal alignment of labels}
  \item{points.size}{size of the points}
  \item{points.col}{color of the points}

}

\details{
This function only works with objects given as output from \code{influenceDiag}.

A rule of thumbs says that an observations has high leverage if \emph{H[i,i] > 2*p/n} where p is the number of coefficients and n the sample size. 
}


\author{
Giuseppe Reale
}

\examples{

## Cambiare dataset: visualizzazione troppo brutta con questo!

data("moons", package = "glmdiag")
model <- glm(Moons ~ Distance + Diameter + Mass, family = poisson, data = moons)
inf <- influenceDiag(model)
leverage(inf, n.label.id = 3)
}
