\name{leverage}
\alias{leverage}

\title{
Visualize Leverages
}
\description{
Graphical visualization of leverages for each observation within input model. Corresponds to the main diagonal of the hat matrix, i.e \emph{H[i,i]}. 
}
\usage{
leverage(object, label.id, n.label.id, xlab, ylab, pos,
         hline, lcol, lwd, lty, ...)
}
\arguments{
  \item{object}{object of class 'influence', see Details.}
  \item{label.id}{labels of observations, should be a vector with n elements.}
  \item{n.label.id}{number of observations with highest leverage to label in the plot.}
  \item{xlab}{title for the x axis.}
  \item{ylab}{title for the y axis.}
  \item{pos}{position of observations labels. Values \code{1, 2, 3, 4} respectively indicate below, left, above, right.}
  \item{hline}{numeric, where to position the horizontal line; useful to individuate points that exceed a specific treshold. Defaults to 2*p/n.}
  \item{lcol}{color of line.}
  \item{lwd}{width of line.}
  \item{lty}{type of line.}
  \item{...}{further arguments passed to \code{plot}}

}

\details{
This function only works with objects given as output from \code{influenceDiag}.

A rule of thumbs says that an observations has high leverage if H[i,i] > 2*p/n where p is the number of coefficients and n the sample size. 
}


\author{
Giuseppe Reale
}

\examples{
data("mtcars")
mod <- glm(mpg ~ cyl + hp + carb, family = Gamma, data = mtcars)
inf <- influenceDiag(mod)
leverage(inf, label.id = rownames(mtcars))
}
