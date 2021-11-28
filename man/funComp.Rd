\name{funComp}
\alias{funComp}

\title{
Compatibility of functions
}
\description{
Not all functions inside \code{glmdiag} are available for GLM extensions. The output table helps the user orientate inside the package in order to check which function can be used according to the model under analysis. 
}
\usage{
funComp(as.dataframe = T)
}
\arguments{
  \item{as.dataframe}{logical, if TRUE prints the table in data.frame format.}
}

\details{
Since the output table is constructed by hand, it's possible to visualize a not aligned table because of R settings of the user's computer. When this happens, set \code{as.dataframe} TRUE.
}


\author{
Giuseppe Reale
}

\examples{
funComp()
funComp(as.dataframe = TRUE)
}
