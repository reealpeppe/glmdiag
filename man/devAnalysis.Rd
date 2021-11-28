\name{devAnalysis}
\alias{devAnalysis}

\title{
Graphical analysis of Deviance
}
\description{
A graphical tool developed by Brown (1992) to assess the contribution of one or more of the explanatory variables inside a Generalized Linear Model. This plot displays all single variable comparisons and shows how they relate to other terms that are already in the model in terms of deviance. The output plot is divided into three frames, see Details.
}
\usage{
devAnalysis(model, cex.pl = .8, cex.vars = 1, cex.vars2 = 1, 
            layout.heights = c(1.2,0.8,4), pl.scale = 1,
            xlab, ylab, title)}
\arguments{
  \item{model}{a model supported by \code{glmdiag}.}
  \item{cex.pl}{dimension of points of linear predictors.}
  \item{cex.vars}{dimension of variables names inside lower frame.}
  \item{cex.vars2}{dimension of variables names inside medium frame.}
  \item{layout.heights}{a vector with three heights for the frames, from upper to lower.}
  \item{pl.scale}{proportion given to the sub-frame of the lower frame indicating the linear predictors.}
  \item{xlab}{a title for the x axis of lower frame.}
  \item{ylab}{a title for the y axis of lower frame.}
  \item{title}{an overall title for the plot.}

}

\details{
In order to give a clear explanation of the plot, these will be given referred to the output plot of example code.
The plot is divide into three frames: upped, medium and lower; we will start by explaining the lower one.

On the y axis we have the residual deviance and each horizontal line corresponds to the (residual) deviance of a particular fitted model. 
On the right side of each horizontal line there are plus signs: based on the column they're in, they indicate if that specific variable is included in the linear predictor. For example, looking at the first horizontal line (right under the names diam, mass, dist), there are no plus signs on the adjacent columns: that is the null model.
The line under the null model has a plus sign in correspondece of dist, i.e. only variable distance is included within that model.

The differences in deviance corresponding to the addition of a term to the various possible models (i.e. models not containing the term in question) are represented by vertical lines connecting the two appropriate horizontal model lines. For example the first vertical line connects the null model to the one that has just diam in the linear predictor. 
Note that this frame is divided into blocks and each one contains the differences in deviance (i.e. vertical lines) corresponding to addition of a specific variable; in this case there are three blocks for three variables.

The upper frame is just a summary of the lower one; the vertical lines are the same but in a reduced scale.
The medium frame indicates the linear predictors of a model before adding the block-specific variable. For example the are no plus signs under the first vertical line; this means that it corresponds to the change in deviance when diam is added to the null model (because we are in the diam block as in the lower frame).
The second line corresponds to the difference in deviance in a model containing only mass due to adding diam, and so on.

This graph is really useful to investigate the relationship between variable inside a model. In this case we may notice that adding mass is relevant when there are no variables or only dist; on the other hand its contribution is almost imperceptible when diam is already included within linear predictor. 

If the description of arguments \code{layout.heights} and \code{pl.scale} is not clear, the best way to understand their role is to run the code a few times changing these parameters: this will do it.

The best advice is to use this function with a maximum of 4-5 variables or else the plot will result confusing.
}

\author{
Giuseppe Reale
}

\references{
Brown, D. (1992). A Graphical Analysis of Deviance. Journal of the Royal Statistical Society. Series C (Applied Statistics), 41(1), 55-62. 

M. Sciandra & A. Plaia (2018) A graphical model selection tool for mixed models, Communications in Statistics - Simulation and Computation, 47:9, 2624-2638

}

\examples{
data(moons)
dist <- moons$Distance
diam <- moons$Diameter
mass <- moons$Mass
mod <- glm(moons$Moons ~ diam + mass + dist, family = poisson)
devAnalysis(mod, cex.vars = 1.3, cex.vars2 = 1.1)

}
