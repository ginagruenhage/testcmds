\name{plot.cmds}
\alias{plot.cmds}
\title{Plot cmds results}
\usage{
  plot.cmds(res, convergence = FALSE, shepard = FALSE)
}
\arguments{
  \item{res}{The results from running \code{cmds}.}

  \item{convergence}{If set to TRUE, a trace of cost values
  is plotted.}

  \item{shepard}{If set to TRUE, a shapard plot is
  created.}
}
\description{
  This function gives a basic visualization of cmds
  results.
}
\examples{
plot.cmds(cmds(QuadCurves))
plot.cmds(cmds(QuadCurves), convergence = TRUE)
plot.cmds(cmds(QuadCurves), shepard = TRUE)
plot.cmds(cmds(ExpandingTriangle, k = 2))
}

