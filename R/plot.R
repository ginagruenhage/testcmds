#' Plot cmds results
#'
#' This function gives a basic visualization of cmds results.
#'
#' @param res The results from running \code{cmds}.
#' @param convergence If set to TRUE, a trace of cost values is plotted.
#' @param shepard If set to TRUE, a shapard plot is created.
#' @export
#' @examples
#' plot.cmds(cmds(QuadCurves))
#' plot.cmds(cmds(QuadCurves), convergence = TRUE)
#' plot.cmds(cmds(QuadCurves), shepard = TRUE)
#' plot.cmds(cmds(ExpandingTriangle, k = 2))

plot.cmds <- function(res, convergence = FALSE, shepard = FALSE) {
  DL <- res$DL
  XL <- res$XL
  params <- res$params
  DL.X <- Dist.List(XL,params)
  D <- params$D
  T <- params$T
  
  if (D == 1) {
    par(mfrow=c(1,1))
    lim <- max(laply(DL,max))
    plot(1:T, 1:T, type="n", ylim = c(-lim, lim), xlab="alpha", ylab="Dimension 1")

    apply(sapply(center.conf(XL),function(v) v[1,]),1,function(v) lines(v))
  } else {
    par(mfrow = c(ceiling(T/3),3))
    lim <- max(laply(DL,max))
    
    for (i in seq(1,T)){
      plot(0, 0, type="n", ylim = c(-lim, lim), xlim = c(-lim, lim), xlab = "Dimension 1",ylab = "Dimension 2")
      par(pty = "s")
      XLC <- center.conf(XL)
      points(XLC[[i]][1,], XLC[[i]][2,])
    }
  }
  
  if (convergence){
    plot.new()
    par(mfrow=c(1,2))
    
    plot(res$con$trace$iter, res$con$trace$C, xlab = "Iteration", ylab = "Cost")
    plot(res$con$trace$iter[-1], res$con$trace$C[-1], xlab = "Iteration", ylab = "Cost")
  }

  if (shepard){
    par(mfrow = c(ceiling(T/3),3))
    lim <- max(laply(DL,max))
    
    for (i in seq(1,T)){
      plot(0, 0, type="n", ylim = c(0,lim), xlim = c(0,lim), xlab = "Original Distances", ylab = "Embedding Distances")
      par(pty = "s")
      abline(0,1)
      points(a.n(DL[[i]]), a.n(DL.X[[i]]))
    }
  }
    
  
}
  
