#' View a summary of cmds results
#'
#' \code{summary} prints a summary of cmds results and statistics.
#'
#' After running \code{cmds}, the summary function prints information such as whether the algorithm converged, the embedding dimension, the distortion and total error of the embedding, the runtime, the weights that were used by the algorithm. It also prints the value of optional parameters.
#'
#' @param res The results from running \code{cmds}.
#'
#' @export

summary.cmds <- function(res) {

  DL <- res$DL
  XL <- res$XL
  params <- res$params
  con <- res$con

  ans <- list()
  
  ans$Error <- C.L(DL,XL,params) / params$N
  ans$Distortion <- L.L(DL,XL,params) / params$N
  ans$Penalty <- ans$Error-ans$Distortion
  
  ## if ((con$delta >= 0) & con$delta < params$eps) {
  ##   ans$Convergence <- cat("The algorithm converged. (delta = ", con$delta,")\n")
  ## } else if (con$delta < 0 & abs(con$delta) < 1e-3) {
  ##   ans$Convergence <- cat("The algorithm converged with small increase. (delta = ", con$delta,", relative difference = ",con$rel.diff,")\n")
  ## } else {
  ##   ans$Convergence <- cat("The algorithm did not converge. (delta = ", con$delta, ", relative difference = ", con$rel.diff,")\n")
  ## }

  ## con <- adply(seq(1,dim(con$trace)[1]), 1, function(i){
  ##   if (i==1) con$trace$rate <- Inf
  ##   else con$trace$rate <- (con$trace[i,]$C - con$trace[i-1,]$C)/params$N
  ##   con
  ## })
  ## browser()
               
  ans
  

  
  
}
