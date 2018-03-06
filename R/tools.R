#' @import rJava
.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname, lib.loc=libname)
}

.onLoad("ramidst", "../lib/AMIDST-kernel-reduced-0.6.0.jar")


#' Generates a random dynamic Bayesian network
#' @param n_c_vars the number of Gaussian variables
#' @param n_d_vars the number of discrete variables
#' @param n_s the number of states of the discrete variables
#' @return a Java object of class \code{DynamicBayesianNetwork}
#' @examples
#' d <- dbn_generator(3,2,2)
#' print_amidst_bn(d)
#' @export
dbn_generator <- function(n_c_vars,n_d_vars,n_s) {
  rnd <- .jnew("java.util.Random")
  J("eu.amidst.dynamic.utils.DynamicBayesianNetworkGenerator")$setNumberOfContinuousVars(as.integer(n_c_vars))
  J("eu.amidst.dynamic.utils.DynamicBayesianNetworkGenerator")$setNumberOfDiscreteVars(as.integer(n_d_vars))
  J("eu.amidst.dynamic.utils.DynamicBayesianNetworkGenerator")$setNumberOfStates(as.integer(n_s))
  parameter <- TRUE
  x <- 2
  network <- J("eu.amidst.dynamic.utils.DynamicBayesianNetworkGenerator")$generateDynamicNaiveBayes(rnd, 2L, TRUE)
  return(network)
}




