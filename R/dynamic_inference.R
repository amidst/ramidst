#' Runs belief update from a piece of dynamic evidence over a dynamic Bayesian
#' network
#' @note The function uses importance sampling and is based on the factored
#' frontier method, see the AMIDST toolbox documentation.
#' @param dyn_network a java object of class \code{DynamicBayesianNetwork} over which the
#' computations will be carried out
#' @param target_variable the name of the variable over which the
#' posterior distribution will be computed
#' @param dyn_evidence the observations over the dynamic Bayesian network.
#' @param sample_size the size of the sample used to estimate the posterior
#' distribution.
#' @return a \code{data.frame} with the posterior distribution over the target
#' variable on the different time slices.
#' @examples
#' network <- dbn_generator(1,2,2)
#' print_amidst_bn(network)
#' stream <- generate_stream_from_dbn(network,1,10,"ClassVar")
#' resultsIS <- dynamic_importance_sampling(network,"ClassVar",stream)
#' plot(resultsIS[,2],type="l",ylim = c(0,1),col="red",xlab="Time slice",ylab="Prob. ClassVar = 1")
#' @export
dynamic_importance_sampling <- function(dyn_network,target_variable,dyn_evidence,
                                        sample_size=50) {
  variables <- .jcall(dyn_network,"Leu/amidst/dynamic/variables/DynamicVariables;",
                      method="getDynamicVariables")
  var <- .jcall(variables,"Leu/amidst/core/variables/Variable;",
                method="getVariableByName",target_variable)

  number_of_values <- var$getNumberOfStates()
  results <- data.frame(matrix(1:number_of_values,nrow=1))
  colnames(results)<- paste("Value",1:number_of_values,sep=" ")

  iterator <- dyn_evidence$iterator()
  importance_sampling <- .jnew("eu.amidst.core.inference.ImportanceSampling")
  .jcall(importance_sampling,"V",method="setKeepDataOnMemory",TRUE)
  importance_sampling <- .jcast(importance_sampling,new.class = "eu.amidst.core.inference.InferenceAlgorithm")

  ff_algorithm <- .jnew("eu.amidst.dynamic.inference.FactoredFrontierForDBN",
                        importance_sampling)
  J("eu.amidst.dynamic.inference.InferenceEngineForDBN")$setInferenceAlgorithmForDBN(ff_algorithm)


  #Then, we set the DBN model
  J("eu.amidst.dynamic.inference.InferenceEngineForDBN")$setModel(dyn_network)

  time <- 0;
  posterior = NULL

  item <- 1
  repeat {
    if (.jcall(iterator,"Z",method="hasNext")) {
      instance <- .jcall(iterator,"Ljava/lang/Object;",method="next")
    } else {
      break;
    }
    time_id <- .jcall(instance,"J",method="getTimeID")
    if ((time_id == 0) && (!is.null(posterior))) {
      J("eu.amidst.dynamic.inference.InferenceEngineForDBN")$reset()
      time <- 0
    }

    #We also set the evidence.
    J("eu.amidst.dynamic.inference.InferenceEngineForDBN")$addDynamicEvidence(instance)

    #Then we run inference
    J("eu.amidst.dynamic.inference.InferenceEngineForDBN")$runInference()

    #Then we query the posterior of the target variable
    posterior <- J("eu.amidst.dynamic.inference.InferenceEngineForDBN")$getFilteredPosterior(var)

    results[item,] <- matrix(.jcall(posterior,"[D",method="getParameters"),nrow=1)
    item <- item + 1
    time <- time + 1
  }
  return(results)
}
