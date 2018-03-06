#' Runs Importance sampling evidence updating from an AMIDST data stream
#' @note The function computes the posterior distribution given some evidence
#' for all the items in the input stream.
#' @references A. Salmeron, D. Ramos-Lopez, H. Borchani, A.M.
#' Martinez, A.R. Masegosa, A. Fernandez, H. Langseth, A.L.
#' Madsen, T.D. Nielsen (2015) Parallel importance sampling in conditional
#' linear Gaussian networks. CAEPIA'2015. Lecture Notes in Artificial
#' Intelligence 9422, 36-46.
#' @param network a java object of class \code{BayesianNetwork} over which the
#' computations will be carried out
#' @param target_variable a string representing
#' the name of variable whose posterior distribution will be computed
#' @param evidence_variables a vector with the names of the observed variables
#' @param input_stream and AMIDST data stream
#' @param sample_size the sample size to be used during the simulation
#' @param parallel a \code{boolean} indicating whether or not the items in the
#' sample will be generated in parallel (when allowed by the system)
#' @param seed the seed for the genertion of random numbers
#' @return a \code{data.frame} with the posterior distribution of the target
#' variable for each item in the strea
#' @examples
#' network <- load_amidst_bn(system.file("extdata","WasteIncinerator.bn",
#' package="ramidst"))
#' sample_stream <- amidst_data_stream(system.file("extdata",
#' "WasteIncineratorSample.arff",package="ramidst"))
#' posterior <- importance_sampling_from_stream(network,"B",c("F","E"),
#' sample_stream,50L)
#' posterior
#' posterior <- importance_sampling_from_stream(network,"L",c("F","E"),
#' sample_stream,50L)
#' posterior
#' @export
importance_sampling_from_stream <- function(network,target_variable,
                                            evidence_variables,input_stream,
                                            sample_size,parallel=T,seed=3L) {
  ## Initializing the stream
  iterator <- amidst_ds_iterator(input_stream)

  variables <- .jcall(network,"Leu/amidst/core/variables/Variables;",
                      method="getVariables")
  target_var <- .jcall(variables,"Leu/amidst/core/variables/Variable;",
                      method="getVariableByName",target_variable)
  number_of_values <- .jcall(target_var,"I",method="getNumberOfStates")


  if (number_of_values == -1) {
    results <- data.frame(matrix(c(0.0,0.0),nrow=1))
    colnames(results)<-c("Mean","Sigma")
  } else {
    results <- data.frame(matrix(1:number_of_values,nrow=1))
    colnames(results)<- paste("Value",1:number_of_values,sep=" ")
  }

  item = 1

  repeat {
    if (.jcall(iterator,"Z",method="hasNext")) {
      y <- .jcall(iterator,"Ljava/lang/Object;",method="next")
    } else {
      break;
    }

    n_ev_vars <- length(evidence_variables)
    evidence <- new(J("eu.amidst.core.variables.HashMapAssignment"),
                    as.integer(n_ev_vars))
    for (i in 1:n_ev_vars) {
      y_attr <- .jcall(y,"Leu/amidst/core/datastream/Attributes;",
                      method="getAttributes")
      var <- .jcall(y_attr,"Leu/amidst/core/datastream/Attribute;",
                    method="getAttributeByName",evidence_variables[i])
      value <- .jcall(y,"D",method="getValue",var)
      name <- .jcall(var,"Ljava/lang/String;",method="getName")
      var2 <- .jcall(variables,"Leu/amidst/core/variables/Variable;",
                     method="getVariableByName",name)
      .jcall(evidence,"V",method="setValue",var2,value)
    }

    alg <- .jnew("eu.amidst.core.inference.ImportanceSampling")
    .jcall(alg,"V",method="setModel",network)
    .jcall(alg,"V",method="setParallelMode",parallel)
    .jcall(alg,"V",method="setSeed",seed)
    .jcall(alg,"V",method="setSampleSize",as.integer(sample_size))
    .jcall(alg,"V",method="setEvidence",
           .jcast(evidence,"eu/amidst/core/variables/Assignment"))
    .jcall(alg,"V",method="runInference")
    poste <- .jcall(alg,"Leu/amidst/core/distribution/UnivariateDistribution;",
                    method="getPosterior",target_var)
    results[item,] <- matrix(.jcall(poste,"[D",method="getParameters"),nrow=1)
    item <- item + 1
  }
  return(results)
}


#' Runs MAP inference from an AMIDST data stream
#' @note The function computes the MAP configuration of the variables of
#' interest given some evidence for all the items in the input stream.
#' @references D. Ramos-Lopez, A. Salmeron, R. Rumi, A.M.
#' Martinez, T.D. Nielsen, A.R. Masegosa, H. Langseth, A.L.
#' Madsen (2016) Scalable MAP inference in Bayesian networks based on a
#' Map-Reduce approach. PGM'2016. JMLR: Workshop and Conference Proceedings,
#' vol. 52: 415-425.
#' @param network a java object of class \code{BayesianNetwork} over which the
#' computations will be carried out
#' @param map_variables a vector with the name of the variables over which the
#' MAP configuration will be computed
#' @param evidence_variables a vector with the names of the observed variables
#' @param input_stream and AMIDST data stream
#' @param sample_size the sample size to be used for estimating marginals
#' @param parallel a \code{boolean} indicating whether or not the items in the
#' sample will be generated in parallel (when allowed by the system)
#' @param seed the seed for the genertion of random numbers
#' @return a \code{data.frame} with the MAP configuration of the
#' variables of interest for each item in the stream
#' @examples
#' \dontrun{
#' network <- load_amidst_bn(system.file("extdata","WasteIncinerator.bn",
#' package="ramidst"))
#' sample_stream <- amidst_data_stream(system.file("extdata",
#' "WasteIncineratorSample.arff",package="ramidst"))
#' map_configurations <- map_inference_from_stream(network,c("D","B"),c("W"),
#' sample_stream,5L)
#' map_configurations
#' }
#' @export
map_inference_from_stream <- function(network,map_variables,evidence_variables,
                                      input_stream,sample_size,parallel=T,
                                      seed=3L) {
  ## Initializing the stream
  iterator <- amidst_ds_iterator(input_stream)

  variables <- network$getVariables()

  vars_interest <- new(J("java.util.ArrayList"))
  for (i in 1:length(map_variables)) {
    name_var <- map_variables[i]
    map_variable <- network$getVariables()$getVariableByName(name_var)
    vars_interest$add(map_variable)
  }
  results <- data.frame(Configuration="A",LogProb=0.0, stringsAsFactors=FALSE)
  item = 1
  repeat {
    if (iterator$hasNext()) {
      y <- .jcall(iterator,"Ljava/lang/Object;",method="next")
    } else {
      break;
    }

    n_ev_vars <- length(evidence_variables)
    evidence <- new(J("eu.amidst.core.variables.HashMapAssignment"),as.integer(n_ev_vars))
    for (i in 1:n_ev_vars) {
      var <- y$getAttributes()$getAttributeByName(evidence_variables[i])
      value <- y$getValue(var)
      var2 <- variables$getVariableByName(var$getName())
      evidence$setValue(var2,value)
    }

    alg <- .jnew("eu.amidst.core.inference.MAPInference")
    alg$setParallelMode(parallel) ## Use multicore in Java or not
    alg$setSeed(seed)
    alg$setModel(network)
    alg$setSampleSize(sample_size)
    alg$setEvidence(evidence)
    alg$setMAPVariables(vars_interest)
    alg$runInference()
    map_conf <- alg$getEstimate()
    map_prob <- alg$getLogProbabilityOfEstimate()

    results[item,1] <- map_conf$outputString()
    results[item,2] <- map_prob
    item <- item + 1
  }
  return(results)
}


#' Runs MPE inference from an AMIDST data stream
#' @note The function computes the most probable explanation
#' of the evidence in all the items in the input stream.
#' @references D. Ramos-Lopez, A. Salmeron, R. Rumi, A.M.
#' Martinez, T.D. Nielsen, A.R. Masegosa, H. Langseth, A.L.
#' Madsen (2016) Scalable MAP inference in Bayesian networks based on a
#' Map-Reduce approach. PGM'2016. JMLR: Workshop and Conference Proceedings,
#' vol. 52: 415-425.
#' @param network a java object of class \code{BayesianNetwork} over which the
#' computations will be carried out
#' @param evidence_variables a vector with the names of the observed variables
#' @param input_stream and AMIDST data stream
#' @param parallel a \code{boolean} indicating whether or not the items in the
#' sample will be generated in parallel (when allowed by the system)
#' @param seed the seed for the genertion of random numbers
#' @return a \code{data.frame} with the MPE configuration
#' for each item in the stream
#' @examples
#' \dontrun{
#' network <- load_amidst_bn(system.file("extdata","WasteIncinerator.bn",
#' package="ramidst"))
#' sample_stream <- amidst_data_stream(system.file("extdata",
#' "WasteIncineratorSample.arff",package="ramidst"))
#' mpe_configurations <- mpe_inference_from_stream(network,c("E"),sample_stream)
#' mpe_configurations
#' }
#' @export
mpe_inference_from_stream <- function(network,evidence_variables,input_stream,
                                      parallel=T,seed=3L) {
  ## Initializing the stream
  iterator <- amidst_ds_iterator(input_stream)

  results <- data.frame(Configuration="A",LogProb=0.0, stringsAsFactors=FALSE)

  item = 1

  repeat {
    if (iterator$hasNext()) {
      y <- .jcall(iterator,"Ljava/lang/Object;",method="next")
    } else {
      break;
    }

    n_ev_vars <- length(evidence_variables)
    evidence <- new(J("eu.amidst.core.variables.HashMapAssignment"),as.integer(n_ev_vars))
    for (i in 1:n_ev_vars) {
      var <- y$getAttributes()$getAttributeByName(evidence_variables[i])
      value <- y$getValue(var)
      var2 <- network$getVariables()$getVariableByName(var$getName())
      evidence$setValue(var2,value)
    }

    alg <- .jnew("eu.amidst.core.inference.MPEInference")
    alg$setParallelMode(parallel) ## Use multicore in Java or not
    alg$setSeed(seed)
    alg$setModel(network)
    alg$setSampleSize(1000L)
    alg$setEvidence(evidence)
    alg$runInference()
    map_conf <- alg$getEstimate()
    map_prob <- alg$getLogProbabilityOfEstimate()

    results[item,1] <- map_conf$outputString()
    results[item,2] <- map_prob
    item <- item + 1
  }
  return(results)
}
