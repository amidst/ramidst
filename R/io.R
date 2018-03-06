#' Loads an AMIDST BN from a fle
#' @param file_name the name of the file containing the network in .net format
#' @return a java object of class \code{BayesianNetwork}
#' @examples
#' network <- load_amidst_bn(system.file("extdata","WasteIncinerator.bn",
#' package="ramidst"))
#' print_amidst_bn(network)
#' @export
load_amidst_bn <- function(file_name) {
  new_network <- J("eu.amidst.core.io.BayesianNetworkLoader")$loadFromFile(file_name)
  return(new_network)
}

#' Loads an AMIDST dynamic BN from a fle
#' @param file_name the name of the file containing the network in .net format
#' @return a java object of class \code{DynamicBayesianNetwork}
#' @export
load_dynamic_amidst_bn <- function(file_name) {
  new_network <- J("eu.amidst.dynamic.io.DynamicBayesianNetworkLoader")$loadFromFile(file_name)
  return(new_network)
}

#' Prints an AMIDST model
#' @param network the network to print out.
#' @examples
#' network <- load_amidst_bn(system.file("extdata","WasteIncinerator.bn",
#' package="ramidst"))
#' print_amidst_bn(network)
#' @export
print_amidst_bn <- function(network) {
  cat(.jcall(network,"Ljava/lang/String;",method="toString"))
}


#' Loads an AMIDST dynamic data sequence from a fle
#' @param file_name the name of the file containing the data sequence
#' @return a Java object of class \code{DataStream}
#' @export
load_dynamic_sequence <- function(file_name) {
  sequence <- J("eu.amidst.dynamic.io.DynamicDataStreamLoader")$loadFromFile(file_name)
  ## Cast it to DataStream
  sequence <- .jcast(sequence,new.class="eu.amidst.core.datastream.DataStream")
  return(sequence)
}

#' Saves an AMIDST dynamic BN to a fle
#' @param network a java object of class \code{DynamicBayesianNetwork}
#' @param file_name the name of the file that will contain the network
#' @examples
#' network <- dbn_generator(1,2,2)
#' save_dynamic_amidst_bn(network,"sample_net.bn")
#' new_network <- load_dynamic_amidst_bn("sample_net.bn")
#' print_amidst_bn(network)
#' @export
save_dynamic_amidst_bn <- function(network,file_name) {
  J("eu.amidst.dynamic.io.DynamicBayesianNetworkWriter")$save(network,file_name)
}

