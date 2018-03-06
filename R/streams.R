#' Creates an AMIDST data stream from an arff file
#' @param file_name the name of the data file in arff format
#' @return a Java object of class \code{DataStream}
#' @export
amidst_data_stream <- function(file_name) {
  reader <- new(J("eu.amidst.core.datastream.filereaders.arffFileReader.ARFFDataReader"))
  .jcall(reader,"V",method="loadFromFile",file_name)
  data_on_disk_reader <- new(J("eu.amidst.core.datastream.filereaders.DataStreamFromFile"),
                             reader)
  return(data_on_disk_reader)
}

#' Creates an iterator over an AMIDST data stream
#' @param data_on_disk_reader an AMIDST data stream
#' @return an iterator over the stream
#' @export
amidst_ds_iterator <- function(data_on_disk_reader) {
  data_on_disk_iterator <- data_on_disk_reader$iterator()
  return(data_on_disk_iterator)
}


#' Generates a stream from a DBN.
#' The DBN is assumed to be a classifier,
#' and therefore there is a class variable.
#' @param net the network that will be sampled
#' @param sequences the number of random sequences to generate. Each one will
#' be an element of the stream.
#' @param seq_length the length of the sequences
#' @param class_var the class variable
#' @return a Java object of class DataStream with the generated sequences
#' @export
generate_stream_from_dbn <- function(net,sequences,seq_length,class_var) {
  variables <- .jcall(net,"Leu/amidst/dynamic/variables/DynamicVariables;",
                      method="getDynamicVariables")
  var <- .jcall(variables,"Leu/amidst/core/variables/Variable;",
                method="getVariableByName",class_var)
  sampler <- .jnew("eu.amidst.dynamic.utils.DynamicBayesianNetworkSampler",net)
  .jcall(sampler,"V",method="setHiddenVar",var)
  stream <- .jcall(sampler,"Leu/amidst/core/datastream/DataStream;",
                   method="sampleToDataBase",as.integer(sequences),
                   as.integer(seq_length))
  return(stream)
}


#' Generates a stream where each item is a full sequence from some
#' dynamic evidence.
#' @param dynamic_evidence some dynamic observation over s DBN
#' @return a data stream out of the input evidence
#' @export
generate_stream_of_sequences <- function(dynamic_evidence) {
  iterator <- dynamic_evidence$iterator()

  final_stream <- .jnew("java.util.ArrayList")

  if (.jcall(iterator,"Z",method="hasNext")) {
    instance <- .jcall(iterator,"Ljava/lang/Object;",method="next")
  } else {
    break;
  }
  current_sequence_id <- instance$getSequenceID()
  current_list <- .jnew("java.util.ArrayList")
  current_list$add(instance)
  repeat {
    if (.jcall(iterator,"Z",method="hasNext")) {
      instance <- .jcall(iterator,"Ljava/lang/Object;",method="next")
    } else {
      break;
    }
    this_instance_id <- instance$getSequenceID()
    if (this_instance_id != current_sequence_id) {
      final_stream$add(current_list)
      current_list <- .jnew("java.util.ArrayList")
      current_sequence_id <- this_instance_id
    }
    current_list$add(instance)
  }

  return(final_stream)
}
