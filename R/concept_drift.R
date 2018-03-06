#' Naive Bayes concept drift detector from an AMIDST data stream
#' @note The function builds a dynamic naive Bayes model with a
#' Gaussian hidden variable which is aimed at capturing an underlying
#' unobserved process.
#' @references H. Borchani, A.M. Martinez, A.R. Masegosa, H.
#' Langseth, T.D. Nielsen, A. Salmeron, A. Fernandez, A.L.
#' Madsen, R.Saez (2015) Modeling concept drift: A probabilistic graphical
#' model based approach. IDA'2015. Lecture Notes in Computer Science 9385, 72-83.
#' @param input_stream an AMIDST input stream
#' @param class_index the index of the class variable in the list of variables
#' @param window_size the number of items in the stream to be analysed
#' simultaneously
#' @param transition_variance the variance of the transition distribution
#' @param hidden_vars the number of global hidden variables to include in the
#' model
#' @return the value of the hidden variables for each window
#' @examples
#' \dontrun{
#' data <- amidst_data_stream(system.file("extdata","sea.arff",
#' package="ramidst"))
#' results <- nb_concept_drift_detector_from_stream(data,class_index = -1L,
#' window_size=1000L,transition_variance=0.1,hidden_vars=1L)
#' re <- 0
#' for (k in 1:length(results)) re[k] <- results[[k]]
#' ymin = min(re)-0.05
#' ymax = max(re)+0.05
#' plot(re,type="l",ylim=c(ymin,ymax),ylab="Hidden variable",
#' xlab="Instance number (x 1000)")
#' abline(v=15,col="red")
#' abline(v=30,col="red")
#' abline(v=45,col="red")
#' }
#' @export

nb_concept_drift_detector_from_stream <- function(input_stream,
                                                  class_index = -1L,
                                                  window_size,
                                                  transition_variance=0.1,
                                                  hidden_vars=1L) {
  nb_detector <- .jnew("eu.amidst.core.conceptdrift.NaiveBayesVirtualConceptDriftDetector")

  nb_detector$setClassIndex(class_index)
  nb_detector$setData(input_stream)
  nb_detector$setWindowsSize(window_size)
  nb_detector$setTransitionVariance(as.double(transition_variance))
  nb_detector$setNumberOfGlobalVars(hidden_vars)

  nb_detector$initLearning()

  results <- list()
  iterable <- .jcall(input_stream,"Ljava/lang/Iterable;",
                     method="iterableOverBatches",window_size)
  iterator <- iterable$iterator()

  i <- 1
  repeat {
    if (iterator$hasNext()) {
      batch <- .jcall(iterator,"Ljava/lang/Object;",method="next")
    } else {
      break;
    }
    out <- nb_detector$updateModel(batch)
    results[[i]] <- out
    i <- i+1
  }
  return(results)
}
