#### 20181212

#'@importFrom Rdpack reprompt
NULL

#' Extracts the DAG-SVM of a set of tuned DAG-SVM with the Best Classification Accuracy.
#'
#' \code{tunedagsvm_extractoptimisedparametersandmodels} takes the output of
#' \code{\link{tunedagsvm}}, i.e. a set of tuned directed acyclic graph-support vector
#' machines (DAG-SVM) \insertCite{Platt.2000}{TraceIdentification} for a specific
#' classification task and returns for each node the SVM
#' that had the maximum overall classification accuracy when running \code{\link{tunedagsvm}}.
#'
#' @param optimparam A list as returned by \code{\link{tunedagsvm}} containing performance
#' parameters and tuned DAG-SVM.
#' @param kernel A character value indicating a specific kernel for which to extract
#' the tuned SVM for each node of the DAG-SVM. If \code{kernel = NULL},
#' \code{tunedagsvm_extractoptimisedparametersandmodels} select for each node the kernel
#' with the maximum overall classification accuracy. \code{kernel} is intended to provide the
#' possibility to select a specific desired kernel for a DAG-SVM.
#' @return The function returns a list with two elements:
#' \describe{
#'   \item{\code{prunedoptimparam}}{A \code{data.frame} object containing for each node the
#'   respective selected row of the first element of \code{optimparam}.}
#'   \item{\code{prunedbestmodels}}{A list with an element for layer containing the SVM for all
#'   nodes of this layer. Each element of \code{prunedbestmodels} is a list of the respective SVM.}
#' }
#' @seealso \code{\link{tunedagsvm}}.
#' @references
#' \insertAllCited{}
#' @examples #
#' @export
tunedagsvm_extractoptimisedparametersandmodels <- function(optimparam,
                                                           kernel = "radial"){

  # get the unique layers
  layers <- unique(optimparam[[1]]$layer)

  # get the nodes of each layer
  nodesoflayers <- tapply(optimparam[[1]]$node, optimparam[[1]]$layer, unique, simplify = FALSE)

  # extract for each layer and node the svm with the highest overall classification accuracy
  prunedoptimparam <- do.call(rbind, lapply(seq_along(nodesoflayers), function(layer_i){

    do.call(rbind, lapply(nodesoflayers[[layer_i]], function(node_i){

      if(is.null(kernel)){
        optimparam[[1]][which.min(optimparam[[1]]$Accuracy[optimparam[[1]]$layer == layer_i & optimparam[[1]]$node == node_i]),]
      }else{
        optimparam[[1]][optimparam[[1]]$kernel[optimparam[[1]]$layer == layer_i & optimparam[[1]]$node == node_i] == kernel,]
      }

    }))

  }))

  # extract the respective models
  prunedoptimparam_names <- apply(prunedoptimparam, 1, function(x){
    paste(x[7], x[1], x[2], sep = "_")
  })

  # extract all svm models with matching names
  prunedbestmodels <- optimparam[[2]][names(optimparam[[2]]) %in% prunedoptimparam_names]

  # create a list mimiking the dag-svm structure
  counter <- 1
  prunedbestmodelsnew <- list()
  while(counter != length(layers) + 1){

    prunedbestmodelsnew[[counter]] <- prunedbestmodels[counter]
    counter <- counter + 1
  }

  # return prunedoptimparam and prunedbestmodelsnew
  list(optimparam = prunedoptimparam, prunedbestmodels = prunedbestmodelsnew)
}
