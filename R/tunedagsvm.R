#### 20181212

#'@importFrom Rdpack reprompt
#'@import e1071
#'@import caret
#'@importFrom Hmisc %nin%
#'@import doParallel
#'@import foreach
#'@import parallel
NULL

#' Tunes a Directed Acyclic Graph-Support Vector Machine.
#'
#' \code{tunedagsvm} constructs based on a provided test and training sample as
#' returned by \code{\link{gettestandtrainingsampledf}} a directed acyclic graph-
#' support vector machine (DAG-SVM) \insertCite{Platt.2000}{TraceIdentification}
#' in order to discriminate the classes in the
#' provided test and training sample. This is done by implementing a simple grid
#' search as suggested by \insertCite{Chang.2001}{TraceIdentification}. The DAG-SVM is tuned for the follwing parameters:
#' \describe{
#'   \item{kernels:}{linear, sigmoidal, radial.}
#'   \item{\eqn{gamma}:}{\eqn{2^{-20}, 2^{-18}, …, 2^6} (not for the linear kernel)}
#'   \item{\eqn{C} (cost):}{
#'   \enumerate{
#'     \item In a first rough search step: \eqn{2^{-10}, 2^{-8}, …, 2^{10}}.
#'     \item In a following finer search step: \eqn{\text{C}_{\text{initial}} - 1.2, \text{C}_{\text{initial}} - 1.0,…, \text{C}_{\text{initial}}, \text{C}_{\text{initial}} + 1.0, \text{C}_{\text{initial}} + 1.2}
#'   }}
#' }
#' Tuning is done via random subset cross-validation. Tuning is done in parallel for each kernel.
#' Each node of the DAG-SVM is trained separately and the function provides performance
#' measures for the classification and a tuned model for each node and kernel.
#'
#' @param testandtrainingsample The provided test and training sample
#' (\code{data.frame} objetc) as returned by \code{\link{gettestandtrainingsampledf}}.
#' @param crossvalidationfolds A numeric value representing the number of folds (random subsets)
#' to use during cross-validation.
#' @return The function returns a list with two elements:
#' \describe{
#'   \item{\code{optimparam}}{A \code{data.frame} object containing for each node and
#'   kernel the tuned parameter values and various performance measures as returned by
#'   \code{\link[caret]{confusionMatrix}}.}
#'   \item{\code{svm_fit_list}}{A list with an element for each kernel and node of the
#'   constructed DAG-SVM. Each element represents the tuned SVM for each kernel and node.
#'   The elements are named as kernel_layer_node where kernel is the respective kernel,
#'   layer the index of the layer and node the index of the node in the layer.}
#' }
#' @seealso \code{\link{gettestandtrainingsampledf}}.
#' @source The function relies on the R packages \code{raster}
#' (\insertCite{Hijmans.2017}{TraceIdentification}), \code{e1071}
#' (\insertCite{Meyer.2015}{TraceIdentification}), \code{caret}
#' (\insertCite{Kuhn.2018}{TraceIdentification}), \code{Hmisc}
#' (\insertCite{HarrellJr.2018}{TraceIdentification}), \code{doParallel}
#' (\insertCite{Weston.2017}{TraceIdentification}) and \code{foreach}
#' (\insertCite{Weston.2017b}{TraceIdentification}).
#' @references
#'     \insertAllCited{}
#' @examples #
#' @export
tunedagsvm <- function(testandtrainingsample, crossvalidationfolds = 10){

  # get the unique classes
  classes <- unique(as.character(testandtrainingsample$class))

  # get needed number of layers in the DAGSVM
  numberoflayers <- length(classes) - 1

  # get needed number of nodes in the DAGSVM
  numberofnodes <- sum(seq_len(numberoflayers))

  # definition of the kernels and the needed SVM parameters
  hypergrid <- data.frame(kernel = c("linear", "polynomial", "radial", "sigmoid"),
                          gamma = c(0, 1, 1, 1),
                          degree = c(0, 1, 0, 0), stringsAsFactors = FALSE)
  hypergrid <- hypergrid[c(1, 3, 4),]

  # definition of the cost parameter for the rough analysis
  cost1 <- 2^(seq(from = -10, to = 10, by = 2))

  optimparam <- lapply(seq_len(numberoflayers), function(layer_i){

    # get the number of nodes for layer_i
    numbernodesoflayeri <- layer_i

    optimparam <- lapply(seq_len(numbernodesoflayeri), function(node_i){

      # get the classes in the input of node_i of layer_i and the classes node_i of layer_i will compare
      inputclasses <- getclassestocompare(classes, layer_i, node_i)[[1]]
      classestocompare <- getclassestocompare(classes, layer_i, node_i)[[2]]

      # get the sample data fro the classes that should be compared
      testandtrainingsamplelayerinodei <- testandtrainingsample[which(testandtrainingsample$class %in%  classestocompare),]
      testandtrainingsamplelayerinodei$class <- as.factor(as.character(testandtrainingsamplelayerinodei$class))

      # set up the cluster
      cl <- makeCluster(length(hypergrid$kernel), type = "PSOCK")
      registerDoParallel(cl)
      clusterExport(cl = cl, varlist = list("hypergrid", "node_i", "layer_i", "testandtrainingsamplelayerinodei",
                                            "classestocompare", "cost1", "crossvalidationfolds"), envir = environment())
      clusterCall(cl, fun = function(w){
        requireNamespace(e1071)
        requireNamespace(caret)
        requireNamespace(Hmisc)
      })

      # optimise the svm for each kernel
      optimparam <- foreach(kernel_i = seq_along(hypergrid$kernel))%dopar%{

        # definition of the kernel
        kernel <- hypergrid$kernel[kernel_i]

        # definition of the kernel parameters
        if(hypergrid$gamma[kernel_i] == 1){ # gamma
          m_gamma <- 2^(seq(from = -20, to = 6, by = 2))
        }else{
          m_gamma <- NULL
        }

        if(hypergrid$degree[kernel_i] == 1){ # degree
          m_degree <- c(1:4)
        }else{
          m_degree <- NULL
        }

        # fit the svm with the parameters
        svm_fit <- best.svm(class ~ r + g + b,
                            data = testandtrainingsamplelayerinodei,
                            gamma = m_gamma,
                            cost = cost1,
                            degree = m_degree,
                            kernel = kernel,
                            cross = crossvalidationfolds)

        # definition of the cost parameter for the finer analysis
        cost2 <- seq(from = svm_fit$cost - 1.2, to = svm_fit$cost + 1.2, by = 0.2)

        # delete all values in m_cost2 <= 0
        if(any(cost2 <= 0)){
          cost2 <- cost2[-which(cost2 <= 0)]
        }

        # fit the SVM with the parameters
        svm_fit <- best.svm(class ~ r + g + b,
                            data = testandtrainingsamplelayerinodei,
                            gamma = m_gamma,
                            cost = cost2,
                            degree = m_degree,
                            kernel = kernel,
                            cross = crossvalidationfolds)

        # save the best SVM model
        assign(paste("svm_fit_", kernel, "_", layer_i, "_", node_i, sep=""), svm_fit)

        # add the SVM model to the list of SVM models
        # svm.model.list[[paste("svm_fit_", kernel, "_", layer_i, "_", node_i, sep="")]] = svm_fit

        # get predicted class labels for the compared sample data
        svm_pred = predict(svm_fit, testandtrainingsamplelayerinodei[,c(1:3)])

        # calculate error matrix
        confusion_matrix = confusionMatrix(testandtrainingsamplelayerinodei$class, svm_pred) # Fehlermatrix

        # extract classification measures from the error matrix
        confusion_matrix1 = as.data.frame(confusion_matrix$byClass)

        # extract the performance values from the confusion matrix
        bestsvm_confusionmatrix_byclass <- as.data.frame(matrix(confusion_matrix$byClass, nrow = 1))
        colnames(bestsvm_confusionmatrix_byclass) <- names(confusion_matrix$byClass)
        colnames(bestsvm_confusionmatrix_byclass)[c(3, 4)] <- c("predvalue_class1", "predvalue_class2")

        bestsvm_confusionmatrix_overall <- as.data.frame(matrix(confusion_matrix$overall, nrow = 1))
        colnames(bestsvm_confusionmatrix_overall) <- names(confusion_matrix$overall)

        # return a data.frame object with performance measures of the tuned svm for node_i of layer_i
        bestsvm_performance <-
          data.frame(layer = layer_i,
                     node = node_i,
                     classestocompare = paste(classestocompare, collapse = "_"),
                     class1 = classestocompare[1],
                     class2 = classestocompare[2],
                     model = paste("svm_fit_", layer_i, "_", node_i, sep=""),
                     kernel = kernel,
                     cost = svm_fit$cost,
                     gamma = svm_fit$gamma,
                     degree = svm_fit$degree
          )
        optimparam <- list(optimparam = cbind(bestsvm_performance, bestsvm_confusionmatrix_overall, bestsvm_confusionmatrix_byclass), model = svm_fit)
        names(optimparam) <- rep(paste(as.character(kernel), layer_i, node_i, sep = "_"), 2)
        optimparam

      }

      stopCluster(cl)

      # extract the models from optimparam
      svm_fit_list <- lapply(optimparam, function(x) x[[2]])
      names(svm_fit_list) <- sapply(optimparam, function(x) names(x)[1])

      # extract the optimised parameters
      optimparam <- do.call(rbind, lapply(optimparam, function(x) x[[1]]))

      # return a list with optimparam and svm_fit_list
      optimparam <- list(optimparam, svm_fit_list)
      names(optimparam) <- rep(paste0("layer_", layer_i), length(optimparam))
      return(optimparam)

    })

    # extract the models from optimparam
    svm_fit_list <- unlist(lapply(optimparam, function(x) x[[2]]), recursive = FALSE)

    # extract the optimised parameters
    optimparam <- do.call(rbind, lapply(optimparam, function(x) x[[1]]))

    # return a list with optimparam and svm_fit_list
    optimparam <- list(optimparam, svm_fit_list)
    return(optimparam)

  })

  # extract the models from optimparam
  svm_fit_list <- unlist(lapply(optimparam, function(x) x[[2]]), recursive = FALSE)

  # extract the optimised parameters
  optimparam <- do.call(rbind, lapply(optimparam, function(x) x[[1]]))

  # return a list with optimparam and svm_fit_list
  optimparam <- list(optimparam, svm_fit_list)
  return(optimparam)

}
