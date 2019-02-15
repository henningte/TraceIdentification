#### 20181212

#'@importFrom Rdpack reprompt
NULL

#' Identifies the Class Pair that is Discriminated by a Specific Node of a DAG-SVM.
#'
#' \code{getclassestocompare} determines for a set of classes and a specific node of a
#' directed acyclic graph-support vector machine (DAG-SVM) (\insertCite{Platt.2000}{TraceIdentification})
#' the pair of classes that is
#' compared. This function is designed as internal function of \code{\link{tunedagsvm}}.
#'
#' @param classes A character vector with the class labels for each class.
#' @param layer_i A numeric value representing the index of the layer.
#' @param node_i A numeric value representing the index of the node.
#' @return The function returns a character vector with the first element representing
#' the positive clas and the second element the negative class the SVM of \code{node_i}
#' of \code{layer_i} distingiushes.
#' @seealso \code{\link{tunedagsvm}}.
#' @references
#' \insertAllCited{}
#' @examples #
#' @export
getclassestocompare <- function(classes, layer_i, node_i){

  # at each layer, one class is removed from the class vector
  # get the number of classes that were removed until node_i of layer_i
  numberofremovedclasses <- layer_i -1

  # get the number of classes that will be removed
  numberofclassestoremove <- 1

  # get the number of classes that will be removed from the head of the vector
  numberofremovedclasses_head <- numberofremovedclasses - (node_i - 1)

  # get the number of classes that will be removed from the tail of the vector
  numberofremovedclasses_tail <- node_i - 1

  # create the class vector that represents the input of node_i in layer_i
  indexremovedclasses <- seq_along(classes)
  if(numberofremovedclasses_head != 0){
    indexremovedclasses <- indexremovedclasses[-seq_len(numberofremovedclasses_head)]
  }
  if(numberofremovedclasses_tail != 0){
    indexremovedclasses <- rev(rev(indexremovedclasses)[-seq_len(numberofremovedclasses_tail)])
  }
  removedclasses <- classes[indexremovedclasses]

  # get the class that will be compared
  classestocompare <- c(removedclasses[1], removedclasses[length(removedclasses)])

  # return a list with the deleted classes as first element and the classes to compare as second element
  list(removedclasses = removedclasses, classestocompare = classestocompare)

}
