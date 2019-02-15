#### 20181212

#'@importFrom Rdpack reprompt
#'@importFrom raster brick
NULL

#' Creates Random Rasters of Traces of a Specific Class.
#'
#' \code{gettestandtrainingsampledf} creates one \code{data.frame} object for
#' a set of test and training samples that can be used in combination with
#' \code{\link{tunedagsvm}} in order to tune a support vector machine (SVM)
#' in order to classify values into classes. For each class, there must
#' exist a an individual image file in a separate directory without any other
#' files. NULL values must have the rgb values \code{c(255,255,255)}, i.e.
#' filled with white color. It is possible to subsample the provided samples
#' and to merge classes.
#'
#' @param directorydata A character string representing the directory where
#' the image files of the test and training samples are stored. No other files
#' must be in this directory.
#' @param classes A character vector with the class labels to use for each class.
#' The order has to match the order of the files when assessed via
#' \code{\link[base]{list.files}}. The number of elements of \code{classes} has to match
#' the number of files in \code{directorydata}.
#' @param subsamplesize A numeric vector with the sizes of the subsamples to collect
#' for each class. The order has to match the order in  \code{classes}. The number of
#' elements of \code{subsamplesize} has to match the number of files in \code{directorydata}.
#' The order has to match the order of the files when assessed via
#' \code{\link[base]{list.files}}. Alternatively, \code{subsamplesize}
#' can be set to \code{NULL} if no subsampling should be performed.
#' @param classestomerge A list with a vector for each merge of two classes.
#' Each vector must be a character vector with two elements correspnding to two
#' elements of \code{classes}. The first element represents the class label to
#' retain whereas the class label of elements correspnding to the second element
#' will be changed into the first element of the vector.
#' @return The function returns a \code{data.frame} object with four columns:
#' \describe{
#'   \item{\code{r}}{The values of the red band of the test and training samples.}
#'   \item{\code{g}}{The values of the greeb band of the test and training samples.}
#'   \item{\code{b}}{The values of the blue band of the test and training samples.}
#'   \item{\code{b}}{The corresponding class labels as factor.}
#' }
#' @seealso \code{\link{tunedagsvm}}.
#' @source The function relies on the R packages \code{raster}
#' (\insertCite{Hijmans.2017}{TraceIdentification}).
#' @references
#' \insertAllCited{}
#' @examples #
#' @export
gettestandtrainingsampledf <- function(directorydata,
                                       classes,
                                       subsamplesize = NULL,
                                       classestomerge = NULL){

  # get the names of the files containing the test and training samples
  filenamestestandtrainingsamples <- list.files(directorydata, full.names = TRUE)

  # load the image data for each class, extract the values, perform subsampling and merge of classes
  testandtrainingsample <-
    do.call(rbind, lapply(seq_along(classes), function(class_i){

      # import the file
      testandtrainingsample <- brick(filenamestestandtrainingsamples[[class_i]])

      # create a data.frame object with band values and class labels
      testandtrainingsample <- data.frame(r = getValues(testandtrainingsample[[1]]),
                                          g = getValues(testandtrainingsample[[2]]),
                                          b = getValues(testandtrainingsample[[3]]))

      # delete NULL values (white pixels)
      testandtrainingsampl <- testandtrainingsample[which(testandtrainingsample$r < 254 &
                                                            testandtrainingsample$g < 254 &
                                                            testandtrainingsample$b < 254),]

      # add class labels to individual values of testandtrainingsample
      testandtrainingsample$class = classes[class_i]

      # subsampling of values
      if(!is.null(subsamplesize)){
        # perform resampling
        if(nrow(testandtrainingsample) > subsamplesize[class_i]){
          testandtrainingsample <- testandtrainingsample[sample(seq_len(nrow(testandtrainingsample)), size = subsamplesize[class_i]),]
        }
      }

    }))

  # merge classes
  if(!is.null(classestomerge)){
    for(merge_i in seq_along(classestomerge)){
      testandtrainingsample$class[which(testandtrainingsample$class == classestomerge[[merge_i]][2])] <- classestomerge[[merge_i]][1]
    }
  }

  # convert testandtrainingsample$class to a factor
  testandtrainingsample$class <- as.factor(as.character(testandtrainingsample$class))

  # return testandtrainingsample
  return(testandtrainingsample)

}
