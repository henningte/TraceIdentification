#### 20181212

#'@importFrom Rdpack reprompt
#'@import raster
#'@importFrom EBImage bwlabel
#'@importFrom stats median
NULL

#' Identifies Objects/Traces in Classified Images.
#'
#' \code{object_detection} detects objects in classified images as clusters of pixels
#' having a specific value (and corresponding to a specific class) and returns their
#' centroid (median) coordinates. The function relies
#' on the funciton \code{\link[EBImage]{bwlabel}} in order to deliniate objects/traces
#' and on the function \code{\link[raster]{rasterToPoints}} in order to identify objects/traces
#' centroid coordinates.
#'
#' @param classifiedimage A \code{\link[raster]{raster}} object (RasterLayer) representing
#' an image that was classified using \code{\link{dagsvm_classification_predict}}.
#' @param targetclassvalue A numeric value representing the class values of
#' \code{classifiedimage} for which objects/traces will be identified.
#' @return The function returns a list with an element for each image file in \code{directorydata}
#' containing the classified image.
#' @seealso \code{\link{dagsvm_classification_predict}}.
#' @source  The function relies on the R packages \code{raster}
#' (\insertCite{Hijmans.2017}{TraceIdentification}) and \code{EBImage}
#' (\insertCite{Pau.2010}{TraceIdentification}).
#' @references
#'     \insertAllCited{}
#' @examples #
#' @export
object_detection <- function(classifiedimage,
                            targetclassvalue = 1

){

  # set values for non target classes to zero
  classifiedimage[which(values(classifiedimage) != targetclassvalue)] <- 0

  # detect objects of target class
  identifiedobjects <- bwlabel(as.array(classifiedimage))

  # convert identifiedobjects to a matrix
  identifiedobjectsraster <- raster(matrix(as.vector(identifiedobjects),
                                           nrow = dim(identifiedobjects)[1],
                                           ncol = dim(identifiedobjects)[2]))

  objectlabels <- as.data.frame(table(identifiedobjects))
  objectlabels[,1] <- as.numeric(as.character(objectlabels[,1]))

  # extract the object centroids
  objectcoordinates <- do.call(rbind, lapply(seq_len(nrow(objectlabels))-1, function(object_i){

    # get the coordinates of pixels of object_i
    objectcoordinates <- rasterToPoints(identifiedobjectsraster, function(y){y == object_i})

    # calcualte the centroid of the pixel coordinates of object_i
    data.frame(class = object_i, x = median(objectcoordinates[,1]), y = median(objectcoordinates[,2]))

  }))

  # merge objectlabels and objectcoordinates
  objects <- cbind(objectlabels, objectcoordinates[c(2,3)])
  colnames(objects) <- c("object_id", "numberofpixels", "x", "y")

  # return objects
  return(objects)

}
