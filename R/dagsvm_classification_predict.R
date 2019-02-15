#### 20181212

#'@importFrom Rdpack reprompt
#'@importFrom raster brick values
#'@import e1071
#'@importFrom grDevices dev.off jpeg
#'@importFrom graphics par
NULL

#' Classifies Images with a DAG-SVM.
#'
#' \code{dagsvm_classification_predict} takes a tuned DAG-SVM and the respective paramters
#' as returned by \code{\link{tunedagsvm_extractoptimisedparametersandmodels}} and a directory
#' name with images to classify and returns the images that were classified with the DAG-SVM.
#' The construction of DAG-SVM is derived according to \insertCite{Platt.2000}{TraceIdentification}
#'
#' @param optimparam A \code{data.frame} object corresponding to the first element of a
#' list returned by \code{\link{tunedagsvm_extractoptimisedparametersandmodels}} with the
#' tuned parameters of the DAG-SVM.
#' @param optimmodels A list corresponding to the second element of a
#' list returned by \code{\link{tunedagsvm_extractoptimisedparametersandmodels}} with the
#' tuned models of the DAG-SVM.
#' @param directorydata A character string representing the directory where
#' the image files to be classified are stored. No other files
#' must be in this directory.
#' @return The function returns a list with an element for each image file in \code{directorydata}
#' containing the classified image.
#' @seealso \code{\link{tunedagsvm}}, \code{\link{tunedagsvm_extractoptimisedparametersandmodels}}.
#' @source The function relies on the R packages \code{raster}
#' (\insertCite{Hijmans.2017}{TraceIdentification}) and \code{e1071}
#' (\insertCite{Meyer.2015}{TraceIdentification}).
#' @references
#' \insertAllCited{}
#' @examples #
#' @export
dagsvm_classification_predict <- function(optimparam, optimmodels, directorydata){

  # get the unique classes
  classes <- unique(c(as.character(optimparam$class1), as.character(optimparam$class2)))

  # get needed number of layers in the DAGSVM
  numberoflayers <- length(classes) - 1

  # get needed number of nodes in the DAGSVM
  numberofnodes <- sum(seq_len(numberoflayers))

  # create list with names of sample files
  filenames = list.files(directorydata, full.names = TRUE)

  lapply(seq_along(filenames), function(image_i){

    # import the file
    imagetoclassifyor <- brick(filenames[[image_i]])

    # create a data.frame object with band values and class labels
    imagetoclassify <- data.frame(r = getValues(imagetoclassifyor[[1]]),
                                  g = getValues(imagetoclassifyor[[2]]),
                                  b = getValues(imagetoclassifyor[[3]]))

    # add the row number indices to imagetoclassify
    imagetoclassify$rownumber = seq_len(nrow(imagetoclassify))

    classifiedimagedf <- do.call(rbind, lapply(seq_len(numberoflayers), function(layer_i){

      # get the number of nodes of layer_i
      numberofnodeslayeri <- layer_i

      do.call(rbind, lapply(seq_len(numberofnodeslayeri), function(node_i){

        ### for all layers except the last one
        # if(layer_i != numberoflayers){

        ## input data for the current node
        if(layer_i == 1){
          # root node
          imagetoclassifyinput <- imagetoclassify
        }else{
          # not root node
          if(node_i == 1){
            # first node of layer_i
            imagetoclassifyinput_negative <- get(paste("imagetoclassifyinput_",image_i, "_", (layer_i - 1), "_", (node_i),"_",  "negative",  sep="")) # negative: left
            imagetoclassifyinput <- imagetoclassifyinput_negative
          }
          if(node_i == numberofnodeslayeri){
            # last node of layer_i
            imagetoclassifyinput_positive <- get(paste("imagetoclassifyinput_",image_i, "_", (layer_i - 1), "_", (node_i - 1),"_",  "positive",  sep="")) # positive: right
            imagetoclassifyinput <- imagetoclassifyinput_positive
          }
          if(node_i != 1 && node_i != numberofnodeslayeri){
            # intermediate node of layer_i
            imagetoclassifyinput_positive <- get(paste("imagetoclassifyinput_",image_i, "_", (layer_i - 1), "_", (node_i - 1),"_",  "positive",  sep="")) # positive: right
            imagetoclassifyinput_negative <- get(paste("imagetoclassifyinput_",image_i, "_", (layer_i - 1), "_", (node_i),"_",  "negative",  sep="")) # negative: left
            imagetoclassifyinput <- rbind(imagetoclassifyinput_positive, imagetoclassifyinput_negative)
          }

        }

        # get the classes in the input of node_i of layer_i and the classes node_i of layer_i will compare
        classestocompare <- getclassestocompare(classes, layer_i, node_i)[[2]]

        ## predicted values

        # get the optimal kernel
        kernel <- optimparam$kernel[which(optimparam$layer == layer_i & optimparam$node == node_i)]

        # load the best svm model for node_i of layer_i
        svm_fit <- optimmodels[[layer_i]][[node_i]]

        # get the predicted values
        svm_pred <- predict(svm_fit, imagetoclassifyinput)

        ## create the partitioned output data

        # positive (right, first class in the list)
        imagetoclassifyinput_positive <- imagetoclassifyinput[which(svm_pred == unique(svm_pred)[1]),]
        imagetoclassifyinput_positive <- imagetoclassifyinput_positive[!is.na(imagetoclassifyinput_positive$r),]
        imagetoclassifyinput_positive$class <- rev(classestocompare)[1]
        assign(paste("imagetoclassifyinput_",image_i, "_", (layer_i), "_", (node_i),"_",  "positive",  sep=""), imagetoclassifyinput_positive, envir = parent.frame())

        # negative(left, last class in the list)
        imagetoclassifyinput_negative <- imagetoclassifyinput[which(svm_pred == unique(svm_pred)[2]),]
        imagetoclassifyinput_negative <- imagetoclassifyinput_negative[!is.na(imagetoclassifyinput_negative$r),]
        imagetoclassifyinput_negative$class <- rev(classestocompare)[2]
        assign(paste("imagetoclassifyinput_",image_i, "_", (layer_i), "_", (node_i),"_",  "negative",  sep=""), imagetoclassifyinput_negative, envir = parent.frame())

        # }

        # for the last layer in the DAGSVM
        if(layer_i == numberoflayers){

          # assign the class labels
          imagetoclassifyinput_positive$class <- rev(classestocompare)[1]
          imagetoclassifyinput_negative$class <- rev(classestocompare)[2]
          imagetoclassifyinput <- rbind(imagetoclassifyinput_positive, imagetoclassifyinput_negative)

          # return imagetoclassifyinput
          return(imagetoclassifyinput)

          # }))

        }else{
          NULL
        }

      }))

    }))

    ## create the raster
    classifiedimagenew <- imagetoclassifyor[[1]]

    # assign the class labels
    for(class_i in c(1:length(classes))){
      values(classifiedimagenew)[classifiedimagedf$rownumber[which(classifiedimagedf$class == classes[class_i])]] <- class_i
    }

    # return classifiedimagenew
    return(classifiedimagenew)

  })

}
