#### 20181212

#'@importFrom Rdpack reprompt
#'@import sinkr
#'@importFrom raster raster
NULL

#' Creates Random Rasters of Traces of a Specific Class.
#'
#' \code{create_random_raster} creates a set of different rasters with a specified
#' number of rows and columns. For a set of classes, randomly 1 % of the cells are
#' selected for each class and filled with the respective colour value. 20 % of these
#' cells will be enlarged by the next pixel to the right, to the top and to the top right.
#' Each created raster will be exported as jpg file with a fixed resolution of
#' 6000 times 6000 pixels. The funciton is used in order to create random rasters that serve
#' as templates in order to collect test and training samples.
#'
#' @param n.col.raster A numeric value representing the number of columns
#' of the raster to create.
#' @param n.row.raster A numeric value representing the number of rows
#' of the raster to create.
#' @param colours.cells A character vector with colours to use for each class.
#' @param seed A numeric vector with integers. For each element of \code{seed},
#' a random raster is created.
#' @param directory.result A character string representing the directory where
#' the results will be stored.
#' @return The function returns nothing, but stores a jpg file of each created
#' raster into \code{directory.result}. Created rasters have a resolution of
#' 6000 times 6000 pixels and are named as raster_position_x_y where x is the
#' respective element of \code{seed} and y the number of elements in
#' \code{colours.cells}, i.e. the number of different classes.
#' @seealso .
#' @note Since the function \code{pos2coord} is part of the package \code{sinkr},
#' but not implemented in its structure, the R script has to be sourced prior
#' using \code{create_random_raster}. The function \code{pos2coord} can be retrieved
#' from the github repository of the author of \code{sinkr}:
#' \url{https://github.com/marchtaylor/sinkr/blob/master/R/pos2coord.R}.
#' @source The function relies on the R packages \code{raster}
#' (\insertCite{Hijmans.2017}{TraceIdentification}) and \code{sinkr}
#' (\insertCite{Taylor.2017}{TraceIdentification}).
#' @references
#' \insertAllCited{}
#' @examples #
#' @export
create_random_raster <- function(n.col.raster = 200,
                                n.row.raster = 200,
                                colours.cells = c("lightgrey", "green", "red"),
                                seed = c(1:4),
                                directory.result
){

  # get number of colours
  number.colours = length(colours.cells)

  for(try_i in seed){

    # define raster position as vector
    raster.position = seq(from = 1, to = n.col.raster * n.row.raster, by = 1)

    # create dummy raster with zero cell values
    raster.position = pos2coord(pos = raster.position, dim.mat = c(n.col.raster, n.row.raster))
    raster.position = as.data.frame(raster.position)
    raster.position$values = rep(0, times = nrow(raster.position))

    # define counter
    col.counter = 1

    # define dummy raster for sampling
    raster.position1 = raster.position

    while(!col.counter > number.colours){

      if(col.counter > 1){

        # delete dummy raster entries that were already sampled
        raster.position1 = raster.position1[-random.sample.tot,]

      }

      # random sample of 1%/number.colours of the samples
      set.seed(try_i)
      random.sample = sample(c(1:nrow(raster.position1)), size = (n.col.raster * n.row.raster)/100/number.colours)

      # random sample of 20% of the cells of "random.sample"
      random.sample1 = sample(random.sample, size = length(random.sample)/100*20)

      # addition of the raster cells one raster cell to the right
      random.sample1_right = random.sample1 + 1

      # addition of the raster cells one raster cell above
      random.sample1_above = random.sample1 - n.col.raster

      # addition of the raster cells one raster cell to the right and one raster cell above
      random.sample1_aboveright = random.sample1 - n.col.raster + 1

      # merge random samples
      random.sample.tot = unique(c(
        random.sample,
        random.sample1,
        random.sample1_right,
        random.sample1_above,
        random.sample1_aboveright
      ))

      # delete negative entries
      if(length(which(sign(random.sample.tot) == -1)) > 0){

        random.sample.tot = random.sample.tot[-which(sign(random.sample.tot) == -1)]

      }

      # set raster cell value to colour value:
      raster.position$values[random.sample.tot] = col.counter

      # augment col.counter by 1
      col.counter = col.counter + 1

    }

    # convert to raster
    raster.position <- raster(matrix(data = raster.position$values, nrow = n.row.raster, ncol = n.col.raster))

    # export raster as jpg
    jpeg(paste(directory.result, "/", "raster_position_", try_i, "_", number.colours, ".jpeg", sep=""), 6000, 6000)
    par(oma=c(0,0,0,0), mar=c(0,0,0,0))
    plot(raster.position, col= c("white", colours.cells), maxpixels= 1e8, axes=F, box = F, legend =F)
    dev.off()

  }

}
