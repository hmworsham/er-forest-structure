# Functions to run LMF fixed window ITC delineation algorithm


#' LMF variable window function
#' @description Define variable window as thresholded exponential f(height)
#' @export vws

#' LMF variable window initialization
#' @description Initialize LMF fixed window for optimization
#' @export lmf.vw.init
#'
# Define variable window size function
lmf.vw.init <- function(pc, b0, b1, c0, shape, hmin=1.3) {
  vws = function(z, p1, p2, p3) {
    y <- p1 * (-(exp(-p2*(z-2)) - 1)) + p3
    y[z < 2] <- 3
    y[z > 20] <- 5
    return(y)
  }
  ws_args = list(z='Z', p1=b0, p2=b1, p3=c0)
  algo = lmf(ws=vws, shape=shape, hmin=hmin, ws_args=ws_args)
  lmf.vw.trees <- find_trees(pc, algo)
  return(lmf.vw.trees)
}

# TODO: WRITE THIS UP
#' LMF fixed window optimization
#' @description Run LMF fixed window optimization algorithm across sample areas
#' @export lmf.vw.opt
#'
lmf.vw.opt <- function(x, params) {
  modtrees <- mapply(lmf.vw.init,
                     b0=params[,1][[1]],
                     b1=params[,2][[1]],
                     c0=params[,3][[1]],
                     shape=params[,4][[1]],
                     MoreArgs=list(pc=x, hmin=1.3)#,
                     # mc.cores = getOption("mc.cores", length(workerNodes)-2)
                     )


  # Clean up results
  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x) {
    tl <- x[!st_is_empty(x),]
    tl
  })

  return(modtrees)
}
