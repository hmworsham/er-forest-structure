# Functions to run LMF fixed window ITC delineation algorithm


#' LMF variable window function
#' @description Define variable window as thresholded exponential f(height)
#' @export vws
vws = function(x, p1, p2, p3) {
  y <- p1 * (-(exp(-p2*(x-2)) - 1)) + p3
  y[x < 2] <- 3
  y[x > 20] <- 5
  return(y)
}

#' LMF variable window initialization
#' @description Initialize LMF fixed window for optimization
#' @export ls.init
#'
# Define variable window size function
lmf.vw.init <- function(pc, ws, p1, p2, p3, shape, hmin=1.3) {
  algo = lmf(ws=vws(p1,p2,p3), shape=shape, hmin=hmin)
  lmf.vw.trees <- find_trees(pc, algo)
  return(lmf.vw.trees)
}

# TODO: WRITE THIS UP
#' LMF fixed window optimization
#' @description Run LMF fixed window optimization algorithm across sample areas
#' @export ls.opt
#'
lmf.vw.opt <- function(x, params) {
  modtrees <- mapply(lmf.vw.init,
                     ws=params[,1][[1]],
                     shape=params[,2][[1]],
                     MoreArgs=list(pc=x, hmin=1.3)#,
                     # mc.cores=getOption('mc.cores', 30)
  )

  # Clean up results
  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x) {
    tl <- x[!st_is_empty(x),]
    tl
  })

  return(modtrees)
}
