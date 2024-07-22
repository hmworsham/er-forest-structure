# Functions to run watershed ITC segmentation algorithm

#' Watershed initialization
#' @description Initialize watershed algorithm for optimization
#' @param pc LAS. Point cloud data in LAS format
#' @param res numeric. The size of a grid cell in point cloud coordinates units.
#' @param p2r.p numeric. Radius parameter fed into function `lidR::p2r`. Defined as radius of the subcircle that replaces each point with 8 points around the original LAS point to obtain fewer empty pixels.
#' @param ker.size numeric. The size of one side of the weighting kernel used in `raster::focal` to compute the raster digital surface model
#' @param hmin numeric. Minimum height of a detected tree. Default is 2.
#' @param pkg character. Use pkg = "terra|raster|stars" to get an output in SpatRaster, RasterLayer or stars format. Default is 'raster'
#' @return dataframe. X,Y,Z values corresponding to locations and heights of detected trees
#' @export ws.init
#'

# Initialize watershed algorithm
ws.init <- function(pc, res, p2r.p, ker.size, hmin, pkg='raster') {
  chm = rasterize_canopy(pc, res=res, algorithm=p2r(0.3), pkg=pkg)
  chm = raster::focal(chm, w=matrix(1,ker.size, ker.size), fun=mean, na.rm=T)
  ws.trees = segment_trees(pc, watershed(chm))
  crowns = crown_metrics(ws.trees, func = .stdtreemetrics, geom = "convex")
  crowns = crowns[st_is_valid(crowns),]
  ttops = st_centroid(crowns)
  ttops = ttops[ttops$Z >= hmin,]
  return(ttops)
}

#' Watershed optimization
#' @description Run watershed optimization algorithm across sample areas
#' @export ws.opt
#'

ws.opt <- function(x, params, hmin=1.3) {
  modtrees <- mapply(ws.init,
                       res=params[,1],
                       p2r.p=params[,2],
                       ker.size=params[,3],
                       MoreArgs=list(pc=x, hmin=hmin)#,
                       # mc.cores = getOption("mc.cores", length(workerNodes)-2)
                     )

  # Clean up results
  modtrees <- apply(modtrees, 2, data.frame)
  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x) {
    tl <- x[!st_is_empty(x),]
    tl
  })

  return(modtrees)
}

