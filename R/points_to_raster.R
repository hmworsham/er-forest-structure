#' Points to raster
#' @description Count trees per grid cell to rasterize density
#' @param ras raster Raster template
#' @param pts data.frame. Dataframe of modeled trees with at minimum X,Y,Z coordinates
#' @return raster. Raster summarizing stem density per grid cell
#' @export pointcount
#'

pointcount = function(ras, pts){
  # make a raster of zeroes like the input
  r2 = ras
  r2[] = 0

  # make another raster of zeroes like the input
  r3 = ras
  r3[] = 0

  # get n returns
  rr = data.frame(x=pts$X, y=pts$Y)

  # get the cell index for each point and make a table:
  rr = table(cellFromXY(ras, rr))

  # fill in the raster with the counts from the cell index:
  #r2[as.numeric(names(peaks))] = peaks
  r3[as.numeric(names(rr))] = rr
  return(r3)
}
