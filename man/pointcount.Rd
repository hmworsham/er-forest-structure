% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/points_to_raster.R
\name{pointcount}
\alias{pointcount}
\title{Points to raster}
\usage{
pointcount(ras, pts)
}
\arguments{
\item{ras}{raster Raster template}

\item{pts}{data.frame. Dataframe of modeled trees with at minimum X,Y,Z coordinates}
}
\value{
raster. Raster summarizing stem density per grid cell
}
\description{
Count trees per grid cell to rasterize density
}
