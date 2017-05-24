#' Load country boundaries into a \code{\link[sp]{SpatialPolygonsDataFrame}}
#' object
#'
#' @param x If missing, \code{link[raster]{getData}} function is used to load
#'   boundaries from the GADM project (http://gadm.org). Otherwise the name of
#'   the data source (see \code{\link[gdal]{readOGR}}).
#' @param country_code Country specified by three letter ISO code. Use function
#'   \code{\link[raster]{ccodes}} to get appropriate codes.
#' @param adm_level Level of administrative subdivision 0 = country, 1 = first
#'   level subdivision
#' @param ... Other arguments to \code{\link[rgdal]{readOGR}}.
#'
#' @return A \code{\link[sp]{SpatialPolygonsDataFrame}} object storing country
#'   boundaries.
#' @author Nils Noelke, Sebastian Schnell
#' @export
#'
#' @examples
#' # Load boundary of Germany
#' ger_bnd <- load_boundary(country_code = "DEU", adm_level = 0);
#' plot(ger_bnd);
load_boundary = function (x, country_code, adm_level, ...) {
  if (missing(x)) {
    # Load boundary from GADM
    bnd <- raster::getData(name = 'GADM',
                           country = country_code,
                           level = adm_level);
  } else {
    # Load boundary from other vector format using readOGR
    bnd <- rgdal::readOGR(dsn = x, ...);
  }
  return(bnd);
}
