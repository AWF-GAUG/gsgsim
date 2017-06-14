#' Load country boundaries into a \code{\link[sp]{SpatialPolygonsDataFrame}}
#' object
#'
#' @param x If missing, \code{link[raster]{getData}} function is used to load
#'   boundaries from the GADM project (http://gadm.org). Otherwise the name of
#'   the data source (see \code{\link[gdal]{readOGR}}).
#' @param country_code Country specified by three letter ISO code. Use function
#'   \code{\link[raster]{ccodes}} to get appropriate codes. If multiple
#'   countries are desired, provide a character vector of appropriate ISO codes.
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
#'
#' # Load boundary of several countries
#' bnd <- load_boundary(country_code = c("DEU", "ITA", "FRA"));
load_boundary = function (x = NULL, country_code = 'world', adm_level = 0, ...) {
  if (is.null(x)) {
    if (toupper(country_code[1]) == 'WORLD') {
      data("wrld_simpl");
      return(wrld_simpl);
    } else {
      # Load boundary from GADM
      bnd <- raster::getData(name = 'GADM',
                             country = country_code[1],
                             level = adm_level);
      row.names(bnd) <- country_code[1];
      if (length(country_code) > 1) {
        for (i in 2:length(country_code)) {
          temp_bnd <- raster::getData(name = 'GADM',
                                      country = country_code[i],
                                      level = adm_level);
          row.names(temp_bnd) <- country_code[i];
          bnd <- maptools::spRbind(bnd, temp_bnd);
        }
      }
      bnd <- bnd[, c("ISO", "NAME_ENGLISH")];
    }
  } else {
    # Load boundary from other vector format using readOGR
    layer = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x))
    bnd <- rgdal::readOGR(dsn = x, layer=layer, ...);
  }
  return(bnd);
}

