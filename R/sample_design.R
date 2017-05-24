#' Two-dimensional systematic sample
#'
#' @param dis A vector of length two indicating the distance in pixels between
#'   sampling locations. The first element is the distance in y-direction, the
#'   second element the distance in x-direction.
#' @param ras_dim Dimensions of the raster image on which to perform sampling.
#'   The first value is the number of rows, the second the number of columns.
#' @param start A vector of length two indicating the starting point of the
#'   sampling grid. The first element is the row index, the second the column
#'   index. If \code{NULL}, a random start is generated.
#'
#' @details Sampling on irregular shapes is not implemented yet. The function
#'   assumes a recangular shape of the study area.
#'
#' @return A data.table object with row and column indices
#' @author Sebastian Schnell
#' @export
gen_sys <- function(dis, ras_dim, start = NULL) {
  if (is.null(start)) {
    start <- c(round(runif(1, min = 1, max = dis)),
               round(runif(1, min = 1, max = dis)));
  }
  if (length(dis) == 1) {
    d_r <- d_c <- dis;
  } else {
    d_r <- dis[1];
    d_c <- dis[2];
  }

  dt_s <- data.table(expand.grid(row = seq.int(start[1], ras_dim[1], d_r),
                                 col = seq.int(start[2], ras_dim[2], d_c)));
  dt_s[, id := 1:.N];
  return(dt_s);
}

#' Generating global sampling grids with constant distance between sample
#' locations on the surface of a sphere
#'
#' @param dis Distance in kilometers between sample locations
#' @param bnd Polygon outline of an area of interest for which the sampling grid
#'   is generated (a \code{\link[sp]{SpatialPolygonsDataFrame}} object). If
#'   \code{NULL}, a global grid is generated.
#'
#' @details The grid consists of equidistant points along circles of latitude on
#'   a spheroid (WGS84/Pseudo-Mercator, epsg:43328).
#'
#' @return An object of \code{\link[sp]{SpatialPointsDataFrame}} holding the
#'   sampling locations of the grid.
#' @author Lutz Fehrmann
#' @export
#'
#' @examples
#' # Boundary of Germany
#' ger_bnd <- load_boundary(x = NA, country_code = "DEU", adm_level = 0);
#'
#' gsg_ger <- gen_gsg(50, ger_bnd);
#' plot(gsg_ger)
gen_gsg <- function(dis, bnd = NULL) {
  wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0";
  # Area of interest
  if (is.null(bnd)) {
    data("wrld_simpl"); # Using data set from maptools, "countries" in getData() not working
    bnd <- wrld_simpl;
  } else {
    if (identical(proj4string(bnd), wgs84) == FALSE) {
      warning("bnd has wrong projection! Transformed to epsg:4326");
      bnd <- sp::spTransform(x = bnd, CRSobj = CRS(wgs84));
    }
  }

  aoi <- c(sp::bbox(bnd)[2], sp::bbox(bnd)[4],
           sp::bbox(bnd)[1], sp::bbox(bnd)[3]);

  wgs84_semi_major_axis <- 6378.137;
  deg <- (dis/(pi * wgs84_semi_major_axis)) * 180;

  # Create a vector of longitue along equator
  lon <- sort(c(seq(0, -180, -deg), seq(deg, 180, deg)));

  # Create a vector of latitudes in aoi
  lat <- lon[lon >= aoi[1] & lon <= aoi[2]];

  # Calculate a matrix of longitudes for each circle of latitude
  lon_mat <- outer(lon, lat, function(x, y) x/cos(pi/180 * y));

  # Compile coordinates
  coord <- cbind(as.vector(t(lon_mat)), lat);
  colnames(coord) <- c("X", "Y");

  # Subset coordinates in aoi
  coord <- coord[coord[, 1] >= aoi[3] & coord[, 1] <= aoi[4],];

  spdf_gsg <- sp::SpatialPointsDataFrame(coords = coord,
                                         data = data.frame(1:nrow(coord)));
  sp::proj4string(spdf_gsg) <- sp::CRS(wgs84);

  # Subset gridpoints falling on land (or in a specific country)
  df_over <- sp::over(spdf_gsg, bnd);
  idx <- is.na(df_over[, 1]) == FALSE;
  return(SpatialPointsDataFrame(coords = coordinates(spdf_gsg[idx, ]),
                                data = cbind(1:sum(idx), df_over[idx, ]),
                                proj4string = crs(spdf_gsg)));
}
