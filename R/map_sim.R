#' Simulating forest cover maps with defined covariance structure using random
#' fields
#'
#' @param ras_dim A vector of two values where the first specifies the number of
#'   columns and the second th number of rows of the output raster.
#' @param pxl_size Pixel size of the output map.
#' @param cov_model A covariance model used for the random fields simulation.
#'   Type \code{RFgetModelNames(type="positive definite")} to get a list of
#'   available models.
#' @param p Forest cover of the out put map, 0 < p <= 1.
#'
#' @return A binary \code{\link[raster]{raster}} map of simulated forest cover.
#' @author Paul Magdon, Sebastian Schnell
#' @export
#'
#' @examples
#' # Cauchy covariance model
#' # 0 < alpha <= 2 - low alpha -> high fragmentation
#' # beta > 0 - smaller beta -> longer long-range dependence
#' cov_model <- RMgencauchy(alpha = 1.95, beta = 0.3);
#'
#' # Raster size
#' ras_dim <- c(1000, 1000);
#'
#' # Pixel size
#' pxl_size <- 30;
#'
#' # Forest cover
#' p <- 0.7;
#'
#' forest_ras <- sim_forest_cover(ras_dim, pxl_size, cov_model, p);
#' plot(forest_ras);
sim_forest_cover <- function(ras_dim, pxl_size, cov_model, p) {

  RandomFields::RFoptions(spConform = FALSE);

  grd <- sp::GridTopology(c(0, 0), c(pxl_size, pxl_size), ras_dim);
  sim <- RandomFields::RFsimulate(cov_model,
                                  x = grd);
  thresh <- qnorm(p, mean = mean(sim), sd = sd(sim));
  bin <- sim < thresh;
  map <- raster::raster(bin);
  raster::extent(map) <- raster::extent(c(0, ras_dim[1]*pxl_size,
                                          0, ras_dim[2]*pxl_size));
  return(map);
}
