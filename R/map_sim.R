#' Simulating forest cover maps with defined covariance structure using random
#' fields
#'
#' @param ras_dim A vector of two values where the first specifies the number of
#'   columns and the second the number of rows of the output raster.
#' @param pxl_size Pixel size of the output map.
#' @param p Forest cover of the out put map, 0 < p <= 1.
#' @param cov_model A covariance model used for the random fields simulation.
#'   Type \code{RFgetModelNames(type="positive definite")} to get a list of
#'   available models. Defaults to \code{NULL} in which case a generalized
#'   cauchy family covariance model is used
#'   (\code{\link[RandomFields]{RMgencauchy}}).
#' @param alpha See \code{\link[RandomFields]{RMgencauchy}}
#' @param beta See \code{\link[RandomFields]{RMgencauchy}}
#' @param scale See \code{\link[RandomFields]{RMgencauchy}}. Default value is
#'   set to on third of the main diagonal taken from \code{ras_dim} and
#'   \code{pxl_size}.
#' @param ... Other parameters used by \code{\link[RandomFields]{RMgencauchy}}.
#'
#' @return A binary \code{\link[raster]{raster}} map of simulated forest cover.
#' @author Paul Magdon, Sebastian Schnell
#' @export
#'
#' @examples
#' # Raster size
#' ras_dim <- c(1000, 1000);
#'
#' # Pixel size
#' pxl_size <- 30;
#'
#' # Forest cover
#' p <- 0.7;

#' # Cauchy covariance model
#' # 0 < alpha <= 2 - low alpha -> high fragmentation
#' # beta > 0 - smaller beta -> longer long-range dependence
#' cov_model <- RMgencauchy(alpha = 1.95,
#'                          beta = 0.3,
#'                          scale = 1/3*sqrt((sum(ras_dim*pxl_size)^2)));
#' forest_ras <- sim_forest_cover(ras_dim, pxl_size, cov_model, p);
#' plot(forest_ras);
sim_forest_cover <- function(ras_dim,
                             pxl_size,
                             p,
                             cov_model = NULL,
                             alpha = 0.7,
                             beta = 0.3,
                             scale = 1/3*sqrt((sum(ras_dim*pxl_size)^2)),
                             ...) {

  RandomFields::RFoptions(spConform = FALSE, cPrintlevel = 3);

  if (is.null(cov_model)) {
    cov_model <- RMgencauchy(alpha = alpha,
                             beta = beta,
                             scale = scale,
                             ...);
  }

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
