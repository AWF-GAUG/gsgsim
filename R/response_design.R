#' Create point clusters arround sample locations
#'
#' @param dt_s A data.table object storing sample locations as returned from
#'   \code{\link{create_sys}}. Column names must be identical to "row", "col",
#'   and "id". The first stores the row index, the second the column index, and
#'   the last an identifier.
#' @param col_off A vector specifying the offset of sub-plot coordinates from
#'   the origin in x-direction. See details.
#' @param row_off A vector specifying the offset of sub-plot coordinates from
#'   the origin in y-direction. See details.
#' @param sp_dist The distance between sub-plots.
#'
#' @details The parameters \code{col_off}, \code{row_off}, and \code{sp_dist}
#'   are used to specify the size and shape of the point cluster. For the number
#'   of sub-plots and their spatial arrangement around a reference point, use
#'   \code{col_off} and \code{row_off}. There are as many sub-plots as values in
#'   the two vectors. The two vectors must have the same length, as values are
#'   interpreted as coordinate pairs. The location of the reference point is
#'   defined by offset values (see the example). \code{sp_dist} can be used to
#'   control the distance between sub-plots. If used, the values in
#'   \code{col_off} and \code{row_off} should be relative to the reference
#'   point. If set to a value of one, it has no effect and offsets can be
#'   specified in absolut values from the reference.
#'
#' @return A \code{\link[data.table]{data.table}} object with row and column
#'   indices of the generated clusters
#'
#' @author Sebastian Schnell
#'
#' @examples
#' dt_s <- data.table(row = c(10, 10, 20, 20),
#'                    col= c(10, 20, 20, 10), id = c(1:4));
#'
#' dt_s_cl <- create_clus(dt_s, col_off = c(-1, 1, 1, -1),
#'                        row_off = c(-1, -1, 1, 1),
#'                        sp_dist = 3);
#'
#' dt_s_cl[, plot(col, row)]; dt_s[, points(col, row, pch = 16)];
#' @import data.table
#' @export
create_clus <- function(dt_s, col_off, row_off, sp_dist) {
  if (is.data.table(dt_s) == FALSE) {
    stop("Input parameter dt_s is not a data.table object");
  } else if (identical(names(dt_s), c("row", "col", "id")) == FALSE) {
    stop("Column names or order of columns or column number do not match the
         expectation. The data.table object should have the following columns in
         that order: row, col, id");
  }
  dt_s_cl <- dt_s[, list(id_sp = 1:length(col_off),
                         col = col + col_off*sp_dist,
                         row = row + row_off*sp_dist),
                  by = id];
  return(dt_s_cl[!is.na(col)]);
}