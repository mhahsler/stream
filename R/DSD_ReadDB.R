#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#' Read a Data Stream from an open DB Query
#'
#' A DSD class that reads a data stream from an open DB result set from a
#' relational database with using R's data base interface (DBI).
#'
#' This class provides a streaming interface for result sets from a data base
#' with via [DBI::DBI]. You need to connect to the data base and submit a SQL
#' query using [DBI::dbGetQuery()] to obtain a result set. Make sure that your
#' query only includes the columns that should be included in the stream
#' (including class and outlier marking columns).
#'
#' **Closing and resetting the stream**
#'
#' Do not forget to clear the
#' result set and disconnect from the data base connection. Since this procedure is data base dependent,
#' `close_stream()` just produced a warning (see Example section).
#'
#'  [reset_stream()] is not available for this type of stream.
#'
#' **Additional information**
#'
#' If additional information is available (e.g., class information), then the SQL
#' statement needs to make sure that the columns have the appropriate name starting with `.`.
#' See Examples section below.
#'
#' @family DSD
#'
#' @param result An open DBI result set.
#' @param k Number of true clusters, if known.
#' @param description a character string describing the data.
#' @return An object of class `DSD_ReadDB` (subclass of  [DSD_R], [DSD]).
#' @author Michael Hahsler
#' @seealso [DBI::dbGetQuery()]
#' @examples
#' ### create a data base with a table with 3 Gaussians
#' library("RSQLite")
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' points <- get_points(DSD_Gaussians(k=3, d=2), n = 110)
#' head(points)
#'
#' dbWriteTable(con, "Gaussians", points)
#'
#' ### prepare a query result set. Make sure that the additional information
#' ### column starts with .
#' res <- dbSendQuery(con, "SELECT X1, X2, `.class` AS '.class' FROM Gaussians")
#' res
#'
#' ### create a stream interface to the result set
#' stream <- DSD_ReadDB(res, k = 3)
#' stream
#'
#' ### get points
#' get_points(stream, n = 5)
#'
#' plot(stream, n = 100)
#'
#' ### clean up (close_stream() does not do this!)
#' dbClearResult(res)
#' dbDisconnect(con)
#' @export
DSD_ReadDB <- function(result,
  k = NA,
  description = NULL) {

  # figure out d
  d <- length(grep('^\\.', DBI::dbColumnInfo(result)[["name"]], invert = TRUE))

  if (is.null(description))
    description <- paste0('DB Query Stream (d = ', d , 'k = ', k, ')')


  # creating the DSD object
  l <- list(
    description = description,
    d = d,
    k = k,
    result = result
  )
  class(l) <- c("DSD_ReadDB", "DSD_R", "DSD")

  l
}

#' @rdname DSD_ReadDB
#' @export
close_stream.DSD_ReadDB <- function(dsd)
  warning("close_stream not implemented for DSD_ReadDB. You need to clear the result and disconnect the database manually (see example in '? DSD_ReadDB')")

#' @export
get_points.DSD_ReadDB <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  info = TRUE,
  ...) {
  .nodots(...)

  outofpoints <- match.arg(outofpoints)
  n <- as.integer(n)

  d <- DBI::dbFetch(x$result, n = n)

  if (nrow(d) < n) {
    if (outofpoints == "stop") {
      stop("Not enough points in the stream! Lost ", nrow(d), " points.")
    }
    if (outofpoints == "warn")
      warning("Not enough data points left in stream, returning the remaining ", nrow(d), " points!")
  }

  if (!info)
    d <- remove_info(d)

  d
}
