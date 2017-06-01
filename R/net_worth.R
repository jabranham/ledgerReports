#' Graph your net worth
#'
#' Helper function to graph net worth over time
#'
#' @param data the data to graph
#' @param start the start date (e.g. "2017-01-01"), defaults to the start of the
#'   series
#' @param end the end date (e.g. "2017-12-31"), defaults to today
#' @return a "ggplot2" graph object
net_worth <- function(data, start = NA, end = Sys.Date()){
  if (!requireNamespace("ggplot2", quietly = TRUE)){
    stop("ggplot2 not found!")
  }
  start <- as.Date(start)
  end <- as.Date(end)
  g <- ggplot2::ggplot(data, ggplot2::aes(date, cumulative))
  g <- g + ggplot2::geom_line()
  g <- g + ggplot2::scale_x_date(limits = c(start, end))
  return(g)
}
