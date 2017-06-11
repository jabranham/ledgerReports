#' Graph your net worth
#'
#' Helper function to graph net worth over time
#'
#' @param data the data to graph
#' @param start the start date (e.g. "2017-01-01"), defaults to the start of the
#'   series
#' @param end the end date (e.g. "2017-12-31"), defaults to today
#' @return a "ggplot2" graph object
#' @examples
#' demo <- system.file("extdata", "demo.ledger", package="ledgerReports")
#' ledge <- read_ledger("'^assets' '^exp' -X $", file = demo)
#' ledger_graph(ledge, end = "2012-01-01")
#' @export
ledger_graph <- function(data, start = NA, end = Sys.Date(),
                        separate = FALSE, aggregate = "day",
                        cumulative = FALSE){
  if (!requireNamespace("ggplot2", quietly = TRUE)){
    stop("ggplot2 not found!")
  }
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  data <- time_aggregate(data, cumulative = cumulative,
                        aggregate = aggregate, split = separate)
  ## Remove two "accounts" that aren't real
  if(separate){
    data <- dplyr::filter_(data, ~ !account %in% c("<Adjustment>", "<Revalued>"))
  }
  ## Drop data before start and after end, otherwise ggplot uses amounts to
  ## scale y-axis which can look weird:
  data <- dplyr::filter_(data, ~ date < end)
  if(!is.na(start)){
    data <- dplyr::filter_(data, ~ date > start)
  }

  ## Start the plot. We have four conditions:
  ## separate accounts TRUE/FALSE and cumulative TRUE/FALSE
  if(separate & cumulative){
    g <- ggplot2::ggplot(data, ggplot2::aes(date, cumulative, linetype = account))
  } else if(separate & !cumulative){
    g <- ggplot2::ggplot(data, ggplot2::aes(date, amount, linetype = account))
  } else if(!separate & cumulative){
    g <- ggplot2::ggplot(data, ggplot2::aes(date, cumulative))
  } else{
    g <- ggplot2::ggplot(data, ggplot2::aes(date, amount))
  }
  g <- g + ggplot2::geom_line()
  g <- g + ggplot2::scale_x_datetime(limits = c(start, end))
  return(g)
}
