#' Aggregate ledger data.frames by time blocks
#'
#' Aggregate data by time blocks. Currently only supports daily.
#'
#' @param data, the data file (usually produced by \code{read_ledger})
#' @param cumulative should a cumulative sum be added? defaults to FALSE
#' @param aggregate string indicating what level to aggregate to. See \code{lubridate::floor_date} for supported units. Defaults to "day".
#' @return a tibble (data.frame)
#' @examples
#' demo <- system.file("extdata", "demo.ledger", package="ledgerReports")
#' ledge <- read_ledger("'^assets' '^liab' -X $", file = demo)
#' time_aggregate(ledge, cumulative = TRUE)
#' @export
time_aggregate <- function(data, cumulative = FALSE,
                          aggregate = "day",
                          split = FALSE){
  data$date <- lubridate::floor_date(data$date, unit = aggregate)
  if(split){
    data <- tidyr::separate(data, account,
                           into = c("account", "account_two", "account_three"),
                           sep = ":", fill = "right")
    data <- dplyr::summarize_(dplyr::group_by_(data, "date", "account"),
                             "amount" = ~ sum(amount))
    if(cumulative){
      data <- dplyr::mutate_(dplyr::group_by_(data, "account"),
                            "cumulative" = ~ cumsum(amount))
    }
  } else{
    data <- dplyr::summarize_(dplyr::group_by_(data, "date"),
                             "amount" = ~ sum(amount))
    if(cumulative){
      data$cumulative <- cumsum(data$amount)
    }
  }
  return(data)
}
