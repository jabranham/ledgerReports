#' Read in from a ledger file
#'
#' \code{read_ledger} reads data from a ledger file using \code{ledger csv}
#'
#' @param report_query report query to pass to ledger csv
#' @param file the ledger file to read from
#' @return a data.frame object containing 6 columns - date, payee, account,
#'   commodity, amount, and note
#' @examples
#' demo <- system.file("extdata", "demo.ledger", package="ledgerReports")
#' read_ledger("'^assets' '^liab' -X $", file = demo)
#' @export
read_ledger <- function(report_query, file = Sys.getenv("LEDGER_FILE")){
  lines <- system2("ledger", c(paste0(" csv ", report_query),
                              paste0(" -f ", file),
                              "--date-format '%F'"),
                  stdout = TRUE)
  conn <- textConnection(lines)
  df <- read.csv(conn, header = FALSE)
  close(conn)
  df <- df[, c(1, 3, 4, 5, 6, 8)]
  names(df) <- c("date", "payee", "account", "commodity", "amount", "note")
  df$amount <- as.numeric(df$amount)
  df$date <- as.POSIXct(df$date)
  return(df)
}
