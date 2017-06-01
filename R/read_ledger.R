read_ledger <- function(report_query, file = Sys.getenv("LEDGER_FILE")){
  lines <- system2("ledger", paste0("csv ", report_query), stdout = TRUE)
  conn <- textConnection(lines)
  df <- read.csv(conn, header = FALSE)
  close(conn)
  df <- df[, c(1, 3, 4, 5, 6, 8)]
  names(df) <- c("date", "payee", "account", "commodity", "amount", "note")
  return(df)
}
