# Test-only helper: evaluate `code` while capturing futile.logger output to a
# temp file, restoring the previous appender and threshold on exit.

capture_flog <- function(code, threshold = futile.logger::INFO) {
  log <- tempfile(fileext = ".log")
  old_app <- futile.logger::flog.appender()
  old_thresh <- futile.logger::flog.threshold()
  futile.logger::flog.appender(futile.logger::appender.file(log))
  futile.logger::flog.threshold(threshold)
  on.exit({
    futile.logger::flog.appender(old_app)
    futile.logger::flog.threshold(old_thresh)
  }, add = TRUE)
  value <- force(code)
  futile.logger::flog.appender(old_app)
  futile.logger::flog.threshold(old_thresh)
  log_lines <- if (file.exists(log)) readLines(log) else character(0)
  list(value = value, log = log_lines)
}
