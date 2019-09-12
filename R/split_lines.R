split_lines <- function(x, lines, parse_data, parse_lines) {

  y       <- parse_lines[x == parse_lines$root, c("line1", "line2")]
  y       <- seq(y[, 1], y[, 2])
  fn_lines <- c(parse_data$line1[parse_data$parent %in% (-x)], y)
  lout     <- lines[fn_lines]
  fn_name  <- parse_data$text[which(parse_data$id == x) + 1]

  if (!nzchar(fn_name))
    fn_name <- NULL

  ret <- data.frame(
    text = lines[fn_lines],
    lines = fn_lines,
    stringsAsFactors = FALSE
    )

  attr(ret,'name') <- fn_name

  attr(ret,'parse') <- parse_data[parse_data$line1%in%fn_lines,]

  attr(ret,'SYMBOL_FUNCTION_CALL') <- parse_data$text[parse_data$line1%in%fn_lines&parse_data$token=='SYMBOL_FUNCTION_CALL']

  class(ret) <- c('box','data.frame')

  ret

}
