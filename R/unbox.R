#' @importFrom utils getParseData
unbox <- function(file = "", text = NULL) {

  if (!is.null(text) & length(text) == 1) {
    text <- strsplit(text, "\n")[[1]]
  }

  if (nzchar(file)) {
    text <- readLines(file, warn = FALSE)
  }

  if (is.null(text)||length(text)==0)
    return(NULL)

  parse_text        <- parse(text = text,keep.source = TRUE)
  parse_data        <- utils::getParseData(parse_text)
  parse_data_filter <- parse_data$parent[with(parse_data, text == "function" & terminal == TRUE)]

  if(!length(parse_data_filter))
    return(NULL)

  parse_lines       <- parse_data[parse_data$id %in% (parse_data_filter + 1), ]
  parse_lines$root  <- as.numeric(rownames(parse_lines))

  parse_split <- lapply(parse_lines$root, split_lines, lines = text, parse_data = parse_data, parse_lines = parse_lines)

  names(parse_split) <- sapply(parse_split,attr,'name')

  parse_split

}
