scan_for_content <- function(FILE, warn = TRUE){

  lines <- readLines(FILE, warn = FALSE)

  lines <- lines[!grepl("^\\s*#'", lines)]

  objs <- gsub(
    "\\s*([[:alnum:]._]+).*",
    "\\1", grep("^\\s*[[:alnum:]._]+\\s*(<-|=)", lines, value = TRUE)
  )

  res <- length(objs) == 0L

  if(warn & res)
    warning("No functions found in\n", normalizePath(FILE), call. = FALSE)

  return(!res)
}
