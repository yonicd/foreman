#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param path PARAM_DESCRIPTION, Default: 'R'
#' @param ns PARAM_DESCRIPTION, Default: NULL
#' @param warn PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname unpack
#' @export
unpack <- function(path = 'R', ns = NULL, warn = TRUE){

  if(!is.null(ns)){

    path <- get_ns(asNamespace(ns))

    on.exit(unlink(path,recursive = TRUE,force = TRUE),add = TRUE)

  }

  if (length(path) == 1L && file.info(path)$isdir) {

    files <- list.files(path = path, pattern = ".+\\.[rR]$", full.names = TRUE)

  } else {

    files <- path

  }

  if (!all(grepl("\\.[rR]$", basename(files)))) {
    stop("Supplied file(s) is not an .R file!", call. = FALSE)
  }

  files <- files[sapply(files,scan_for_content,warn = warn)]

  if(length(files)==0){
    return(invisible(NULL))
  }

  names(files) <- basename(files)

  ret <- lapply(files,unbox)

  class(ret) <- c('boxes','list')

  ret
}
