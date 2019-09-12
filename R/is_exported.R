#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param funs PARAM_DESCRIPTION
#' @param pkg PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname is_exported
#' @export

is_exported <- function(funs,pkg){

  if(!pkg%in%loadedNamespaces())
    attachNamespace(asNamespace(pkg))

  funs%in%ls(sprintf('package:%s',pkg))
}
