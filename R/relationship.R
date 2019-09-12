#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param parent PARAM_DESCRIPTION, Default: NULL
#' @param child PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname relationship
#' @export
relationship <- function(x, parent = NULL, child = NULL){

  fn_names <- unlist(lapply(x,function(xx) names(xx)),use.names = FALSE)

  if(!is.null(child)){

    fn_names <- intersect(fn_names,child)

  }

  out <- lapply(x,find_relatives,
                parent = parent, fn_names = fn_names)

  out <- out[!sapply(out,is.null)]

  structure(out,class=c('relationship','list'))

}

find_relatives <- function(y,parent,fn_names) {

  if(!is.null(parent)){

    if(!any(names(y)%in%parent))
      return(NULL)

  }

  ret_up <- lapply(y,function(xx) {

    ret <- fn_names[fn_names %in% attr(xx,'SYMBOL_FUNCTION_CALL')]
    if(!length(ret))
      ret <- NULL

    ret
  })

  ret_up <- ret_up[!sapply(ret_up,is.null)]

  if(!length(ret_up))
    ret_up <- NULL

  if(!is.null(parent)){

    ret_up <- ret_up[intersect(parent,names(ret_up))]

  }

  ret_up

}
