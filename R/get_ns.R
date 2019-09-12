get_ns <- function(ns, td = NULL  ){

  if(is.null(td)){

    if(inherits(ns,'character')){
      ns_name <- ns
      ns <- asNamespace(ns)

    }

    if(inherits(ns,'environment')){

      ns_name <- gsub('^(.*?)namespace:|>$','',capture.output(print(ns)))

    }

    td <- file.path(tempdir(),sprintf('package_clone_%s',ns_name))

  }


  lsx <- ls(envir = ns)

  lsx_cl <- sapply(lsx,function(x) inherits(get(x,envir = ns),'function'),USE.NAMES = FALSE)

  lsx_f <- lsx[lsx_cl]

  invisible(lapply(lsx_f,extract_lines,ns = ns, td = td))

  return(td)

}
