#' @importFrom utils capture.output
extract_lines <- function(fun, ns, td){

  if(!dir.exists(td))
    dir.create(td)


  script     <- get(fun,envir = ns)

  if(grepl('^[[:punct:]]',fun)){

    file       <- file.path(td,'other.R')
    file_temp  <- file.path(td,'other_temp.R')

  }else{

    file       <- file.path(td,sprintf('%s.R',fun))
    file_temp  <- file.path(td,sprintf('%s_temp.R',fun))

  }

  on.exit(unlink(file_temp),add = TRUE)

  utils::capture.output(print(script), file = file_temp)

  check.file <- readLines(file_temp)

  body       <- check.file[!grepl("^<", check.file)]

  if(grepl('^[[:punct:]]',fun)){

    body[1]    <- sprintf('`%s` <- %s',fun,body[1])

  }else{

    body[1]    <- sprintf('%s <- %s',fun,body[1])

  }

  cat(body, file = file, sep = "\n",append = TRUE)

}
