#' @export
subset.boxes <- function(x,...){

  y <- c(...)
  xx <- as.data.frame(relationship(x))
  x1 <- xx[xx$parent%in%y,]
  z <- do.call('rbind',lapply(names(x),function(z) {
    data.frame(file = z, funs = names(x[[z]]),stringsAsFactors = FALSE)
  }))

  z1 <- z[z$funs%in%unique(c(x1$parent,x1$child)),]

  ret <- mapply(function(f,fun,x){x[[f]][[fun]]},
         f = z1$file,
         fun = z1$funs,MoreArgs = list(x=x),
         SIMPLIFY = FALSE
  )

  class(ret) <- c('boxes','list')

  ret

}

#' @export
print.box <- function(x,...){
  cat(get_text(x),sep='\n')
}

#' @export
as.data.frame.relationship <- function(obj){

  do.call('rbind',lapply(names(obj),function(nmx){

    xx <- obj[[nmx]]

    ret <- lapply(names(xx),function(nm,file){
      data.frame(child = xx[[nm]], parent = nm, file = file, stringsAsFactors = FALSE)
    },file = nmx)

    ret[[1]]
  }))
}
