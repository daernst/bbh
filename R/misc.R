#' Binds rows of SST data
#' 
#' @export
#' @param ... one of more class sst tibbles passed to base rbind
#' 
#' @return a tibble of class sst_group

rbind.sst <- function(...){
  #cat(str(...), '/n')
  x <- list(...)
  ## make list of sst tibbles
  if(length(x) == 1 && is.list(x[[1]])){
    x <- x[[1]]
  }
  ## harvest the attributes from individual tables
  a <- lapply(x,
              function(y){
                dplyr::tibble(name = attr(y, "name"),
                              longname = attr(y, "longname"),
                              source = attr(y, "source")) 
              }) |>
    dplyr::bind_rows()
  ## add names
  names(x) <- a$name
  ## bind tibbles
  x <- lapply(names(x),
              function(n){
                y <- dplyr::mutate(x[[n]], name = n, .before = 1)
                attr(y, "name") <- NULL
                attr(y, "longname") <- NULL
                attr(y, "source") <- NULL
                return(y)
              }) |>
    dplyr::bind_rows()
  attr(x, "info") <- a
  class(x) <- c("sst_group", class(x))
  return(x)
}




#' Binds rows of SST data in a list
#' 
#' @export
#' @param x a list of one or more sst class tibbles
#' 
#' @return a tibble of class sst_group

bind_rows_sst <- function(x){
  ## harvest the attributes from individual tables
  a <- lapply(x,
              function(y){
                dplyr::tibble(name = attr(y, "name"),
                              longname = attr(y, "longname"),
                              source = attr(y, "source")) 
              }) |>
    dplyr::bind_rows()
  ## add names
  names(x) <- a$name
  ## bind tibbles
  x <- lapply(names(x),
              function(n){
                y <- dplyr::mutate(x[[n]], name = n, .before = 1)
                attr(y, "name") <- NULL
                attr(y, "longname") <- NULL
                attr(y, "source") <- NULL
                return(y)
              }) |>
    dplyr::bind_rows()
  attr(x, "info") <- a
  class(x) <- c("sst_group", class(x))
  return(x)
}



