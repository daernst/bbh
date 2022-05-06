#' Binds rows of SST data
#' 
#' @export
#' @param ... one of more class sst tibbles passed to base rbind
#' 
#' @return a tibble of class sst_group

rbind.sst <- function(...){
  x <- list(...)
  a <- lapply(x,
              function(y){
                dplyr::tibble(name = attr(y, "name"),
                              longname = attr(y, "longname"),
                              source = attr(y, "source")) 
              }) |>
    dplyr::bind_rows()
  names(x) <- a$name
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