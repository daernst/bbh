# #' Plots SST data
# #' 
# #' @export
# #' @param x sst class tibble
# #' @param type character see \code{\link[base]{plot}}, defaults to 'l'
# #' @param y NULL or another tibble of class sst
# #' @param ... other arguments passed to \code{\link[base]{plot}}
# #' 
# #' @return NULL

# plot.sst <- function(x, y = NULL, type = "l", ...){
#   plot(x$COLLECTION_DATE, x$SEA_SURFACE_TEMP_AVG_C, type = type, ...)
# }

#' Plots SST group data from rbind.sst
#' 
#' @export
#' @param x sst class tibble
#' @param ... potential future other arguments passed to ggplot2
#' 
#' @return ggplot2 object

plot.sst_group <- function(x, ...){
  ggplot2::ggplot(x, ggplot2::aes(x = .data$COLLECTION_DATE,
                                  y = .data$SEA_SURFACE_TEMP_AVG_C,
                                  group = .data$name,
                                  col = .data$name)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$SEA_SURFACE_TEMP_MIN_C,
                                      ymax = .data$SEA_SURFACE_TEMP_MAX_C,
                                      fill = .data$name),
                         linetype = 0,
                         alpha = 0.4)
}


#' Plots SST data
#' 
#' @export
#' @param x sst class tibble
#' @param ... future other arguments passed to ggplot2
#' @param xlim date range for plotting
#' @return ggplot2 object

plot.sst <- function(x, xlim = range(x$COLLECTION_DATE, na.rm = TRUE), ...){
  if(!inherits(xlim, "Date")){xlim <- as.Date(xlim)}
  ggplot2::ggplot(x |> dplyr::filter(dplyr::between(.data$COLLECTION_DATE, xlim[1], xlim[2])),
                  ggplot2::aes(x = .data$COLLECTION_DATE,
                               y = .data$SEA_SURFACE_TEMP_AVG_C)) +
    ggplot2::xlim(xlim) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$SEA_SURFACE_TEMP_MIN_C,
                                      ymax = .data$SEA_SURFACE_TEMP_MAX_C),
                         linetype = 0,
                         alpha = 0.4) +
    ggplot2::labs(title = attr(x, "longname"),
         caption = paste("Source:", attr(x, "source")))
}