#' Plot a drawr dataframe
#'
#' @param dat dataframe
#' @param x1 column name of x1
#' @param y1 column name of y1
#' @param x2 column name of x2
#' @param y2 column name of y2
#' @param join_x column name of join_x
#' @param join_y column name of join_y
#'
#' @return a ggplot2 object
#' @export
#' @import ggplot2
ggdrawr_whole <- function(dat, x1 = "x1", y1 = "y1", x2 = "x2", y2 = "y2",
                    join_x = "join_x", join_y = "join_y", print = TRUE) {
  p <- ggplot(dat) +
    geom_point(aes_string(x1, y1), colour = "darkgrey") +
    geom_point(aes_string(x2, y2), colour = "darkgrey") +
    geom_point(aes_string(join_x, join_y), size = 0.2, colour = "darkgrey") +
    coord_fixed()
  if(print) print(p)
  return(p)
}

#' Plot a drawr dataframe
#'
#' @param dat dataframe
#' @param join_x column name of join_x
#' @param join_y column name of join_y
#'
#' @return a ggplot2 object
#' @export
#' @import ggplot2
ggdrawr <- function(dat, join_x = "join_x", join_y = "join_y", print = TRUE) {
  range_x <- range(dat[[join_x]])
  range_y <- range(dat[[join_y]])
  xlim_vec <- range_x - mean(range_x) * 1.1 + mean(range_x)
  ylim_vec <- range_y - mean(range_y) * 1.1 + mean(range_y)
  geom_point(size = 0.2, colour = "darkgrey") +
    geom_path() +
    xlim(xlim_vec) +
    ylim(ylim_vec)
}

