#' Animate drawing of a drawr dataframe
#'
#' @param dat dataframe
#' @param x1 column name of x1
#' @param y1 column name of y1
#' @param x2 column name of x2
#' @param y2 column name of y2
#' @param join_x column name of join_x
#' @param join_y column name of join_y
#'
#' @return TRUE
#' @export
#' @import ggplot2
ani_drawr <- function(dat)
saveGIF({
  base_p <- ggplot()
  for(i in seq(1, 2500, length.out = 100)) {
    p <- ggplot(dat[i,]) +
      geom_point(data = dat[1:i,], aes(join_x, join_y), size = 2, colour = "darkgrey") +
      geom_segment(aes(x = x1, xend = join_x, y = y1, yend = join_y)) +
      geom_segment(aes(x = x2, xend = join_x, y = y2, yend = join_y)) +
      geom_point(aes(x1, y1), size = 2) +
      geom_point(aes(x2, y2), size = 2) +
      geom_point(aes(join_x, join_y), size = 2) +
      coord_fixed() +
      xlim(-12.5, 12.5) +
      ylim(-5, 20)
    print(p)
  }},
  "test.gif", interval = 0.1
)
