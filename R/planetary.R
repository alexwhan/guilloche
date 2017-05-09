#' Get x coordinates of planetary location
#'
#' @param theta Angle in radians
#' @param rad radius
#' @param offset_x x position of center
#'
#' @return numeric
#' @export
#'
#' @examples
#' get_x(pi / 6, 1, 0)
get_x <- function(theta, rad, offset_x) {
  cos(theta) * rad + offset_x
}

#' Get y coordinates of planetary location
#'
#' @param theta Angle in radians
#' @param rad radius
#' @param offset_y y position of center
#'
#' @return numeric
#' @export
#'
#' @examples
#' get_y(pi / 6, 1, 0)
get_y <- function(theta, rad, offset_y) {
  sin(theta) * rad + offset_y
}

#' Get theta of disc
#'
#' @param period Number of periods since beginning
#' @param frequency Number of periods taken for a complete revolution
#'
#' @return angle in radians
#' @export
#'
#' @examples
#' get_theta(10, 100)
get_theta <- function(period, frequency) {
  (2 * pi / frequency) * (period %% frequency)
}

#' Get euclidean distance between two points
#'
#' @param x1 x position of point 1
#' @param y1 y position of point 1
#' @param x2 x position of point 2
#' @param y2 y position of point 2
#'
#' @return numeric
#' @export
#'
#' @examples
#' eucl_dist(0, 0, 1, 1)
eucl_dist <- function(x1, y1, x2, y2) {
  x_disp <- abs(x1 - x2)
  y_disp <- abs(y1 - y2)
  (x_disp ^ 2 + y_disp ^ 2) ^ 0.5
}

#' Get angle of point 2 from point 1
#'
#' @param x1 x position of point 1
#' @param y1 y position of point 1
#' @param x2 x position of point 2
#' @param y2 y position of point 2
#'
#' @return numeric
#' @export
#'
#' @examples
#' get_theta_diff(0, 0, 1, 1)
get_theta_diff <- function(x1, y1, x2, y2) {
  atan2(y2 - y1, x2 - x1)
}

#' Get offset from the plane of two points for a given scissor set up
#'
#' @param x1 x position of point 1
#' @param y1 y position of point 1
#' @param x2 x position of point 2
#' @param y2 y position of point 2
#' @param segment_length The length of scissor segments
#' @param segment_number The number of scissor segments
#'
#' @return numeric
#' @export
#'
#' @examples
#' get_scissor_offset(0, 0, 1, 1, 3, 1)
get_scissor_offset <- function(x1, y1, x2, y2, segment_length, segment_number) {
  mid_dist <- eucl_dist(x1, y1, x2, y2) / 2
  seg_offset <- (segment_length ^ 2 - mid_dist ^ 2) ^ 0.5
  return(seg_offset * segment_number)
}

#' Get x of drawing point
#'
#' @param x1 x position of point 1
#' @param y1 y position of point 1
#' @param x2 x position of point 2
#' @param y2 y position of point 2
#' @param segment_length The length of scissor segments
#' @param segment_number The number of scissor segments
#'
#' @return numeric
#' @export
#'
#' @examples
#' get_scissor_offset(0, 0, 1, 1, 2, 3)
get_drawing_point <- function(x1, y1, x2, y2, segment_length, segment_number) {
  mid_point_x <- mean(c(x1, x2))
  mid_point_y <- mean(c(y1, y2))
  offset <- get_scissor_offset(x1, y1, x2, y2, segment_length, segment_number)
  theta <- get_theta_diff(x1, y1, x2, y2)
  drawing_x <- offset * cos((pi / 2) - theta) + mid_point_x
  drawing_y <- offset * sin((pi / 2) - theta) + mid_point_y
  return(c(drawing_x, drawing_y))
}
