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

#' Get x of a hinged join between two points
#'
#' @param x1 x position of point1
#' @param y1 y position of point1
#' @param x2 x position of point2
#' @param y2 y position of point2
#' @param dist Distance from each point to the join
#'
#' @return numeric
#' @export
#'
get_join_x <- function(x1, y1, x2, y2, dist) {
  disp1 <- eucl_dist(x1, y1, x2, y2) / 2
  theta_diff <- get_theta_diff(x1, y1, x2, y2)
  theta2 <- acos(disp1 / dist)
  x <- cos(theta_diff + theta2) * dist + x1
  return(x)
}

#' Get y of a hinged join between two points
#'
#' @param x1 x position of point1
#' @param y1 y position of point1
#' @param x2 x position of point2
#' @param y2 y position of point2
#' @param dist Distance from each point to the join
#'
#' @return numeric
#' @export
#'
get_join_y <- function(x1, y1, x2, y2, dist) {
  disp1 <- eucl_dist(x1, y1, x2, y2) / 2
  theta_diff <- get_theta_diff(x1, y1, x2, y2)
  theta2 <- acos(disp1 / dist)
  y <- sin(theta_diff + theta2) * dist + y1
  return(y)
}

