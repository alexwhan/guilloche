#' Get x coordinates of orbit location
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

#' Get y coordinates of orbit location
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

#' Get (x, y) position of orbit location
#'
#' @param theta Angle in radians
#' @param orbit Object of class orbit
#'
#' @return numeric vector
#' @export
#'
get_orbit_position <- function(period_range, orbit) {
  stopifnot(class(parent_orbit) == "character")
  stopifnot(class(orbit) == "orbit")
  if(!is.null(orbit$parent_orbit)) {
    if(exists(orbit$parent_orbit)) {
      parent_orbit <- get(parent_orbit)
      offset_tbl <- get_orbit_position(period_range, parent_orbit)
    } else stop(paste("Parent orbit", orbit$parent_orbit, "does not exist"))
  }

  theta <- get_theta(period_range, orbit$speed)

  x <- get_x(theta, orbit$radius, orbit$offset[1])
  y <- get_y(theta, orbit$radius, orbit$offset[2])
  orbit_name <- deparse(substitute(orbit))
  out_df <- tibble(x = x, y = y)
  setNames(out_df, paste0(names(out_df), "_", orbit_name))
}

get_parent_offset <- function(orbit) {
  stopifnot(class(orbit) == "orbit")
  if(!is.null(orbit$parent_orbit)) {
    if(exists(orbit$parent_orbit)) {
      parent_offset <- get(orbit$parent_orbit)$offset + get_parent_offset(get(orbit$parent_orbit))
      return(parent_offset)
    } else {
      stop("Parent orbit is named but doesn't exist")
    }
  } else return(c(0, 0))
}

#' Get theta of disc
#'
#' @param period Number of periods since beginning
#' @param speed Number of periods taken for a complete revolution
#'
#' @return angle in radians
#' @export
#'
#' @examples
#' get_theta(10, 100)
get_theta <- function(period, speed) {
  (2 * pi / speed) * (period %% speed)
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
#' @param return a string specifying whether to return "x", "y" or "both"
#'
#' @return numeric
#' @export
#'
#' @examples
#' get_scissor_offset(0, 0, 1, 1, 2, 3)
get_drawing_point <- function(x1, y1, x2, y2, segment_length, segment_number,
                              return = "both") {
  mid_point_x <- x1 - (x1 - x2) / 2
  mid_point_y <- y1 - (y1 - y2) / 2
  offset <- get_scissor_offset(x1, y1, x2, y2, segment_length, segment_number)
  theta <- get_theta_diff(x1, y1, x2, y2)
  drawing_x <- offset * cos((pi / 2) - theta) + mid_point_x
  drawing_y <- offset * sin((pi / 2) - theta) + mid_point_y
  if(return == "x") return(drawing_x)
  if(return == "y") return(drawing_y)
  else return(c(drawing_x, drawing_y))
}

#' Transform a position by orbiting around an origin
#'
#' @param x1 x position of point to be transformed
#' @param y1 y position of point to be transformed
#' @param x2 x position of origin
#' @param y2 y position of origin
#' @param theta Angle in radians
#' @param return a string specifying whether to return "x", "y" or "both"
#'
#' @return numeric
#' @export
#'
#' @examples
#' orbit_transform(3,2,0,1,pi/4)
orbit_transform <- function(x1, y1, x2, y2, theta, return = "both") {
  theta_origin <- get_theta_diff(x1, y1, x2, y2)
  theta_final <- theta_origin + theta
  dist_origin <- eucl_dist(x1, y1, x2, y2)
  x_origin <- x1 - x2
  new_x_origin <- cos(theta_final) * dist_origin
  y_origin <- y1 - y2
  new_y_origin <- sin(theta_final) * dist_origin
  new_x <- x1 - (x_origin - new_x_origin)
  new_y <- y1 - (y_origin + new_y_origin)
  if(return == "x") return(new_x)
  if(return == "y") return(new_y)
  else return(c(new_x, new_y))
}
