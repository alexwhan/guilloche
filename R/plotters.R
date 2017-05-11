#' Get (x, y) position of orbit center
#'
#' @param period_range Integer vector of periods
#' @param orbit Object of class orbit
#'
#' @return numeric vector
#'
get_orbit_position <- function(period_range, orbit) {
  stopifnot(class(period_range) == "integer")
  stopifnot(class(orbit) == "orbit")

  if(!is.null(orbit$parent_orbit)) {
    if(exists(orbit$parent_orbit)) {
      stopifnot(class(get(orbit$parent_orbit)) == "orbit")
      theta <- get_theta(period_range, orbit)
      browser()

      parent_orbit_centre <- get_orbit_position(period_range, get(orbit$parent_orbit))

      x <- cos(theta) * eucl_dist(orbit$offset, c(0, 0)) + parent_orbit_centre$x
      y <- sin(theta) * eucl_dist(orbit$offset, c(0, 0)) + parent_orbit_centre$y

      return(tibble(x = x, y = y))
    } else stop(paste("Parent orbit", orbit$parent_orbit, "does not exist"))
  } else {

    x <- rep(orbit$offset[1], length(period_range))
    y <- rep(orbit$offset[2], length(period_range))
    orbit_name <- deparse(substitute(orbit))
    out_df <- tibble(x = x, y = y)
    # setNames(out_df, paste0(names(out_df), "_", orbit_name))
  }
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

#' Get theta of orbit
#'
#' @param period_range Integer vector of periods
#' @param orbit An object of class orbit
#'
#' @return angle in radians
#'
#' @examples
#' get_theta(10, 100)
get_orbit_theta <- function(period_range, orbit) {
  stopifnot(class(period_range) == "integer")
  stopifnot(class(orbit) == "orbit")
  base_theta <- (2 * pi / orbit$speed) * (period_range %% orbit$speed)
  if(!is.null(orbit$parent_orbit)) {
    if(exists(orbit$parent_orbit)) {
      parent_theta <- get_theta(period_range, get(orbit$parent_orbit))
      return((parent_theta + base_theta) %% (2 * pi))
    } else {
      stop("Parent orbit is named but doesn't exist")
    }
  } else {
    return(base_theta)
  }
}

get_orbit_origin <- function(period_range, orbit) {
  stopifnot(class(period_range) == "integer")
  stopifnot(class(orbit) == "orbit")

  if(!is.null(orbit$parent_orbit)) {
    if(exists(orbit$parent_orbit)) {
      origin_df <- get_orbit_origin(get(orbit$parent_orbit))
    } else {
      stop("Parent orbit is named but doesn't exist")
    }
  } else {
    theta <- get_theta(period_range, orbit)

    return()
  }
}
#' Get euclidean distance between two points
#'
#' @param pos1 A numeric vector of length 2 defining the first position
#' @param pos2 A numeric vector of length 2 defining the second position
#'
#' @return numeric
#'
#' @examples
#' eucl_dist(c(0, 0), c(1, 1))
eucl_dist <- function(pos1, pos2) {
  stopifnot(length(pos1) == 2 & length(pos2) == 2)
  stopifnot(class(pos1) == "numeric" & class(pos2) == "numeric")
  x_disp <- abs(pos1[1] - pos2[1])
  y_disp <- abs(pos1[2] - pos2[2])
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
