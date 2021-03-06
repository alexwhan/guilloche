#' Get (x, y) position of orbit center
#'
#' @param period_range Integer vector of periods
#' @param orbit Object of class orbit
#' @param top_orbit logical. Is this the highest level orbit?
#' @param relative logical. Should positions be calculated relative to parent orbits?
#'
#' @return numeric vector
#'
get_orbit_position <- function(orbit, period_range = 1:100, top_orbit = TRUE) {
  stopifnot(class(period_range) == "integer")
  stopifnot(class(orbit) == "orbit")

  if(!is.null(orbit$parent_orbit)) {
    if(exists(orbit$parent_orbit)) {
      stopifnot(class(get(orbit$parent_orbit)) == "orbit")
      parent_orbit_centre <- get_orbit_position(get(orbit$parent_orbit), period_range, FALSE)
      parent_orbit_centre$period <- NULL

      new_x <- paste0("x_", orbit$parent_orbit)
      new_y <- paste0("y_", orbit$parent_orbit)

      names(parent_orbit_centre)[names(parent_orbit_centre) == "x"] <- new_x
      names(parent_orbit_centre)[names(parent_orbit_centre) == "y"] <- new_y

      theta <- get_orbit_theta(get(orbit$parent_orbit), period_range) + get_initial_theta(orbit)

      x <- cos(theta) * eucl_dist(orbit$offset, c(0, 0)) + parent_orbit_centre[[paste0("x_", orbit$parent_orbit)]]
      y <- sin(theta) * eucl_dist(orbit$offset, c(0, 0)) + parent_orbit_centre[[paste0("y_", orbit$parent_orbit)]]

      orbit_center <- tibble(period = period_range, x = x, y = y)
      if(top_orbit){
        new_x <- paste0("x_", deparse(substitute(orbit)))
        new_y <- paste0("y_", deparse(substitute(orbit)))
        names(orbit_center)[names(orbit_center) == "x"] <- new_x
        names(orbit_center)[names(orbit_center) == "y"] <- new_y

      }
      output_df <- dplyr::bind_cols(orbit_center, parent_orbit_centre)
      return(output_df)
    } else stop(paste("Parent orbit", orbit$parent_orbit, "does not exist"))
  } else {

    x <- rep(orbit$offset[1], length(period_range))
    y <- rep(orbit$offset[2], length(period_range))
    out_df <- tibble(x = x, y = y)
  }
}

get_machine_positions <- function(pantograph, period_range = 1:100){
  stopifnot(class(period_range) == "integer")
  stopifnot(class(pantograph) == "pantograph")
  orbit1_pos <- get_orbit_position(pantograph$orbit1, period_range)
  names(orbit1_pos)[2:3] <- paste0(c("x", "y"), "_", pantograph$orbit1_name)

  orbit2_pos <- get_orbit_position(pantograph$orbit2, period_range)
  names(orbit2_pos)[2:3] <- paste0(c("x", "y"), "_", pantograph$orbit2_name)

  anchor1_theta <- get_orbit_theta(pantograph$orbit1, period_range)
  x_anchor1 <- cos(anchor1_theta) * eucl_dist(pantograph$offset1, c(0, 0)) + orbit1_pos[[paste0("x_", pantograph$orbit1_name)]]
  y_anchor1 <- sin(anchor1_theta) * eucl_dist(pantograph$offset1, c(0, 0)) + orbit1_pos[[paste0("y_", pantograph$orbit1_name)]]

  anchor2_theta <- get_orbit_theta(pantograph$orbit2, period_range)
  x_anchor2 <- cos(anchor2_theta) * eucl_dist(pantograph$offset2, c(0, 0)) + orbit2_pos[[paste0("x_", pantograph$orbit2_name)]]
  y_anchor2 <- sin(anchor2_theta) * eucl_dist(pantograph$offset2, c(0, 0)) + orbit2_pos[[paste0("y_", pantograph$orbit2_name)]]

  orbit_pos <- dplyr::bind_cols(orbit1_pos, orbit2_pos,
                                tibble(x_anchor1 = x_anchor1, y_anchor1 = y_anchor1,
                                       x_anchor2 = x_anchor2, y_anchor2 = y_anchor2))
  orbit_pos <- orbit_pos[,unique(names(orbit_pos))]

  orbit_pos_g <- tidyr::gather(orbit_pos, key, value, -period)
  orbit_pos_g <- tidyr::separate(orbit_pos_g, key, c("axis", "orbit"), sep = "_", extra = "merge")
  orbit_pos <- tidyr::spread(orbit_pos_g, axis, value)

  return(orbit_pos)
}

#' Get positions of complete orbit at theta = 0
#'
#' @param orbit
#'
#' @return
#'
get_complete_position <- function(orbit, top_orbit = TRUE) {
  stopifnot(class(orbit) == "orbit")

  if(!is.null(orbit$parent_orbit)) {
    if(exists(orbit$parent_orbit)) {
      stopifnot(class(get(orbit$parent_orbit)) == "orbit")
      theta <- seq(0, pi * 2, length.out = 100)
      parent_orbit_centre <- get_complete_position(get(orbit$parent_orbit), FALSE)
      parent_orbit_centre$orbit[is.na(parent_orbit_centre$orbit)] <- orbit$parent_orbit

      x <- cos(theta) * eucl_dist(orbit$offset, c(0, 0)) +
        get_total_offset(get(orbit$parent_orbit))[1]
      y <- sin(theta) * eucl_dist(orbit$offset, c(0, 0)) +
        get_total_offset(get(orbit$parent_orbit))[2]

      out_df <- tibble(x = x, y = y)
      if(top_orbit) out_df$orbit <- deparse(substitute(orbit)) else {
        out_df$orbit <- NA
      }

      out_df <- dplyr::bind_rows(parent_orbit_centre, out_df)
      return(out_df)
    } else stop(paste("Parent orbit", orbit$parent_orbit, "does not exist"))
  } else {

    x <- orbit$offset[1]
    y <- orbit$offset[2]
    out_df <- tibble(x = x, y = y, orbit = NA)
  }
}

get_total_offset <- function(orbit) {
  if(!is.null(orbit$parent_orbit)) {
    return(get_total_offset(get(orbit$parent_orbit)) + orbit$offset)
  }
  return(orbit$offset)
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
get_orbit_theta <- function(orbit, period_range) {
  stopifnot(class(period_range) == "integer")
  stopifnot(class(orbit) == "orbit")
  base_theta <- (2 * pi / orbit$speed) * (period_range %% orbit$speed)
  if(!is.null(orbit$parent_orbit)) {
    if(exists(orbit$parent_orbit)) {
      parent_theta <- get_orbit_theta(get(orbit$parent_orbit), period_range)
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
#' @param pos1 The first position, numeric length 2
#' @param pos2 The second position, numeric length 2
#'
#' @return numeric
#' @export
#'
#' @examples
#' get_theta_diff(c(0, 0), c(1, 1))
get_theta_diff <- function(pos1, pos2) {
  atan2(pos2[2] - pos1[2], pos2[1] - pos1[1])
}

#' Get offset from the plane of anchor points for a given pantograph set up
#'
#' @param pos1 First anchor point of pantograph
#' @param pos2 Second anchor point of pantograph
#' @param segment_length The length of scissor segments
#' @param segment_number The number of scissor segments
#'
#' @return numeric
#' @examples
#' get_pantograph_distance(c(0, 0), c(1, 1), 3, 1)
get_pantograph_distance <- function(pos1, pos2, segment_length, segment_number) {
  mid_dist <- eucl_dist(pos1, pos2) / 2
  seg_offset <- (segment_length ^ 2 - mid_dist ^ 2) ^ 0.5
  return(seg_offset * segment_number)
}

#' Get position of drawing point
#'
#' @param pantograph An object of class pantograph
#'
#' @return A tibble
#' @export
#'
#' @examples
#' orbit_base <- define_orbit(c(0, 0), 1000)
#' orbit1 <- define_orbit(c(8, 0), 100, parent_orbit = orbit_base)
#' orbit2 <- define_orbit(c(1, 0), 20, parent_orbit = orbit_base)
#' orbit3 <- define_orbit(c(-8, 0), 90, orbit_base)
#' orbit4 <- define_orbit(c(-1, 0), 200, orbit3)
#' pan1 <- define_pantograph(orbit2, orbit4, c(0, 1), c(0, 5), 3, 4)
#' dp <- get_drawing_point(pan1)
get_drawing_point <- function(pantograph, period_range = 1:100) {
  pos_dat <- get_machine_positions(pantograph, period_range)
  pos_dat <- dplyr::filter(pos_dat, orbit %in% c("anchor1", "anchor2"))
  pos_dat <- dplyr::group_by(pos_dat, period)
  pos_dat <- tidyr::gather(pos_dat, axis, value, x, y)
  pos_dat <- tidyr::unite(pos_dat, var, axis, orbit)
  pos_dat <- tidyr::spread(pos_dat, var, value)
  pos_dat <- dplyr::mutate(pos_dat,
                           pan_offset = guilloche:::get_pantograph_distance(c(x_anchor1, y_anchor1),
                                                                               c(x_anchor2, y_anchor2),
                                                                            pantograph$segment_length,
                                                                            pantograph$n_segments),
                           mid_point_x = x_anchor1 - (x_anchor1 - x_anchor2) / 2,
                           mid_point_y = y_anchor2 - (y_anchor2 - y_anchor2) / 2,
                           theta = get_theta_diff(c(x_anchor1, y_anchor1), c(x_anchor2, y_anchor2)),
                           drawing_x = pan_offset * cos(( pi / 2 ) - theta) + mid_point_x,
                           drawing_y = pan_offset * sin(( pi / 2 ) - theta) + mid_point_y)
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

#' Get the starting angle of an orbit relative to its parent
#'
#' @param orbit Object of class orbit
#'
#' @return Angle in radians
#'
#' @examples
#' orbit_base <- define_orbit(c(25, 0), 1000)
#' orbit2 <- define_orbit(c(8, 8), 100, parent_orbit = orbit_base)
#' get_initial_theta(orbit2)
get_initial_theta <- function(orbit) {
  get_theta_diff(c(0, 0), orbit$offset)
}
