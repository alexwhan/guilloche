#' Define a planetary orbit
#'
#' @param offset The position of the orbit center relative to the parent orbit center
#' @param speed The number of periods taken for a complete revolution
#' @param pantograph_point The position of the pantograph attachment relative to the orbit center
#' @param parent_orbit Name of an orbit object produced by `define_orbit()`
#'
#' @export
define_orbit <- function(offset, speed, parent_orbit = NULL) {
  stopifnot(is.numeric(offset))
  stopifnot(length(offset) == 2)
  stopifnot(is.numeric(speed))
  stopifnot(speed >= 0)
  if(!is.null(parent_orbit)) {
    if(class(parent_orbit) == "character") stop("The parent_orbit argument should be an unquoted object name")
    parent_orbit_name <- deparse(substitute(parent_orbit))
    if(!exists(parent_orbit_name)) message("Parent orbit object doesn't exist, this orbit will be defined without a parent")
    else stopifnot(class(parent_orbit) == "orbit")
  }
  orbit <- list(
    offset = offset,
    speed = speed
  )

  if(!is.null(parent_orbit)) orbit$parent_orbit <- parent_orbit_name
  class(orbit) <- c("orbit")
  return(orbit)
}

check_orbit <- function(orbit) {
  stopifnot(class(orbit) == "orbit")
  if(!is.null(orbit$parent_orbit)) stopifnot(check_orbit(get(orbit$parent_orbit)))
  return(TRUE)
}

#' Define a pantograph
#'
#' @param orbit1 The first orbit it is attached to
#' @param orbit2 The second orbit it is attached to
#' @param offset1 The distance of the attachment from the origin of orbit1
#' @param offset2 The distance of the attachment from the origin of orbit2
#' @param n_segments The number of segments in the pantograph
#' @param segment_length The length of each segment
#'
#' @return
#' @export
#'
#' @examples
define_pantograph <- function(orbit1, orbit2, offset1 = 0, offset2 = 0, n_segments = 3, segment_length = 1) {
  stopifnot(class(orbit1) == "orbit" & class(orbit2) == "orbit")
  stopifnot(class(offset1) == "numeric" & class(offset2) == "numeric")
  # stopifnot(class(n_segments) == "integer")
  # stopifnot(class(segment_length) == "numeric")
  stopifnot(segment_length > 0 & n_segments > 0)
  stopifnot(exists(deparse(substitute(orbit1))))
  stopifnot(exists(deparse(substitute(orbit2))))
  stopifnot(check_orbit(orbit1) & check_orbit(orbit2))
  pg <- list(orbit1 = orbit1,
             orbit1_name = deparse(substitute(orbit1)),
             orbit2 = orbit2,
             orbit2_name = deparse(substitute(orbit2)),
             offset1 = offset1,
             offset2 = offset2,
             n_segments = n_segments,
             segment_length = segment_length)
  class(pg) <- "pantograph"
  return(pg)
}

#' Update a pantograph
#'
#' @param pantograph Object of class pantograph, produced by `define_pantograph()`
#' @param orbit1 Optional, object of class orbit
#' @param orbit2 Optional, object of class orbit
#' @param offset1 Optional, the distance of the attachment from the origin of orbit1
#' @param offset2 Optional, the distance of the attachment from the origin of orbit2
#' @param n_segments Optional, the number of segments in the pantograph
#' @param segment_length Optional, the length of each segment
#'
#' @return pantograph
#' @export
#'
update_pantograph <- function(pantograph, orbit1 = NULL, orbit2 = NULL, offset1 = NULL, offset2 = NULL, n_segments = NULL, segment_length = NULL) {
  if(!exists(pantograph$orbit1_name)) stop(paste("Orbit", pantograph$orbit1_name, "does not exist"))
  if(!exists(pantograph$orbit2_name)) stop(paste("Orbit", pantograph$orbit2_name, "does not exist"))
  if(is.null(orbit1)) pantograph$orbit1 <- get(pantograph$orbit1_name) else {
    pantograph$orbit1 <- orbit1
    pantograph$orbit1_name <- deparse(substitute(orbit1))
  }
  if(is.null(orbit2)) pantograph$orbit2 <- get(pantograph$orbit2_name) else {
    pantograph$orbit2 <- orbit2
    pantograph$orbit2_name <- deparse(substitute(orbit2))
  }
  if(!is.null(offset1)) pantograph$offset1 <- offset1
  if(!is.null(offset2)) pantograph$offset2 <- offset2
  if(!is.null(n_segments)) pantograph$n_segments <- n_segments
  if(!is.null(segment_length)) pantograph$segment_length <- segment_length
  return(pantograph)
}
