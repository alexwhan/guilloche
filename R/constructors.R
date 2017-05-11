#' Define a planetary orbit
#'
#' @param offset The position of the orbit center relative to the parent orbit center
#' @param speed The number of periods taken for a complete revolution
#' @param pantograph_point The position of the pantograph attachment relative to the orbit center
#' @param parent_orbit Name of an orbit object produced by `define_orbit()`
#'
#' @export
define_orbit <- function(offset, speed, parent_orbit = NULL, pantograph_point = NULL) {
  stopifnot(is.numeric(offset))
  stopifnot(length(offset) == 2)
  stopifnot(is.numeric(speed))
  stopifnot(speed >= 0)
  if(!is.null(pantograph_point)) {
    stopifnot(length(pantograph_point) == 2)
    stopifnot(class(pantograph_point) == "numeric")
  }
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

#' Check an orbit is legal
#'
#' @param orbit An object of class orbit
check_orbit <- function(orbit) {

}
