#' Define a planetary orbit
#'
#' @param radius The radius of the orbit around its origin
#' @param offset The position of the orbit center relative to the absolute origin
#' @param speed The number of periods taken for a complete revolution
#' @param parent_orbit
#'
#' @export
define_orbit <- function(radius, offset, speed, parent_orbit = NULL) {
  stopifnot(is.numeric(radius))
  stopifnot(is.numeric(offset))
  stopifnot(length(offset) == 2)
  stopifnot(is.numeric(speed))
  stopifnot(speed >= 0)
  if(!is.null(parent_orbit)) stopifnot(class(parent_orbit) == "orbit")
  orbit <- list(
    radius = radius,
    offset = offset,
    speed = speed
  )

  if(!is.null(parent_orbit)) orbit$parent_orbit <- parent_orbit
  class(orbit) <- c("orbit")
  return(orbit)
}

