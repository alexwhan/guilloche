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
  ggplot(dat, aes_string(join_x, join_y)) +
    geom_point(size = 0.2, colour = "darkgrey") +
    xlim(get_lims(dat, "join_x")) +
    ylim(get_lims(dat, "join_y"))
}

#' Get axis limits to add to ggplot
#'
#' @param dat A dataframe
#' @param var A variable name
#'
#' @return numeric vector of length 2
#' @export
#'
get_lims <- function(dat, var) {
  range_var <- range(dat[[var]])
  lim_vec <- (range_var - mean(range_var)) * 1.1 + mean(range_var)
}

#' Draw basic state of drawing machine
#'
#' @param pantograph Object of class pantograph produced by `define_pantograph`
#'
#' @return ggplot objects
#' @export
#'
#' @examples
#' orbit1 <- define_orbit(c(0, 25), 1000)
#' orbit2 <- define_orbit(c(0, 8), 100, parent_orbit = orbit1)
#' orbit3 <- define_orbit(c(0, 1), 20, parent_orbit = orbit2)
#' pan <- define_pantograph(orbit3, orbit3, c(0, 1), c(0, 5), 3, 4)
#' draw_machine(pan)
draw_machine <- function(pantograph) {
  stopifnot(class(pantograph) == "pantograph")
  dat1 <- get_complete_position(pantograph$orbit1)
  dat1$orbit[dat1$orbit == "pantograph$orbit1"] <- pantograph$orbit1_name
  dat2 <- get_complete_position(pantograph$orbit2)
  dat2$orbit[dat2$orbit == "pantograph$orbit2"] <- pantograph$orbit2_name
  p <- ggplot(rbind(dat1, dat2), aes(x, y)) + geom_path(aes(colour = orbit), show.legend = FALSE) +
    geom_point(aes(colour = orbit)) +
    theme_void() +
    theme(legend.title = element_blank()) +
    coord_fixed() +
    labs(title = paste("Setup for", deparse(substitute(pantograph))),
         subtitle = "Path around which orbit centers will travel")
  print(p)
}
