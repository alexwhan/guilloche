library(dplyr)
dat <- data_frame(period = 1:10000)
dat$theta1 <- get_theta(dat$period, 100)
dat$x1 <- get_x(dat$theta1, 8, -8)
dat$y1 <- get_y(dat$theta1, 8, 0)

dat$theta2 <- get_theta(1001:11000, 88)
dat$x2 <- get_x(dat$theta2, 3, 8)
dat$y2 <- get_y(dat$theta2, 3, 0)

dat$drawing_point_x2 <- get_drawing_point(dat$x1, dat$y1, dat$x2, dat$y2, 26, 1, return = "x")
dat$drawing_point_y2 <- get_drawing_point(dat$x1, dat$y1, dat$x2, dat$y2, 26, 1, return = "y")

#rotate base
orbit_origin <- c(0, 20)
dat$theta_base <- get_theta(dat$period, 805000)
dat$x_r <- orbit_transform(dat$drawing_point_x2, dat$drawing_point_y2,
                           orbit_origin[1], orbit_origin[2], dat$theta_base, "x")
dat$y_r <- orbit_transform(dat$drawing_point_x2, dat$drawing_point_y2,
                           orbit_origin[1], orbit_origin[2], dat$theta_base, "y")
ggplot(dat, aes(x_r, y_r)) + geom_path(size = 0.3, aes(colour = period)) +
  scale_colour_viridis() +
  theme_bw()
ggplot(dat, aes(x_t, y_t)) + geom_path()
ggplot(dat, aes(x_r, y_r)) + geom_point(size = 0.5)
ggplot(dat, aes(drawing_point_x2, drawing_point_y2)) + geom_path(size = 0.3)
