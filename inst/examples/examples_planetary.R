dat <- data_frame(period = 1:10000)
dat$theta1 <- get_theta(dat$period, 500)
dat$x1 <- get_x(dat$theta1, 6, -8)
dat$y1 <- get_y(dat$theta1, 6, 0)

dat$theta2 <- get_theta(dat$period, 76.3)
dat$x2 <- get_x(dat$theta2, 6, 8)
dat$y2 <- get_y(dat$theta2, 6, 0)

p1 <- ggplot(dat) +
  geom_point(aes(x1, y1), colour = "blue") +
  geom_point(aes(x2, y2), colour = "red")

dat$join_x <- get_join_x(dat$x1, dat$y1,
                         dat$x2, dat$y2, 30)

dat$join_y <- get_join_y(dat$x1, dat$y1,
                         dat$x2, dat$y2, 30)

p2 <- p1 +
  geom_path(data = dat, aes(join_x, join_y), colour = "green",
            size = 0.05) +
  coord_fixed()
p2
#Make one planetary
dat$theta1_p <- get_theta(-dat$period, 500) + dat$theta1
dat$x1_p <- get_x(dat$theta1_p, 2, dat$x1)
dat$y1_p <- get_y(dat$theta1_p, 2, dat$y1)

p3 <- p2 + geom_path(data = dat, aes(x1_p, y1_p), colour = "lightblue",
                      size = 0.1)

dat$join_x_p1 <- get_join_x(dat$x1_p, dat$y1_p,
                         dat$x2, dat$y2, 30)

dat$join_y_p1 <- get_join_y(dat$x1_p, dat$y1_p,
                         dat$x2, dat$y2, 30)

p4 <- p1 +
  geom_path(data = dat, aes(join_x_p1, join_y_p1), colour = "lightgreen") +
  coord_fixed()
p4
ggplot(dat[1:250,], aes(join_x_p1, join_y_p1)) +
  geom_point(size = 0.2, colour = "darkgrey", aes(alpha = period)) +
  # geom_path() +
  xlim(range(dat$join_x_p1) * 1.1) +
  ylim(((range(dat$join_y_p1) - mean(range(dat$join_y_p1))) * 1.1) +
         mean(range(dat$join_y_p1)))

#Make another planetary
dat$theta2_p <- get_theta(-dat$period, 10000) + dat$theta2
dat$x2_p <- get_x(dat$theta2_p, 1.5, dat$x2)
dat$y2_p <- get_y(dat$theta2_p, 1.5, dat$y2)

ggplot(dat[1:500,], aes(x2_p, y2_p)) + geom_point()

dat$join_x_p2 <- get_join_x(dat$x1_p, dat$y1_p,
                            dat$x2_p, dat$y2_p, 20)

dat$join_y_p2 <- get_join_y(dat$x1_p, dat$y1_p,
                            dat$x2_p, dat$y2_p, 20)

pp2 <- ggplot(dat, aes(join_x_p2, join_y_p2)) +
  geom_point(size = 0.2, colour = "darkgrey") +
  geom_path() +
  # xlim(range(dat$join_x_p2) * 1.1) +
  # ylim(((range(dat$join_y_p2) - mean(range(dat$join_y_p2))) * 1.1) +
  #        mean(range(dat$join_y_p2))) +
  geom_point(aes(x1_p, y1_p)) +
  geom_point(aes(x2_p, y2_p), colour = "red")
print(pp2)
#This one worked pretty well
#dat_old <- dat

dat$hinge_x1 <- with(dat, get_hinge_x(x1, y1, x2, y2, 40, 0.5))
dat$hinge_y1 <- with(dat, get_hinge_y(x1, y1, x2, y2, 40, 0.5))
dat$hinge_x2 <- with(dat, get_hinge_x(x2, y2, x1, y1, 40, 0.5))
dat$hinge_y2 <- with(dat, get_hinge_y(x2, y2, x1, y1, 40, 0.5))

get_hinge_x1 <- function(x1, y1, x2, y2, dist, join_prop) {
  if(!(join_prop > 0 & join_prop < 1)) stop("join_prop needs to be between 0 and 1")
  browser()
  disp1 <- eucl_dist(x1, y1, x2, y2) / 2
  theta_diff <- get_theta_diff(x1, y1, x2, y2)
  theta2 <- acos(disp1 / (dist * join_prop))
  x <- cos(theta_diff + theta2) * dist  + x1
  return(x)

}

dat <- within(dat, {
  join2_x <- get_join_x(hinge_x1, hinge_y1, hinge_x2, hinge_y2, dist = 40)
  join2_y <- get_join_y(hinge_x1, hinge_y1, hinge_x2, hinge_y2, dist = 40)
})

ggplot(dat[1:1000,], aes(join2_x, join2_y)) +
  geom_point()
