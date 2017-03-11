library(tidyr)
library(dplyr)
library(ggplot2)
dat <- data_frame(period = 1:10000)
dat$theta1 <- get_theta(dat$period, 112.6)
dat$x1 <- get_x(dat$theta1, 1, -8)
dat$y1 <- get_y(dat$theta1, 1, 0)

dat$theta2 <- get_theta(dat$period, 188.7)
dat$x2 <- get_x(dat$theta2, 1, 8)
dat$y2 <- get_y(dat$theta2, 1, 0)


dat$join_x <- get_join_x(dat$x1, dat$y1,
                         dat$x2, dat$y2, 18)

dat$join_y <- get_join_y(dat$x1, dat$y1,
                         dat$x2, dat$y2, 18)

samples <- sample(1:nrow(dat), 3)
ggplot(dat) +
  geom_point(aes(x1, y1), colour = "darkgrey") +
  geom_point(aes(x2, y2), colour = "darkgrey") +
  geom_point(aes(join_x, join_y), size = 0.2, colour = "darkgrey") +
  geom_point(data = dat[samples,],
             aes(colour = factor(period), x1, y1),
             size = 3) +
  geom_point(data = dat[samples,],
             aes(colour = factor(period), x2, y2),
             size = 3) +
  geom_point(data = dat[samples,],
             aes(colour = factor(period), join_x, join_y),
             size = 3) +
  coord_fixed()

ggplot(dat, aes(join_x, join_y)) +
  geom_point(size = 0.2, colour = "darkgrey") +
  geom_path() +
  xlim(range(dat$join_x) * 1.1) +
  ylim(((range(dat$join_y) - mean(range(dat$join_y))) * 1.1) +
         mean(range(dat$join_y)))

dat$euc_1 <- with(dat, {
  eucl_dist(x1, y1, join_x, join_y)
})


dat$euc_2 <- with(dat, {
  eucl_dist(x2, y2, join_x, join_y)
})


x1 <- dat$x1
y1 <- dat$y1
x2 <- dat$x2
y2 <- dat$y2
dist <- 4

disp1 <- eucl_dist(x1, y1, x2, y2) / 2
theta_diff <- get_theta_diff(x1, y1, x2, y2)
theta2 <- acos(disp1 / dist)
x <- cos(theta_diff + theta2) * dist + x1
