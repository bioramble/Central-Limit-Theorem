############################################################################
# Bioramble
# Central Limit Theorem: Part 2
# by Jesse Lipp
# created: Oct 6, 2015
############################################################################

# --------------------------------------------------------------------------
# Set up environment
# --------------------------------------------------------------------------
# clean-up
rm(list = ls())

# --------------------------------------------------------------------------
# Normal distribution
# --------------------------------------------------------------------------
mean <- 4
sd <- 2
# plot sampling distribution
xs <- seq(-5, 15, by = 0.01)
ys <- dnorm(xs, mean, sd)
par(mfrow = c(1, 1))
plot(xs, ys, type = "l", col = "blue", lwd = 2, 
     xlab = "Domain", ylab = "Density", 
     main = "Normal distribution")

# simulate distribution of means using different 
# sample sizes
set.seed(1)
par(mfcol = c(2, 4))
for (sz in c(2, 5, 15, 30)) {
  # simulate means of random samples of size "sz"
  means <- replicate(1e4, mean(rnorm(sz, mean, sd)))
  means <- means[means >= -5 & means <= 10]
  # plot histogram of means of samples
  hist(means, prob = TRUE, breaks = seq(-5, 10, by = 0.25), 
       col = "ivory4", border = "white", 
       xlim = c(-5, 10), main = paste("n =", sz))
  # plot theoretical distribution of means
  # according to CLT
  x <- seq(-5, 10, by = 0.01)
  lines(x, dnorm(x, mean, sd/sqrt(sz)), 
        col = "blue", lwd = 2)
  qqnorm(means, pch = 16, cex = 0.75, 
         col = "ivory4", ylim = c(0, 10), main = "")
  qqline(means, col = "blue", lwd = 2)
}

# --------------------------------------------------------------------------
# Exponential distribution
# --------------------------------------------------------------------------
rate <- 0.25
# plot sampling distribution
xs <- seq(-5, 15, by = 0.01)
ys <- dexp(xs, rate)
par(mfrow = c(1, 1))
plot(xs, ys, type = "l", col = "blue", lwd = 2, 
     xlab = "Domain", ylab = "Density", 
     main = "Exponential distribution")

# simulate distribution of means using different 
# sample sizes
set.seed(1)
par(mfcol = c(2, 4))
for (sz in c(2, 5, 15, 30)) {
  # simulate means of random samples of size "sz"
  means <- replicate(1e4, mean(rexp(sz, rate)))
  means <- means[means >= -5 & means <= 10]
  # plot histogram of means of samples
  hist(means, prob = TRUE, breaks = seq(-5, 10, by = 0.25), 
       col = "ivory4", border = "white", 
       xlim = c(-5, 10), main = paste("n =", sz))
  # plot theoretical distribution of means
  # according to CLT
  x <- seq(-5, 10, by = 0.01)
  lines(x, dnorm(x, 1/rate, (1/rate)/sqrt(sz)), 
        col = "blue", lwd = 2)
  qqnorm(means, pch = 16, cex = 0.75, 
         col = "ivory4", ylim = c(0, 10), main = "")
  qqline(means, col = "blue", lwd = 2)
}

# --------------------------------------------------------------------------
# Crazy distribution
# --------------------------------------------------------------------------
# plot sampling distribution
xs <- seq(from = -10, to = 30, by = 0.001)
ys <- dnorm(xs) + dexp(xs, 0.25) + dunif(xs, min = -4, max = -2)
par(mfrow = c(1, 1))
plot(xs, ys, type = "l", col = "blue", lwd = 2, xlim = c(-5, 15), 
     xlab = "Domain", ylab = "Density", main = "Crazy distribution")

# determine mean and sd of crazy distribution
crazy <- sample(x = xs, size = 1e4, replace = TRUE, prob = ys)
mean <- mean(crazy)
sd <- sd(crazy)

# simulate distribution of means using different sample sizes
set.seed(1)
par(mfcol = c(2, 4))
for (sz in c(2, 5, 15, 30)) {
  # simulate means of random samples of size "sz"
  means <- replicate(1e4, mean(jitter(sample(x = xs, size = sz, replace = TRUE, prob = ys))))
  means <- means[means >= -5 & means <= 10]
  # plot histogram of means of samples
  hist(means, prob = TRUE, breaks = seq(-5, 10, by = 0.25), 
       col = "ivory4", border = "white", 
       xlim = c(-5, 10), main = paste("n =", sz))
  # plot theoretical distribution of means
  # according to CLT
  x <- seq(-5, 10, by = 0.01)
  lines(x, dnorm(x, mean, sd/sqrt(sz)), 
        col = "blue", lwd = 2)
  qqnorm(means, pch = 16, cex = 0.75, 
         col = "ivory4", ylim = c(-4, 10), main = "")
  qqline(means, col = "blue", lwd = 2)
}

# --------------------------------------------------------------------------
# Why use n = 30 as a rule of thumb?
# --------------------------------------------------------------------------
# plot sample size versus standard error
par(mfrow = c(1, 1))
plot(1/sqrt(seq(100)), type = "l", col = "blue", lwd = 3, 
     xlab = "Sample size", ylab = "Standard error", axes = FALSE)
axis(side = 1, at = seq(0, 100, by = 10))
axis(side = 2, at = seq(0, 1, by = 0.2))
box()
abline(v = 30, lty = 3, lwd = 2)
m <- lm(1/sqrt(seq(30, 100)) ~ seq(30, 100))
abline(m, lty = 3, lwd = 2)

