library(ISLR2)
fit <- lm(nox ~ poly(dis, 3), data = Boston)
dis.grid <- seq(min(Boston$dis), max(Boston$dis), length.out = 100)
pred <- predict(fit, newdata = list(dis = dis.grid))
plot(Boston$dis, Boston$nox, xlab = "dis", ylab = "nox")
lines(dis.grid, pred, lwd=2, col="brown3")

summary(fit)

rss <- c()
colours <- c(
  "orange", "red", "lightblue", "green", "brown", "purple",
  "pink", "lightgoldenrod", "violet", "magenta", "darkblue"
)
plot(Boston$dis, Boston$nox, xlab = "dis", ylab = "nox", ylim=c(0.3, 0.9))
for (i in 1:10) {
  fit <- lm(nox ~ poly(dis, i, raw = TRUE), data = Boston)
  rss[i] <- sum(fit$residuals^2)
  pred <- predict(fit, newdata = data.frame(dis = dis.grid))
  lines(dis.grid, pred, col = colours[i])
}

print(rss)

set.seed(1)
cv.error <- c()
for (i in 1:10) {
  fit <- glm(nox ~ poly(dis, i), data = Boston)
  cv.error[i] <- cv.glm(Boston, fit)$delta[1]
}
plot(1:10, cv.error, type = "l", col="brown3", lwd=2)

print(cv.error)

which.min(cv.error)

library(splines)
# Picking arbitrary knots
fit <- lm(nox ~ bs(dis, knots=c(3,6), df = 4), data = Boston)
pred <- predict(fit, newdata = data.frame(dis = dis.grid))
plot(Boston$dis, Boston$nox, col = "gray")
lines(dis.grid, pred, lwd = 2, col="brown3")

# Let R pick knots
fit <- lm(nox ~ bs(dis, df = 4), data = Boston)
pred <- predict(fit, newdata = data.frame(dis = dis.grid))
plot(Boston$dis, Boston$nox, col = "gray")
lines(dis.grid, pred, lwd = 2, col="brown3")

# Recover the knots
attr(bs(Boston$dis , df = 4), "knots")

# Realise this is just the median (since there is only one knot)
median(Boston$dis)

rss <- c()
plot(Boston$dis, Boston$nox, col = "gray",ylim=c(0.35, 0.9))
for (i in 3:7) {
  fit <- lm(nox ~ bs(dis, df = i), data = Boston)
  rss[i - 2] <- sum(fit$residuals^2)
  pred <- predict(fit, newdata = data.frame(dis = dis.grid))
  lines(dis.grid, pred, col = colours[i])
}

plot(Boston$dis, Boston$nox, col = "gray",ylim=c(0.35, 0.9))
for (i in 8:11) {
  fit <- lm(nox ~ bs(dis, df = i), data = Boston)
  rss[i - 2] <- sum(fit$residuals^2)
  pred <- predict(fit, newdata = data.frame(dis = dis.grid))
  lines(dis.grid, pred, col = colours[i])
}

print(rss)

set.seed(1)
cv.error <- c()
options(warn=-1) # we remove warnings (yes, living dangerously)
for (i in 3:11) {
  fit <- glm(nox ~ bs(dis, df = i), data = Boston)
  cv.error[i - 2] <- cv.glm(Boston, fit, K = 10)$delta[1]
}
options(warn=0)  # reset to default
plot(3:11, cv.error, type = "l", xlab = "df", lwd=2, col="brown3")

which.min(cv.error)