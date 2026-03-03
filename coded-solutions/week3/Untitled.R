library(ISLR2)

fit <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit)

fit2 <- lm(Sales ~ Price + US, data = Carseats)
summary(fit2)


fit2 <- lm(Sales ~ Price + US, data = Carseats)
confint(fit2)

par(mfrow = c(2, 2))
plot(fit2)