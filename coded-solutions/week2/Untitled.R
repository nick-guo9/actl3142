library(ISLR2)

df = Auto


fit = lm(mpg ~ horsepower, data = df)


summary(fit)











fit <- lm(mpg ~ horsepower + weight + acceleration + year + displacement, data = Auto)
summary(fit)
plot(fit)







predict(fit, newdata = data.frame(horsepower = c(98)), interval = "confidence")

predict(fit, newdata = data.frame(horsepower = c(98)), interval = "prediction")





plot(df$horsepower, df$mpg)
abline(a = fit$coefficients[1], b = fit$coefficients[2])





par(mfrow = c(2, 2))
plot(fit)







df['feat_1'] = df['horsepower'] ^ (1 / 2)
fit <- lm(mpg ~ feat_1, data = df)
plot(df$feat_1, df$mpg)
abline(a = fit$coefficients[1], b = fit$coefficients[2])


df['feat_1'] = df['horsepower'] ^ (1)
fit <- lm(mpg ~ feat_1, data = df)
plot(df$feat_1, df$mpg)
abline(a = fit$coefficients[1], b = fit$coefficients[2])
