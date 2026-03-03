# df = read.csv("~/Downloads/airbnb-London.csv")
# 
# str(df)
# 
# hist(df$price_per_room)




# install.packages("ISLR2")

library(ISLR2)



df = Auto



fit = lm(mpg ~ horsepower, data = df)
summary(fit)
# fit <- lm(y ~ x1 + x2 + .., data = Auto)


df['feat_1'] = df['horsepower'] ^ (1/2)



fit <- lm(mpg ~ horsepower + weight + acceleration + year + displacement, data = Auto)

summary(fit)









predict(fit, newdata = data.frame(horsepower = c(98)), interval = "confidence")

predict(fit, newdata = data.frame(horsepower = c(98)), interval = "prediction")





plot(df$feat_1, df$mpg)
abline(a = fit$coefficients[1], b = fit$coefficients[2])





par(mfrow = c(2, 2))
plot(fit)







