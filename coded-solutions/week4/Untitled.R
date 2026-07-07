library(ISLR2)
library(MASS)
library(corrplot)

df = Weekly
str(df)

summary(df)

pairs(df)

corrplot(cor(df[, -9]), method = "color", addCoef.col = "black")

hist(df$Lag1, prob = TRUE, col = "lightgrey")
lines(density(df$Lag1), col = "green", lwd = 3.5) # Adds a smooth trend line








model = glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  family = binomial(link = "logit"),
  data = df
  )
summary(model)


















model = glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  family = binomial(link = "logit"),
  data = df
  )
summary(model)



# b and c
fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
           family = "binomial", data = df
)
summary(fit)

fit$y[1] # check how R is encoding "Up" and "Down"





pred <- rep("Down", length(df$Direction))


pred[model$fitted.values > 0.5] <- "Up"
table(pred, actual=df$Direction)
dim(df)
(48 + 430) / 1089

sum(df$Direction == "Up")
sum(df$Direction == "Down")
605/1089





















train <- (df$Year <= 2008) # boolean index mask
fit2 <- glm(Direction ~ Lag2, family = "binomial", data = df, subset = train)
pred.fit2 <- predict(fit2, newdata = df[!train, ], type = "response")
pred.val <- rep("Down", length(pred.fit2))
pred.val[pred.fit2 > 0.5] <- "Up"
table(pred.val, actual=df$Direction[!train])





















library(class)
train.knn <- df$Lag2[train]
test.knn <- as.matrix(df[!train, "Lag2"], ncol = 1)
train.knn.result <- df[train, "Direction"]






set.seed(3)
fit5 <- knn(train=train.knn, test=test.knn, cl=train.knn.result, k=1)
table(fit5, actual=df$Direction[!train])


(22 + 29) / (21 + 29 + 22 + 32)

















library(MASS)
library(corrplot)
library(ggplot2)
df = ISLR2::Weekly
head(df)
pairs(df)
ggplot(
  data = df,
  mapping = aes(x = Lag1, y = Direction),
  ) + geom_boxplot()

corrplot(cor(df[, -9]), method = "color", addCoef.col = "black")

hist(df$Lag1, prob = TRUE, col = "lightgrey")
lines(density(df$Lag1), col = "green", lwd = 3.5)

model = glm(Direction ~ Volume + Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data = df, family = binomial(link = "logit"))
summary(model)
tmp = model$y
tmp[1000]

pred = rep("Down",length(df$Direction))
pred[model$fitted.values > 0.5] = "Up"
table(pred, actual=df$Direction)


train = df[df['Year'] <= 2008, ]
head(train)
