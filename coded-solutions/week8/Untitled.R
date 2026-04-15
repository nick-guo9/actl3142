library(ISLR2)
data = ISLR2::Carseats
set.seed(1)
train_set <- sample(nrow(data), nrow(data) / 2)
train_mask <- (1:nrow(data) %in% train_set)

# b) fitting a tree using tree package

library(tree)
fit <- tree(Sales ~ ., data = data, subset = train_mask)
plot(fit)
text(fit, pretty = 0)

pred <- predict(fit, newdata = data[!train_mask, ])
test_mse <- mean((data$Sales[!train_mask] - pred) ^ 2)

print(unique(pred))

# b) fitting a tree using rpart package

library(rpart)
library(rpart.plot)
fit_rpart <- rpart(Sales ~ ., data = Carseats, subset = train_mask)
rpart.plot(fit_rpart)

pred <- predict(fit_rpart, newdata = data[!train_mask, ])
test_mse <- mean((data$Sales[!train_mask] - pred) ^ 2)

print(unique(pred))

# nodes are average y value in that node,
# and the percentage of all the data in that node

# c)

set.seed(123)
fit <- tree(Sales ~ ., data = Carseats)
fit_cv <- cv.tree(fit, FUN = prune.tree)
fit_cv
plot(fit_cv$size, fit_cv$dev, type = "l")

(index.min <- which.min(fit_cv$dev))
(size.min  <- fit_cv$size[index.min])
plot(fit_cv$k, fit_cv$dev, type = "l")

# Pruning the tree to the optimal size found above
final_tree <- prune.tree(fit, best = size.min)

# Plot the new, simpler tree
plot(final_tree)
text(final_tree, pretty = 0)

pred <- predict(final_tree, newdata = data[!train_mask, ])
test_mse <- mean((data$Sales[!train_mask] - pred) ^ 2)

# d)

library(randomForest)

set.seed(1)
bag.sales <- randomForest(Sales ~ .,
                          data = data, subset = train_mask,
                          mtry = (ncol(data) - 1), importance = TRUE
)
pred <- predict(bag.sales, newdata = data[!train_mask, ])
(test.mse <- mean((data$Sales[!train_mask] - pred)^2))

importance(bag.sales)

# e)

set.seed(123)
rf.sales <- randomForest(Sales ~ .,
                         data = data, subset = train_mask,
                         importance = TRUE
)
pred <- predict(rf.sales, newdata = data[!train_mask, ])
(test.mse <- mean((data$Sales[!train_mask] - pred)^2))

importance(rf.sales)







set.seed(1)
rfTestMSE <- rep(Inf, ncol(data) - 1)
for (i in 1:(ncol(data) - 1)) {
  rf.sales <- randomForest(Sales ~ .,
                           data = data, subset = train_mask, mtry = i,
                           importance = TRUE
  )
  pred <- predict(rf.sales, newdata = data[!train_mask, ])
  rfTestMSE[i] <- mean((data$Sales[!train_mask] - pred)^2)
}
plot(1:(ncol(Carseats) - 1), rfTestMSE, type = "l")

