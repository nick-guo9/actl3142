library(ISLR2)

data = ISLR2::College

# a
n = length(data[, 1])
# n = 777

set.seed(1)

train_idx = sample(n, 2 / 3 * n)

train_mask = (seq(1,n) %in% train_idx)
!train_mask

train_data = data[train_mask, ]
test_data = data[!train_mask, ]

# b
fit = lm(Grad.Rate ~ ., data = train_data)
# fit = lm(Grad.Rate ~ ., data = data, subset = train_mask)

summary(fit)

print("Training MSE")
mean(fit$residuals ^ 2)

predictions = predict(fit, newdata = test_data)
print("Test MSE")
mean((predictions - test_data$Grad.Rate) ^ 2)

# c

# fitting a ridge model

# install.packages("glmnet")
library(glmnet)

X_design = model.matrix(Grad.Rate ~ ., data)[, -1]
y = as.matrix(data$Grad.Rate)

fit_rdg = glmnet(X_design[train_mask, ], y[train_mask], alpha = 0)
plot(fit_rdg, xvar = "lambda", label = T)

rdg_predictions = predict(fit_rdg, s = 10, newx = X_design[!train_mask, ])

print("Test MSE with lambda 10")
mean((rdg_predictions - y[!train_mask]) ^ 2)

# cross validation
set.seed(1)
cv_rdg_results = cv.glmnet(X_design[train_mask, ], y[train_mask], alpha = 0)
plot(cv_rdg_results)

# get the optimal lambda
cv_rdg_results$lambda.min

rdg_predictions = predict(fit_rdg, s = cv_rdg_results$lambda.min, newx = X_design[!train_mask, ])

print("Test MSE with optimal lambda")
mean((rdg_predictions - y[!train_mask]) ^ 2)

# d

# fitting a lasso model

X_design = model.matrix(Grad.Rate ~ ., data)[, -1]
y = as.matrix(data$Grad.Rate)

fit_lasso = glmnet(X_design[train_mask, ], y[train_mask], alpha = 1)
plot(fit_lasso, xvar = "lambda", label = T)

# cross validation
set.seed(1)
cv_lasso_results = cv.glmnet(X_design[train_mask, ], y[train_mask], alpha = 1)
plot(cv_lasso_results)

# get the optimal lambda
cv_lasso_results$lambda.min

lasso_pred = predict(cv_lasso_results, s = cv_lasso_results$lambda.min, newx = X_design[!train_mask, ])

print("Test MSE with optimal lambda")
mean((lasso_pred - y[!train_mask]) ^ 2)
