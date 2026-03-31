# Can use attach for easier reference to columns
claimsdata <- read.csv("../../data/Third_party_claims.csv")
attach(claimsdata)
hist(claims, breaks="FD", col="lightgreen")
plot(accidents, claims, xlab = "Accidents", ylab = "Claims")

# prefer not to use attach since it can make code messy and confusing

























library(dplyr)



















df <- read.csv("../../data/Third_party_claims.csv")

# 1. Quick EDA
hist(df$claims, breaks="FD", col="lightgreen")
hist(df$accidents, breaks="FD", col="lightgreen")
plot(df$accidents, df$claims, xlab = "Accidents", ylab = "Claims")

# 2. Binning with 'cut' (5 buckets for stability)
mean_var_data <- df %>%
  mutate(accident_bucket = cut(accidents, breaks = 5)) %>%
  group_by(accident_bucket) %>%
  summarise(
    mean_claims = mean(claims),
    var_claims = var(claims, na.rm = TRUE)
  )

# 3. Lightweight Plot
plot(mean_var_data$mean_claims, mean_var_data$var_claims, 
     xlab = "Sample Mean", ylab = "Sample Variance",
     main = "Empirical Mean vs. Variance",
     pch = 16, col = "blue")

# 4. Poisson Reference Line (y = x)
abline(a = 0, b = 1, col = "red", lty = 2)
legend("topleft", legend = c("Observed", "Poisson (Var = Mean)"), 
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 2))

# OLS
naive_lm <- lm(claims ~ accidents, data = df)
summary(naive_lm)

plot(naive_lm)

# Poisson with log(x) transform
poi_glm <- glm(claims ~ log(accidents), data = df, family = poisson(link="log"))
summary(poi_glm)

plot(poi_glm)

# NB
library("MASS")
NB_glm <- glm.nb(claims ~ log(accidents), link="log")
summary(NB_glm)

plot(NB_glm)





sum(resid(poi_glm, type = "pearson")^2) / poi_glm$df.residual





third.qpoi <- glm(claims ~ log(accidents),
                  family = quasipoisson,
)
summary(third.qpoi)
plot(third.qpoi)
