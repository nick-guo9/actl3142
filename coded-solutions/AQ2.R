# Part A
auto = read.csv('Auto.csv')
str(auto) # int numeric

unique(auto$horsepower) # it says char what are issues here?
auto <- read.csv("Auto.csv", na.strings = "?")
auto <- na.omit(auto) # remove missing values













# Part B
quant.var <- c(
  "mpg", "cylinders", "displacement", "horsepower",
  "weight", "acceleration", "year"
)
ranges.df <- apply(auto[, quant.var], 2, range)
rownames(ranges.df) <- c("min", "max")
ranges.df













# Part C
means.df <- apply(auto[, quant.var], 2, mean)
std.df <- apply(auto[, quant.var], 2, sd)
distns.df <- rbind(means.df, std.df)
rownames(distns.df) <- c("mean", "sd.") # adding index
distns.df["mean", ] # using labelled indexing now in row instead of integer index
t(distns.df) # transpose













# Part D
rid <- rownames(auto)
rid <- rid[as.numeric(rid) < 10 | as.numeric(rid) > 85]
subauto <- auto[rid, ]

subranges.df <- apply(subauto[, quant.var], 2, range)
submeans.df <- apply(subauto[, quant.var], 2, mean)
substd.df <- apply(subauto[, quant.var], 2, sd)
subdistns.df <- rbind(subranges.df, submeans.df, substd.df)
rownames(subdistns.df) <- c("min", "max", "mean", "sd.")
t(subdistns.df)













# Part E
pairs(auto[, -9]) # exclude column 9

# install.packages("corrplot")
library(corrplot)
corrplot(cor(auto[, -9]), method = "color", addCoef.col = "black")
# box plots
hist(auto$horsepower, prob = TRUE, col = "lightgrey")
lines(density(auto$horsepower), col = "green", lwd = 2) # Adds a smooth trend line














# Part F
# Briefly looking at the pairwise plots, the factors cylinders, displacement, horsepower, weight, and possibly year are worth investigating.
