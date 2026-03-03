# Show the students how to navigate RStudio, tips and other stuff

# Part A
college = read.csv('College.csv')














# Part B
rownames(college) <- college[, 1] # this is equivalent of index in pandas python?
# View(college) # same as clicking on the view on the right hand side panel

college <- college[, -1] # select the college data frame excluding the first column
# assign it back to college
# could also input a vector to exclude multiple columns i.e. -c(2, 4)
college$Private <- as.factor(college$Private) # turn into categorical variable
# View(college)



str(college) # structure
unique(college$Private)








# Part C
summary(college)
str(college) # structure
pairs(college[ , 2:11])
pairs(college[, 1:10])
plot(college$Private, college$Outstate)













# Part D
Elite <- rep("No", nrow(college)) # replicate
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(college)
plot(college$Elite, college$Outstate)













# Part E
par(mfrow = c(2, 2))
hist(college$Apps, breaks = 20)
hist(college$Accept, breaks = 20)
hist(college$Top10perc, breaks = 20)
hist(college$Top25perc, breaks = 20) # show that when we run this line again after the 4 is filled out it will change now
par(mfrow = c(1, 1))













# Part F
# ...
