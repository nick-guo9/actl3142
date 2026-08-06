X <- matrix(c(1,4, 1,3, 0,4, 5,1, 6,2, 4,0), ncol=2, byrow=TRUE)
labels <- sample(1:2, 6, replace=TRUE) # (b) Random assignment
old_labels <- rep(0, 6)

# (e) Repeat until answers stop changing
while(any(labels != old_labels)) {
  
  # Plot current state
  plot(X, col = labels + 1, pch = 19, cex = 2, main = "K-Means Iteration")
  old_labels <- labels
  
  # (c) Compute Centroids
  c1 <- colMeans(X[labels == 1, , drop=FALSE])
  c2 <- colMeans(X[labels == 2, , drop=FALSE])
  points(rbind(c1, c2), col = 2:3, pch = 4, cex = 3, lwd = 3) # Mark with X
  
  # (d) Assign to closest centroid using squared Euclidean distance 
  # (No need for sqrt(), the comparison works the same and the code is cleaner)
  d1 <- (X[,1] - c1[1])^2 + (X[,2] - c1[2])^2
  d2 <- (X[,1] - c2[1])^2 + (X[,2] - c2[2])^2
  labels <- ifelse(d1 < d2, 1, 2)
  
  # Pause for 2 seconds so you can actually see the plot update!
  Sys.sleep(2) 
}