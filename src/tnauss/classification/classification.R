x <- rbind(matrix(rnorm(1692308*9, sd = 0.3), ncol = 9),
           matrix(rnorm(1692308*9, mean = 1, sd = 0.3), ncol = 9))
colnames(x) <- paste0("t", seq(9))

# 1. kmeans clustering
cntrs <- 1000
xk <- kmeans(x, centers = cntrs, iter.max = 20, algorithm="MacQueen")

# 2. hclust on centroids
xkd <- dist(xk$centers)
xkdh <- hclust(xkd)
plot(xkdh)

# 3. cut hclust whereever
xkdh_cut <- cutree(xkdh, 3)

# 4. assign new cutted cluster ids to initial kmeans ids
kmeans_cut <- data.frame(cid = seq(cntrs),
                         cutid = xkdh_cut)

# 5. merge with original row numbers
kmeans_cut_final <- merge(data.frame(xkcluster = xk$cluster), kmeans_cut, by.x = "xkcluster", by.y = "cid")

summary(kmeans_cut_final)
