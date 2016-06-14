# install.packages("FactoMineR")
library(FactoMineR)

x <- rbind(matrix(rnorm(1692308*9, sd = 0.3), ncol = 9),
           matrix(rnorm(1692308*9, mean = 1, sd = 0.3), ncol = 9))
colnames(x) <- paste0("t", seq(9))

xk <- kmeans(x, centers = 1000, iter.max = 20)
