library(foreach)
library(doParallel)
loop <- makeCluster(detectCores())
registerDoParallel(loop)

sol <- function(n, n_point = 101, size, times = 8) {
      i <- seq(0,1,length.out = n_point)
      dat <- foreach (x = 1:times, .combine = rbind) %dopar% {
            prob = vector(mode = "numeric", length = size)
            for (s in 1:size) {
                  m0 <- cbind(sample(i, n, replace = TRUE),
                              sample(i, n, replace = TRUE))
                  m <- t(apply(m0, 1, sort))
                  for (k in 1:n) {
                        if (all(m[,2][k]>m[,1][-k]) && all(m[,1][k]<m[,2][-k])) {
                              prob[s] <- 1
                              break
                        }
                  }
            }
            data.frame(mean(prob))
      }
      print(c(mean(dat[,1]), sd(dat[,1])/sqrt(length(dat[,1]))))
      with(plot(dat[,1], xlab = "Times", ylab = "Probability", type = "l"),
           abline(h = mean(dat[,1]), col= "blue"))
}
