sol <- function(n, n_point = 101, size, times = 1) {
      i <- seq(0,1,length.out = n_point)
      dat <- vector(mode = "numeric", length = times)
      for (x in 1:times) {
            prob = vector(mode = "numeric", length = size)
            for (s in 1:size) {
                  m0 <- cbind(sample(i, n, replace = TRUE),
                              sample(i, n, replace = TRUE))
                  m <- t(apply(m0, 1, sort))
                  
                  for (k in 1:n) {
                        if (all(m[,2][k] > m[,1]) && all(m[,1][k] < m[,2])) {
                              prob[s] <- 1
                              break
                        }
                  }
            }
            dat[x] <- mean(prob)
      }
      print(c(mean(dat), sd(dat)/sqrt(length(dat))))
      with(plot(dat, xlab = "Times", ylab = "Probability", type = "l"),
           abline(h = mean(dat), col= "blue"))
}