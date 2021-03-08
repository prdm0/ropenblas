library(magrittr)
library(tibble)
library(ggplot2)
library(dplyr)

# n <- 1e3L
# X <- matrix(rnorm(n * n), n, n)
# 
# result_openblas <- microbenchmark::microbenchmark(svd(X), unit = "s") %>% 
#   as_tibble() %>% 
#   mutate(time = time / 1e9)
# result_openblas[, 1L] <- "Using OpenBLAS"
# 
# save(file = "/home/prdm0/Downloads/ropenblas/paper/result_openblas.RData", result_openblas)

# Plot Benchmarck

load(file = "result_blas.RData")
load(file = "result_openblas.RData")

result <- rbind(result_blas, result_openblas)

p <- ggplot(result, aes(x = expr, y = time))

p <- 
  p + geom_violin() + 
  xlab("Librarys") +
  ylab("Time in seconds") + 
  ggtitle("Singular Value Decomposition of a Matrix") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x.top = element_text(face = "bold")
  )

ggsave(filename = "/home/prdm0/Downloads/ropenblas/paper/benchmark.png")

