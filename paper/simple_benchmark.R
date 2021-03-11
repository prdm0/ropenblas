library(magrittr)
library(tibble)
library(ggplot2)
library(dplyr)

n <- 1e3L
set.seed(0)
X <- matrix(rnorm(n * n), n, n)
result_solve_openblas <- microbenchmark::microbenchmark(solve(X), unit = "s") %>%
  as_tibble() %>%
  mutate(time = time / 1e9)
result_solve_openblas[, 1L] <- "Using OpenBLAS"

save(file = "/home/prdm0/Downloads/ropenblas/paper/result_solve_openblas.RData", result_solve_openblas)

# Plot Benchmarck

load(file = "/home/prdm0/Downloads/ropenblas/paper/result_blas.RData")
load(file = "/home/prdm0/Downloads/ropenblas/paper/result_openblas.RData")
load(file = "/home/prdm0/Downloads/ropenblas/paper/result_solve_blas.RData")
load(file = "/home/prdm0/Downloads/ropenblas/paper/result_solve_openblas.RData")


result <- rbind(result_blas, result_openblas, result_solve_blas, result_openblas)
result[, 3] <- c(rep("svd", 200), rep("solve", 200)) 
names(result) <- c("expr", "time", "Functions")

p <- ggplot(result, aes(x = expr, y = time, fill=Functions))

p <- 
  p + geom_violin() + 
  xlab("Librarys") +
  ylab("Time in seconds") + 
  ggtitle("Singular Value Decomposition\nInverse Function", 
          subtitle = "svd and solve functions") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x.top = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

ggsave(filename = "/home/prdm0/Downloads/ropenblas/paper/benchmark.png")

