library(glue)
library(magrittr)
library(stringr)

v_bash <- c("cd opt", "sudo -kS pacman -Syy")

exist_opt <- function(...){
  ifelse(system(command = "cd /opt", ...) == 0L, TRUE, FALSE)
}

mkdir_opt <- function(){
  system(
    command = "sudo -kS mkdir opt/",
    input = readline("Enter your ROOT OS password (creating /opt directory): "))
}

download_openblas <- function(x){
  diretory_tmp <- tempdir(check = FALSE)
  url <- glue("https://github.com/xianyi/OpenBLAS/archive/v{x}.tar.gz")
  download.file(url = url, destfile = glue("{diretory_tmp}/OpenBLAS-{x}.tar.gz")) 
  diretory_tmp
}

download_r <- function(x){
  diretory_tmp <- tempdir(check = FALSE)
  url <- glue("https://cloud.r-project.org/src/base/R-{substr(x, 1, 1)}/R-{x}.tar.gz")
  download.file(url = url, destfile = glue("{diretory_tmp}/R-{x}.tar.gz")) 
  diretory_tmp
}

dir_blas <- function(){
  libblas <- unlist(strsplit(sessionInfo()$BLAS, split = "/"))
  nameblas <- 
}

ropenblas <- function(x = "0.3.7"){
  
  if (Sys.info()[[1]] != "Linux")
    stop("Sorry, this package for now configures R to use the OpenBLAS library on Unix-Like systems.")
  
  cat("You must install the following dependencies on your operating system (Linux):

      1 - make 
      2 - gcc
      3 - gcc-fortran 
      
      ")
  
  if (!exist_opt()) mkdir_opt()
  
  diretory_tmp <- download_openblas(x)
  
  glue("{diretory_tmp}/OpenBLAS-{x}.tar.gz") %>% untar
  
  acess_dir <- glue("{diretory_tmp}/OpenBLAS-{x}")
  
  glue("cd {acess_dir} && make -j $(nproc)") %>% system
  
  glue("cd {acess_dir} && sudo -kS make install PREFIX=/opt/OpenBLAS") %>% 
    system(input = readline("Enter your ROOT password: "))
    
}

