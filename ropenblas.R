rm(list=ls(all=TRUE))

library(glue)
library(magrittr)
library(stringr)
library(getPass)

exist_opt <- function(...){
  ifelse(system(command = "cd /opt", ...) == 0L, TRUE, FALSE)
}

mkdir_opt <- function(){
  
  if (exist_opt()) stop("The /opt directory already exists. Nothing to do!")
  
  system(
    command = "sudo -kS mkdir opt/",
    input = getPass::getPass("Enter your ROOT OS password (creating /opt directory): ")
  )
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
  file_blas <- sessionInfo()$BLAS %>% strsplit(split = "/") %>% 
    unlist %>% tail(n = 1)
  
  path_blas <- head(sessionInfo()$BLAS %>% strsplit(split = "/") %>% 
                      unlist, -1) %>%
    paste(collapse = "/") %>% paste0("/")
  
  list(file_blas = file_blas, path_blas = path_blas)
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
  
  glue("{diretory_tmp}/OpenBLAS-{x}.tar.gz") %>% untar(exdir = glue("{diretory_tmp}"))
  
  acess_dir <- glue("{diretory_tmp}/OpenBLAS-{x}") 
  
  glue("cd {acess_dir} && make -j $(nproc)") %>% system

  setwd(glue({acess_dir}))
  
  glue("sudo -kS make install PREFIX=/opt/OpenBLAS") %>% 
    system(input = getPass::getPass("Enter your ROOT OS password (creating /opt directory): "))
  
  setwd(dir_blas()$path)
  
  glue("sudo -kS ln -sf /opt/OpenBLAS/lib/libopenblas.so {dir_blas()$path}{dir_blas()$file_blas}") %>% 
    system(input = getPass::getPass("Enter your ROOT OS password: "))
  
  if (rstudioapi::isAvailable()) .rs.restartR()
  
  unlink(diretory_tmp)
}
