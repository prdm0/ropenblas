exist_opt <- function(...){
  ifelse(
    system(command = "cd /opt", ...) == 0L,
    TRUE,
    FALSE
  )
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

exist <- function(x = "gcc"){
  nsystem <- function(...) tryCatch(system(...), error = function(e) FALSE)
  result <- glue("{x} --version") %>% nsystem(intern = TRUE) 
  ifelse(length(result) == 1L, FALSE, TRUE)
}

#' @title Download, compile and configure R to use the OpenBLAS library
#' @author Pedro Rafael D. Marinho
#' @description Link R with an optimized version of the BLAS library (OpenBLAS).
#' @details The \code{ropenblas()} function will only work on Linux/Unix systems. When calling the \code{ropenblas()}
#' function on Windows, no settings will be made. Only a warning message will be issued informing you that the
#' configuration can only be performed on Unix-like systems.
#'
#' The function will automatically download the latest version of the OpenBLAS library. However, it is possible to
#' inform olds versions to the single argument of \code{ropenblas()}. The \code{ropenblas()} function downloads, 
#' compiles and link R to use of the OpenBLAS library. Everything is done very simply, just loading the library and
#' invok the function \code{ropenblas()}.
#'  
#' Considering using the OpenBLAS library rather than the BLAS may bring extra optimizations for your code and improved
#' computational performance for your simulations, since OpenBLAS is an optimized implementation of the library BLAS.
#' @note You do not have to in every section of R make use of the \code{ropenblas()} function. Once the function is used, R 
#' will always consider using the OpenBLAS library in future sections.
#' @param x OpenBLAS library version to be considered. By default, \code{x = 0.3.7}.
#' @details You must install the following dependencies on your operating system (Linux): make,  gcc and gcc-fortran. 
#' @importFrom glue glue
#' @importFrom getPass getPass
#' @importFrom magrittr "%>%" 
#' @importFrom rstudioapi isAvailable restartSession
#' @importFrom utils download.file head sessionInfo tail untar
#' @importFrom stringr str_detect
#' @examples 
#' # ropenblas()
#' @export
ropenblas <- function(x = "0.3.7"){
  
  if (Sys.info()[[1]] != "Linux")
    stop("Sorry, this package for now configures R to use the OpenBLAS library on Unix-Like systems.\n")
  
  if (str_detect(dir_blas()$file, "0.3.7")){
    
    cat(glue("The R language is already linked to the {x} version of the OpenBLAS library.\n"))
    
    answer <- readline(prompt = "Do you still want to compile and link again (yes/no)?: ")
    
    if (answer == "no" || answer == "n") stop("Ok. Procedure interrupted.")
    if (!(answer %in% c("y", "no", "yes", "no"))) stop("Invalid option. Procedure interrupted.")
    
  }
  
  cat("You must install the following dependencies on your operating system (Linux):

      1 - make: GNU make utility to maintain groups of programs; 
      2 - gcc: The GNU Compiler Collection - C and C++ frontends;
      3 - gcc-fortran: The GNU Compiler Collection - Fortran frontends. 
      
      ")
  
  if(!exist()) stop("gcc not installed. Install gcc on your operating system.")
  if(!exist("make")) stop("make not installed. Install make on your operating system.") 
  
  if (!exist_opt()) mkdir_opt()
  
  diretory_tmp <- download_openblas(x)
  
  glue("{diretory_tmp}/OpenBLAS-{x}.tar.gz") %>% untar(exdir = glue("{diretory_tmp}"))
  
  acess_dir <- glue("{diretory_tmp}/OpenBLAS-{x}") 
  
  glue("cd {acess_dir} && make -j $(nproc)") %>% system

  setwd(glue({acess_dir}))
  
  repeat{
    key_true <- glue("sudo -kS make install PREFIX=/opt/OpenBLAS") %>% 
      system(input = getPass::getPass("Enter your ROOT OS password: "))
    if (key_true == 0L) break
  }
  
  setwd(dir_blas()$path)
  
  if (!str_detect(dir_blas()$file_blas, "libopenblas")){
    repeat{
      key_true <- glue("sudo -kS ln -snf /opt/OpenBLAS/lib/libopenblas.so {dir_blas()$path}{dir_blas()$file_blas}") %>% 
        system(input = getPass::getPass("Enter your ROOT OS password: "))
      cat("\n\n")
      if (key_true == 0L) break
    }
  }

  if (rstudioapi::isAvailable()) rstudioapi::restartSession()
  
  diretory_tmp %>% unlink(recursive = TRUE, force = TRUE)
}