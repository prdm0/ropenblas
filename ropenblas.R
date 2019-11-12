library(purrr)

v_bash <- c("cd opt", "sudo -kS pacman -Syy")

exist_opt <- function(...){
  ifelse(system(command = "cd /opt", ...) == 0L, TRUE, FALSE)
}

mkdir_opt <- function(){
  system(command = "sudo -kS mkdir opt/", input = readline("Enter your ROOT password: "))
}

download_openblas <- funtion(){
  tempdir(check = FALSE)
}

ropenblas <- function(x = "v0.3.7.tar.gz"){
  diretory_tmp <- tempdir(check = FALSE)
  url <- paste0("https://github.com/xianyi/OpenBLAS/archive/", x)
  download.file(url = url, destfile = paste0(diretory_tmp, "/", x)) 
  diretory_tmp
}

