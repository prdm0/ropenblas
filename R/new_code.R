

mkdir_opt <- function() {
  if (exist_opt())
    stop("The /opt directory already exists. Nothing to do!")
  
  system(
    command = "sudo -kS mkdir opt/",
    input = getPass::getPass("Enter your ROOT OS password (creating /opt directory): ")
  )
  
}

download_openblas <- function(x = NULL) {
  if (file_exists("/tmp/openblas"))
    file_delete("/tmp/openblas")
  
  path_openblas <- dir_create(path = "/tmp/openblas")
  
  repo_openblas <-
    git2r::clone("https://github.com/xianyi/OpenBLAS.git", path_openblas)
  
  last_version <- names(tail(git2r::tags(repo_openblas), 1L))
  
  if (is.null(x))
    x <- last_version
  
  if (glue("v{x}") < names(tail(git2r::tags(repo_openblas), 1L))) {
    list(
      new = TRUE,
      last_version = names(tail(git2r::tags(repo_openblas), 1L)),
      path_openblas = path_openblas,
      repo_openblas = repo_openblas,
      exist_x = TRUE
    )
  } else if (glue("v{x}") > names(tail(git2r::tags(repo_openblas), 1L))) {
    list(
      new = FALSE,
      last_version = names(tail(git2r::tags(repo_openblas), 1L)),
      path_openblas = path_openblas,
      repo_openblas = repo_openblas,
      exist_x = FALSE
    )
  } else {
    list(
      new = FALSE,
      last_version = names(tail(tags(repo_openblas), 1L)),
      path_openblas = path_openblas,
      repo_openblas = repo_openblas,
      exist_x = TRUE
    )
  }
}


#' @importFrom pingr is_online
connection <- function() {
  if(is_online()) TRUE
  else{
    cat("You apparently have no internet connection.\n")
    FALSE
  }
}

loop_root <- function(x, attempt = 3L, sudo = TRUE) {
  if (sudo) {
    i <- 1L
    key_true <- 1L
    while (i <= attempt && key_true != 0L) {
      key_true <- glue("sudo -kS {x}") %>%
        system(input = getPass::getPass(
          glue("Enter your ROOT OS password (attempt {i} of {attempt}): ")
        ),
        ignore.stderr = TRUE)
      if (key_true != 0L && attempt == i)
        stop(
          "Sorry. Apparently you don't is the administrator of the operating system. You missed all three attempts."
        )
      i <- i + 1L
    }
  } else {
    glue("{x}") %>% system
  }
}


#' @importFrom withr 
#' @
compiler_openblas <-
  function(openblas_version = NULL,
           local = TRUE) {
    download <- download_openblas(x = openblas_version)
    
    if (is.null(openblas_version)) {
      git2r::checkout(object = download$path_openblas,
                      branch = download$last_version)
    } else {
      git2r::checkout(object = download$path_openblas,
                      branch = glue("v{openblas_version}"))
    }
  
    # make --------------------------------------------------------------------

    with_dir(
      new = "/tmp/openblas",
      code = loop_root("make -j $(nproc)", sudo = FALSE)
    )
      
    # Install OpenBLAS --------------------------------------------------------

    if (isTRUE(local)) {
      with_dir(
        new = "/tmp/openblas",
        code = loop_root("make install PREFIX=$HOME/.config_r_lang/OpenBLAS",
                         sudo = FALSE)
      )
    } else {
      with_dir(
        new = "/tmp/openblas",
        code = loop_root("make install PREFIX=/opt/OpenBLAS",
                         sudo = TRUE)
      )
    }
    
    
  }


  
  
  
