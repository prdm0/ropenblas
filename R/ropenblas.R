exist_opt <- function(...) {
  ifelse(system(command = "cd /opt", ...) == 0L,
         TRUE,
         FALSE)
}

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
    clone("https://github.com/xianyi/OpenBLAS.git", path_openblas)
  
  last_version <- names(tail(tags(repo_openblas), 1L))
  
  if (is.null(x))
    x <- last_version
  
  if (glue("v{x}") < names(tail(tags(repo_openblas), 1L))) {
    list(
      new = TRUE,
      version = names(tail(tags(repo_openblas), 1L)),
      path_openblas = path_openblas,
      repo_openblas = repo_openblas,
      exist_x = TRUE
    )
  } else if (glue("v{x}") > names(tail(tags(repo_openblas), 1L))) {
    list(
      new = FALSE,
      version = names(tail(tags(repo_openblas), 1L)),
      path_openblas = path_openblas,
      repo_openblas = repo_openblas,
      exist_x = FALSE
    )
  } else {
    list(
      new = FALSE,
      version = names(tail(tags(repo_openblas), 1L)),
      path_openblas = path_openblas,
      repo_openblas = repo_openblas,
      exist_x = TRUE
    )
  }
}

dir_blas <- function() {
  file_blas <- sessionInfo()$BLAS %>% strsplit(split = "/") %>%
    unlist %>% tail(n = 1L)
  
  path_blas <-
    head(sessionInfo()$BLAS %>% strsplit(split = "/") %>%
           unlist,-1L) %>%
    paste(collapse = "/") %>% paste0("/")
  
  if (str_detect(file_blas, "openblas")) {
    use_openblas <- TRUE
    version_openblas <-
      str_extract(file_blas, pattern = "[0-9]+.[0-9]+.[0-9]+")
  } else {
    use_openblas <- FALSE
    version_openblas <- NA
  }
  
  list(
    file_blas = file_blas,
    path_blas = path_blas,
    version_openblas = version_openblas,
    use_openblas = use_openblas
  )
}

exist <- function(x = "gcc") {
  nsystem <-
    function(...)
      tryCatch(
        system(...),
        error = function(e)
          FALSE
      )
  result <- glue("{x} --version") %>% nsystem(intern = TRUE)
  ifelse(length(result) == 1L, FALSE, TRUE)
}

validate_answer <- function(x) {
  if (!(x %in% c("y", "no", "yes", "no")))
    stop("Invalid option. Procedure interrupted.")
}

modern_openblas <- function(x) {
  if (!stringr::str_detect(dir_blas()$file, "openblas"))
    return(FALSE)
  version_sistem <- dir_blas()$file %>%
    stringr::str_extract(pattern = "[0-9]+.[0-9]+.[0-9]+")
  x > version_sistem
}

connection <- function() {
  nsystem <-
    function(...)
      tryCatch(
        system(...),
        error = function(e)
          cat("You apparently have no internet connection.\n"),
        warning = function(w)
          cat("You apparently have no internet connection.\n")
      )
  check <- "ping -c3 google.com" %>%
    nsystem(ignore.stderr = F, intern = TRUE)
  
  ifelse (!is.numeric(check) && !is.null(check), TRUE, FALSE)
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

#' @title Download, Compile and Link OpenBLAS Library with \R
#' @author Pedro Rafael D. Marinho (e-mail: \email{pedro.rafael.marinho@gmail.com})
#' @description Link \R with an optimized version of the \href{http://www.netlib.org/blas/}{\strong{BLAS}} library (\href{https://www.openblas.net/}{\strong{OpenBLAS}}).
#' @details The \code{ropenblas()} function will only work on Linux systems. When calling the \code{ropenblas()}
#' function on Windows, no settings will be made. Only a warning message will be issued informing you that the
#' configuration can only be performed on Linux systems.
#'
#' The function will automatically download the latest version of the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library. However, it is possible to
#' inform olds versions to the single argument of \code{ropenblas()}. The \code{ropenblas()} function downloads,
#' compiles and link \R to use of the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library. Everything is done very simply, just loading the library and
#' invok the function \code{ropenblas()}.
#'
#' Considering using the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library rather than the \href{http://www.netlib.org/blas/}{\strong{BLAS}} may bring extra optimizations for your code and improved
#' computational performance for your simulations, since \href{https://www.openblas.net/}{\strong{OpenBLAS}} is an optimized implementation of the library \href{http://www.netlib.org/blas/}{\strong{BLAS}}.
#' @note You do not have to in every section of \R make use of the \code{ropenblas()} function. Once the function is used, \R
#' will always consider using the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library in future sections.
#' @param x \href{https://www.openblas.net/}{\strong{OpenBLAS}} library version to be considered. By default, \code{x = NULL}.
#' @details You must install the following dependencies on your operating system (Linux):
#' \enumerate{
#'    \item \strong{make};
#'    \item \strong{gcc};
#'    \item \strong{gcc-fortran}.
#' }
#' Your linux operating system may already be configured to use the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library. Therefore, most likely \R will already be linked to this library. To find out if the \R language is using the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library,
#'  at \R, do:
#'
#' \code{extSoftVersion()["BLAS"]}
#'
#' If \R is using the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library, something like \code{/any_directory/libopenblas.so} should be returned. Therefore, there should be the name openblas in the \strong{s}hared \strong{o}bject returned (file extension \strong{.so}).
#'
#' If the \code{ropenblas()} function can identify that the \R language is using the version of \href{https://www.openblas.net/}{\strong{OpenBLAS}} you wish to configure, a warning message will be returned asking if you really would like to proceed with the
#'  configuration again.
#'
#' The \code{ropenblas()} function will download the desired version of the library \href{https://www.openblas.net/}{\strong{OpenBLAS}}, compile and install the library in the \code{/opt} directory of your operational system. If the directory does not exist, it will
#'  be created so that the installation can be completed. Subsequently, files from the version of \href{http://www.netlib.org/blas/}{\strong{BLAS}} used in \R will be symbolically linked to the shared object files of the library version \href{https://www.openblas.net/}{\strong{OpenBLAS}} compiled and installed in \code{/opt}.
#'
#' You must be the operating system administrator to use this library. Therefore, do not attempt to use it without telling your system administrator. If you have the ROOT password, you will be responsible for everything you do on your operating system. Other details you may also find \href{https://prdm0.github.io/ropenblas/index.html}{\strong{here}}.
#' @importFrom glue glue
#' @importFrom getPass getPass
#' @importFrom magrittr "%>%"
#' @importFrom rstudioapi isAvailable restartSession
#' @importFrom utils download.file head sessionInfo tail
#' @importFrom stringr str_detect str_extract
#' @importFrom git2r clone checkout tags
#' @importFrom fs file_exists file_delete dir_create
#' @examples
#' \donttest{ropenblas()}
#' @export
ropenblas <- function(x = NULL) {
  if (Sys.info()[[1]] != "Linux")
    stop("Sorry, this package for now configures R to use the OpenBLAS library on Linux systems.\n")
  
  if (!connection())
    stop("You apparently have no internet connection\n")
  
  download <- download_openblas(x)
  repo <- download$repo_openblas
  diretory_tmp <- download$path_openblas
  
  if (!is.null(x) && glue("v{x}") > download$version)
    stop(
      glue(
        "Version {x} does not exist. The latest version of OpenBLAS is {substr(download$version, 2L, nchar(download$version))}."
      )
    )
  
  
  if (!is.null(x)) {
    if (dir_blas()$use_openblas) {
      if (glue("v{dir_blas()$version_openblas}") < download$version) {
        cat("\n")
        if (glue("v{x}") != download$version) {
          answer <-
            readline(
              prompt = glue(
                "The latest version of OpenBLAS is {substr(download$version, 2L, nchar(download$version))}. Do you want to install this version? (yes/no): "
              )
            ) %>% tolower
          
          validate_answer(answer)
          
        } else {
          answer <- "y"
        }
        
        if (answer %in% c("y", "yes")) {
          checkout(repo, download$version)
        } else {
          checkout(repo, glue("v{x}"))
        }
      } else {
        if (glue("v{dir_blas()$version_openblas}") == download$version) {
          answer <-
            readline(
              prompt = glue(
                "The latest version of OpenBLAS is already in use. Do you want to compile and link again? (yes/no): "
              )
            ) %>% tolower
          
          validate_answer(answer)
          
          if (answer %in% c("y", "yes")) {
            checkout(repo, download$version)
          } else {
            return(warning("Ok, procedure interrupted!"))
          }
        } else {
          stop(
            glue(
              "There is no OpenBLAS version {x}. The latest version is {substr(download$version, 2L, nchar(download$version))}."
            )
          )
        }
      }
    } else {
      if (glue("v{x}") < download$version) {
        answer <-
          readline(
            prompt = glue(
              "The latest version is {substr(download$version, 2L, nchar(download$version))}. Want to consider the latest version? (yes/no): "
            )
          )
        
        validate_answer(answer)
        
        if (answer %in% c("y", "yes")) {
          checkout(repo, download$version)
        } else {
          checkout(repo, glue("v{x}"))
        }
        
      } else {
        checkout(repo, glue("v{x}"))
      }
      
    }
  } else {
    if (dir_blas()$use_openblas) {
      if (glue("v{dir_blas()$version}") < download$version) {
        checkout(repo, download$version)
      } else {
        answer <-
          readline(
            prompt = glue(
              "The latest version of OpenBLAS is already in use. Do you want to compile and link again? (yes/no): "
            )
          ) %>% tolower
        
        validate_answer(answer)
        
        if (answer %in% c("n", "no")) {
          return(warning("Ok, procedure interrupted!"))
        } else {
          checkout(repo, download$version)
        }
        
      }
    } else {
      checkout(repo, download$version)
    }
    
  }
  
  cat(
    "\n\nYou must be the system administrator. You must install the following dependencies on your operating system (Linux):

      1 - Make: GNU Make utility to maintain groups of programs;
      2 - GNU GCC: The GNU Compiler Collection - C frontends;
      3 - GNU GCC Fortran: The GNU Compiler Collection - Fortran frontends.

  "
  )
  
  if (!exist())
    stop(
      "GNU GCC not installed. Install GNU GCC Compiler (C and Fortran) on your operating system."
    )
  if (!exist("make"))
    stop("GNU Make not installed. Install GNU Make on your operating system.")
  
  if (!exist_opt())
    mkdir_opt()
  
  glue("cd {diretory_tmp} && make -j $(nproc)") %>% system
  
  glue({
    diretory_tmp
  }) %>% setwd
  
  glue("sudo -kS make install PREFIX=/opt/OpenBLAS") %>%
    loop_root
  
  setwd(dir_blas()$path)
  
  if (!str_detect(dir_blas()$file_blas, "libopenblas")) {
    glue(
      "sudo -kS ln -snf /opt/OpenBLAS/lib/libopenblas.so {dir_blas()$path}{dir_blas()$file_blas}"
    ) %>% loop_root
  }
  
  cat("Done!\n")
  
  .refresh_terminal <- function() {
    system("R")
    q("no")
  }
  
  if (rstudioapi::isAvailable()) {
    tmp <- rstudioapi::restartSession() # .rs.restartR()
  } else {
    .refresh_terminal()
  }
  
}

#' @title Given the higher version, the function will return the latest stable version of the \R language.
#' @param major Major release number of R language (eg. 1L, 2L, 3L, ...).
#' @importFrom stringr str_extract_all
#' @importFrom glue glue
#' @importFrom RCurl getURL
#' @importFrom magrittr "%>%"
#' @export
last_version_r <- function(major = 3L) {
  vec_versions <-
    stringr::str_extract_all(RCurl::getURL(glue(
      "https://cloud.r-project.org/src/base/R-{major}/"
    )),
    "R-[0-9]+.[0-9]+.[0-9]+") %>% unlist
  vec_versions[length(vec_versions)]
}

download_r <- function(x) {
  if (file_exists("/tmp/r"))
    file_delete("/tmp/r")
  
  path_r <- dir_create(path = "/tmp/r")
  
  url <-
    glue("https://cloud.r-project.org/src/base/R-{substr(x, 1, 1)}/R-{x}.tar.gz")
  download.file(url = url,
                destfile = glue("{path_r}/R-{x}.tar.gz"))
  
  glue("{path_r}/R-{x}.tar.gz") %>% untar(exdir = glue("{path_r}"))
  
  glue("{path_r}/R-{x}")
}

#' @title Compile a version of \R on GNU/Linux systems
#' @description This function is responsible for compiling a version of the R language.
#' @importFrom RCurl getURL
#' @export
rcompiler <- function(x = NULL,
                      version_openblas = NULL) {
  if (Sys.info()[[1]] != "Linux")
    stop("Sorry, this package for now configures R to use the OpenBLAS library on Linux systems.\n")
  
  if (!connection())
    stop("You apparently have no internet connection\n")
  
  if (!exist())
    stop(
      "GNU GCC Compiler not installed. Install GNU GCC Compiler (C and Fortran) on your operating system."
    )
  if (!exist("make"))
    stop("GNU Make not installed. Install GNU Make on your operating system.")
  
  
  if (is.null(x))
    x <- last_version_r() %>%
      substr(3L, nchar(last_version_r()))
  
  path_r <- download_r(x)
  
  if (dir_blas()$use_openblas) {
    setwd(path_r)
    glue("export LD_LIBRARY_PATH=/opt/OpenBLAS/lib/") %>%
      system
    glue(
      "cd {path_r} && ./configure --prefix=/opt/R/{x} ",
      "--enable-R-shlib --enable-threads=posix --with-blas=\"-lopenblas ",
      "-L/opt/OpenBLAS/lib -I/opt/OpenBLAS/include -m64 -lpthread -lm\""
    ) %>%
      system
    
    glue("make -j $(nproc)") %>%
      system
    
    glue("sudo -kS make install PREFIX=/opt/R/{x}") %>%
      loop_root
    
  } else {
    setwd(path_r)
    ropenblas(x = version_openblas)
    glue("export LD_LIBRARY_PATH=/opt/OpenBLAS/lib/") %>%
      system
    glue(
      "cd {path_r} && ./configure --prefix=/opt/R/{x} ",
      "--enable-R-shlib --enable-threads=posix --with-blas=\"-lopenblas ",
      "-L/opt/OpenBLAS/lib -I/opt/OpenBLAS/include -m64 -lpthread -lm\""
    ) %>%
      system
    
    glue("make -j $(nproc)") %>%
      system
    
    glue("sudo -kS make install PREFIX=/opt/R/{x}") %>%
      loop_root
    
  }
  
  glue("sudo -kS ln -sf /opt/R/{x}/lib64/R/bin/R /usr/bin/R")  %>%
    loop_root
  
  cat("Done!\n")
  cat(glue("In the next section of R, version {x} will be considered!"))
  
}