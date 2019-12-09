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
    git2r::clone("https://github.com/xianyi/OpenBLAS.git", path_openblas)
  
  last_version <- names(tail(git2r::tags(repo_openblas), 1L))
  
  if (is.null(x))
    x <- last_version
  
  if (glue("v{x}") < names(tail(git2r::tags(repo_openblas), 1L))) {
    list(
      new = TRUE,
      version = names(tail(git2r::tags(repo_openblas), 1L)),
      path_openblas = path_openblas,
      repo_openblas = repo_openblas,
      exist_x = TRUE
    )
  } else if (glue("v{x}") > names(tail(git2r::tags(repo_openblas), 1L))) {
    list(
      new = FALSE,
      version = names(tail(git2r::tags(repo_openblas), 1L)),
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
           unlist, -1L) %>%
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
  if (!(x %in% c("y", "no", "yes", "n")))
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

answer_yes_no <- function(text) {
  readline(prompt = glue("{text} (yes/no): ")) %>%
    tolower
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
#' @param restart_r If \code{TRUE}, a new section of \R is started after compiling and linking the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library.
#' @details You must install the following dependencies on your operating system (Linux):
#' \enumerate{
#'    \item \strong{GNU Make};
#'    \item \strong{GNU GCC Compiler (C and Fortran)}.
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
#' @return Returns a warning message informing you if the procedure occurred correctly. You will also be able to receive information about
#' missing dependencies.
#' @importFrom glue glue
#' @importFrom getPass getPass
#' @importFrom magrittr "%>%"
#' @importFrom rstudioapi isAvailable restartSession
#' @importFrom utils download.file head sessionInfo tail
#' @importFrom stringr str_detect str_extract
#' @importFrom git2r clone checkout tags
#' @importFrom fs file_exists file_delete dir_create
#' @importFrom cli rule col_green symbol
#' @seealso \code{\link{rcompiler}}, \code{\link{last_version_r}}
#' @examples
#' # ropenblas()
#' @export
ropenblas <- function(x = NULL, restart_r = TRUE) {
  if (Sys.info()[[1L]] != "Linux")
    stop("Sorry, this package for now configures R to use the OpenBLAS library on Linux systems.\n")
  
  if (!connection())
    stop("You apparently have no internet connection.\n")
  
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
            "The latest version of OpenBLAS is {substr(download$version, 2L, nchar(download$version))}. Do you want to install this version?" %>%
            glue %>%
            answer_yes_no
          
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
            "The latest version of OpenBLAS is already in use. Do you want to compile and link again?" %>%
            answer_yes_no
          
          validate_answer(answer)
          
          if (answer %in% c("y", "yes")) {
            checkout(repo, glue("v{x}"))
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
          "The latest version is {substr(download$version, 2L, nchar(download$version))}. Want to consider the latest version?" %>%
          glue %>%
          answer_yes_no
        
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
          "The latest version of OpenBLAS is already in use. Do you want to compile and link again?" %>%
          answer_yes_no
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
    loop_root(attempt = 5L)
  
  setwd(dir_blas()$path)
  
  if (!str_detect(dir_blas()$file_blas, "libopenblas")) {
    glue(
      "sudo -kS ln -snf /opt/OpenBLAS/lib/libopenblas.so {dir_blas()$path}{dir_blas()$file_blas}"
    ) %>% loop_root(attempt = 5L)
  }
  
  .refresh_terminal <- function() {
    system("R")
    q("no")
  }
  
  if (restart_r) {
    if (rstudioapi::isAvailable()) {
      tmp <- rstudioapi::restartSession() # .rs.restartR()
    } else {
      .refresh_terminal()
    }
  }
  
  cat("\n")
  
  cat(
    cli::rule(
      width = 35L,
      center = "Procedure Completed",
      col = "blue",
      background_col = "gray90",
      line = 2L
    )
  )
  
  cat("\n")
  
  if (is.null(x)) {
    "[{cli::style_bold(cli::col_green(cli::symbol$tick))}] OpenBLAS version {substr(download$version, 2L, nchar(download$version))}." %>%
      glue %>%
      cat
    
  } else {
    "[{cli::style_bold(cli::col_green(cli::symbol$tick))}] OpenBLAS version {x}." %>%
      glue %>%
      cat
  }
  
}

#' @importFrom stringr str_remove
#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom RCurl getURL
#' @importFrom XML getHTMLLinks
#' @title Given the higher version, the function will return the latest stable version of the \R language.
#' @param major Major release number of \R language (e.g. \code{1L}, \code{2L}, \code{3L}, ...). If \code{major = NULL}, the function
#' will consider the major release number.
#' @details This function automatically searches \R language versions in the official language repositories. That way,
#' doing \code{last_version_r(major = NULL)} you will always be well informed about which latest stable version the
#' \R language is in. You can also set the higher version and do a search on the versions of the \R language whose major
#' version was \code{1L} or \code{2L}, for example.
#' @return A list of two named elements will be returned. Are they:
#' \enumerate{
#'    \item \code{last_version}: Returns the latest stable version of the language given a major version (major version).
#'    If \code{major = NULL}, the latest stable version of the language will be returned based on the set of all language versions.
#'    \item \code{versions}: Character vector with all language versions based on a major version (higher version).
#'    If \code{major = NULL}, \code{versions} will be a vector with the latest language versions.
#'    \item \code{n}: Total number of versions of \R based on major version. If \code{major = NULL}, \code{versions}
#'    will be a vector with the latest language versions.
#' }
#' @seealso \code{\link{ropenblas}}, \code{\link{rcompiler}}
#' @examples
#' last_version_r(major = NULL)
#' @export
last_version_r <- function(major = NULL) {
  if (!connection())
    stop("You apparently have no internet connection.\n")
  
  search <- function(x, number_version = TRUE) {
    test <- function(index, vector_versions)
      grepl(pattern = "^R-", vector_versions[index])
    
    if (number_version) {
      vector_versions <-
        glue("https://cloud.r-project.org/src/base/R-{x}/") %>%
        getURL %>%
        getHTMLLinks
      index <-
        sapply(X = 1L:length(vector_versions),
               FUN = test,
               vector_versions)
      vector_versions[index] %>%
        unique %>%
        length
    } else {
      vector_versions <-
        glue("https://cloud.r-project.org/src/base/R-{x}/") %>%
        getURL %>%
        getHTMLLinks
      index <-
        sapply(X = 1L:length(vector_versions),
               FUN = test,
               vector_versions)
      vector_versions[index] %>%
        unique %>%
        str_remove(pattern = "(.tar.gz$|.tgz$)")
    }
  }
  
  trysearch <-
    function(...)
      tryCatch(
        expr = search(...),
        error = function(e)
          return(0),
        warning = function(w)
          return(0)
      )
  
  if (is.null(major))
    major <-
    vapply(X = 1L:6L,
           FUN = trysearch,
           FUN.VALUE = double(1L)) %>% which.min - 1L
  
  
  vec_versions <- search(x = major, number_version = FALSE) %>%
    str_remove(pattern = "^R-")
  
  list(
    last_version = vec_versions[length(vec_versions)],
    versions = vec_versions,
    n = length(vec_versions)
  )
}

#' @importFrom utils untar
#' @importFrom fs file_exists dir_create file_delete
#' @importFrom glue glue
download_r <- function(x) {
  if (file_exists("/tmp/r"))
    "/tmp/r" %>% file_delete
  
  path_r <- "/tmp/r" %>% dir_create
  
  extension <- ifelse(x < 2L, "tgz", "tar.gz")
  
  url <-
    "https://cloud.r-project.org/src/base/R-{substr(x, 1L, 1L)}/R-{x}.{extension}" %>%
    glue
  
  url %>% download.file(destfile = glue("{path_r}/R-{x}.{extension}"))
  
  "{path_r}/R-{x}.{extension}" %>%
    glue %>%
    untar(exdir = glue("{path_r}"))
  
  x <-
    list.files("/tmp/r")[!stringr::str_detect(list.files("/tmp/r"), pattern = "gz$")]
  
  if (!grepl(pattern = "^R", x)) {
    "{path_r}/R-{x}" %>%
      glue
  } else {
    "{path_r}/{x}" %>%
      glue
  }
}

check_r_opt <- function(x = NULL) {
  if (is.null(x))
    x <- last_version_r(x)$last_version
  
  "/opt/R/{x}" %>% glue %>% dir_exists
}

attention <- function(x) {
  cat("\n")
  
  cat(
    cli::rule(
      width = 60L,
      center = "VERY ATTENTION",
      col = "red",
      background_col = "gray90",
      line = 2L
    )
  )
  
  cat("\n")
  
  glue(
    'The {cli::style_bold("ropenblas")} package depends on versions of {cli::style_bold("R")} ',
    '{cli::style_bold(cli::symbol$geq)} {cli::style_bold("3.1.0")}. Therefore,',
    'you will not be able to \nuse this package to go back to a later version of R!'
  ) %>% cat
  
  answer <- NULL
  for (i in 1L:3L) {
    answer[i] <-
      answer_yes_no(text = glue("{cli::symbol$bullet} Do you understand? ({i} of 3)"))
    validate_answer(answer[i])
  }
  
  answer
}

#' @importFrom fs dir_exists
#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom getPass getPass
#' @importFrom cli rule col_green symbol
#' @title Compile a version of \R on GNU/Linux systems
#' @description This function is responsible for compiling a version of the \R language.
#' @param x Version of \R you want to compile. By default (\code{x = NULL}) will be compiled the latest stable version of the \R
#' language. For example, \code{x = "3.6.2"} will compile and link \strong{R-3.6.2} version  as the major version on your system.
#' @param version_openblas \href{https://www.openblas.net/}{\strong{OpenBLAS}} library version that will be linked to the \R code that will be compiled. By default, if
#' \code{version_openblas = NULL}, the latest stable version of the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library will be linked.
#' @details The \code{rcompiler()} function will compile the \R language and make the \strong{R} and \strong{Rscript} binaries available
#' in the \code{/usr/bin} directory. Therefore, the version compiled by \code{rcompiler()} will be available to all users of the Linux system.
#' The \code{rcompiler()} function will work on any GNU/Linux distribution, as long as your distribution has the following dependencies:
#' \enumerate{
#'    \item \strong{GNU Make};
#'    \item \strong{GNU GCC Compiler (C and Fortran)}.
#' }
#' \strong{Note}: If the above dependencies are not installed, the function will automatically identify.
#' In the \code{/opt/R} directory subdirectories will be created with the compiled version numbering of \R. For example, when running
#' \code{rcompiler(x = "version_r")}, at the end of the language compilation process, the \code{/opt/R/version_r} directory will be created.
#' The function will link the \strong{R} and \strong{Rscript} binaries from the current section of \R to the respective newly compiled
#' binaries found in \code{/opt/R/version_r/lib64/R/bin}, where, for example, \code{version_r} could be some version of \R like \code{"3.6.2"}
#' or any other version. If \code{version_r = NULL}, the latest stable version of \R will be compiled.
#' @seealso \code{\link{ropenblas}}, \code{\link{last_version_r}}
#' @return Returns a warning message informing you if the procedure occurred correctly. You will also be able to receive information about
#' missing dependencies.
#' @examples
#' # rcompiler()
#' @export
rcompiler <- function(x = NULL,
                      version_openblas = NULL) {
  if (Sys.info()[[1L]] != "Linux")
    stop("Sorry, this package for now configures R to use the OpenBLAS library on Linux systems.\n")
  
  if (!connection())
    stop("You apparently have no internet connection.\n")
  
  if (!exist())
    stop(
      "GNU GCC Compiler not installed. Install GNU GCC Compiler (C and Fortran) on your operating system.\n"
    )
  if (!exist("make"))
    stop("GNU Make not installed. Install GNU Make on your operating system.\n")
  
  if (!is.null(x) && x < "3.1.0") {
    answer <- attention(x)
    if (any(answer != "y" || answer != "yes"))
      return(warning("Given the answers, it is not possible to continue ..."))
  }
  
  if (is.null(x))
    x <- last_version_r()$last_version
  
  if (check_r_opt(x)) {
    if ("/opt/R/{x}" %>% glue %>% dir_exists) {
      answer <- "R version already compiled. Compile again?" %>%
        answer_yes_no
      
      validate_answer(answer)
      
      if (answer %in% c("n", "no"))
        return(warning("Ok, procedure interrupted!"))
    }
  }
  
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
      loop_root(attempt = 5L)
    
    glue("sudo -kS ln -sf /opt/R/{x}/lib64/R/bin/R /usr/bin/R")  %>%
      loop_root(attempt = 5L)
    
    cat("\n")
    
    cat(
      cli::rule(
        width = 35L,
        center = "Procedure Completed",
        col = "blue",
        background_col = "gray90",
        line = 2L
      )
    )

  } else {
    setwd(path_r)
    glue(
      "cd {path_r} && ./configure --prefix=/opt/R/{x} ",
      "--enable-R-shlib --enable-threads=posix --with-blas=\"-lopenblas ",
      "-m64 -lpthread -lm\""
    ) %>%
      system
  
    glue("make -j $(nproc)") %>%
      system
    
    glue("sudo -kS make install PREFIX=/opt/R/{x}") %>%
      loop_root(attempt = 5L)
    
    glue("sudo -kS ln -sf /opt/R/{x}/lib64/R/bin/R /usr/bin/R")  %>%
      loop_root(attempt = 5L)
    
    ropenblas(x = version_openblas, restart_r = FALSE)
    
  }
  
  cat("\n")
  
  "[{cli::style_bold(cli::col_green(cli::symbol$tick))}] R version {x}." %>%
    glue %>%
    cat
  
  cat("\n")
  
  "{cli::symbol$mustache} The roles are active after terminating the current R session ..." %>% 
    glue %>% 
    col_blue %>% 
    style_bold %>% 
    cat
}