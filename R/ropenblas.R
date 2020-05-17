answer_yes_no <- function(text) {
  readline(prompt = glue("{text} (yes/no): ")) %>%
    tolower
}

download_openblas <- function(x = NULL) {
  if (dir.exists("/tmp/openblas"))
    unlink("/tmp/openblas", recursive = TRUE)
  
  dir.create("/tmp/openblas")
  path_openblas <- "/tmp/openblas"
  
  
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

is_sudo <-  function() {
  blas <- ifelse(
    glue("{dir_blas()$path_blas}{dir_blas()$file_blas}") %>%
      file.mode %>% substr(., nchar(.), nchar(.)) < 6L,
    TRUE,
    FALSE
  )
  
  r <- ifelse(paste(system(command = "which R", intern = TRUE)) %>%
                file.mode %>% substr(., nchar(.), nchar(.)) < 6L,
              TRUE,
              FALSE)
  
  rscript <-
    ifelse(paste(system(command = "which Rscript", intern = TRUE)) %>%
             file.mode %>% substr(., nchar(.), nchar(.)) < 6L,
           TRUE,
           FALSE)
  
  list(blas = blas,
       r = r,
       rscript = rscript)
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

#' @importFrom cli style_bold col_red
validate_answer <- function(x) {
  if (!(x %in% c("y", "no", "yes", "n")))
    stop(
      glue(
        "{emoji(\"red_circle\")} Invalid option. Procedure interrupted."
      )
    )
}

modern_openblas <- function(x) {
  if (!stringr::str_detect(dir_blas()$file, "openblas"))
    return(FALSE)
  version_sistem <- dir_blas()$file %>%
    stringr::str_extract(pattern = "[0-9]+.[0-9]+.[0-9]+")
  x > version_sistem
}

#' @importFrom pingr is_online
connection <- function() {
  if (is_online())
    TRUE
  else{
    cat("You apparently have no internet connection.\n")
    FALSE
  }
}

#' @importFrom emojifont emoji
sudo_key <- function(attempt = 3L) {
  test <- function(key_root) {
    system(
      command = glue("echo {key_root} |sudo -S -k id -u"),
      ignore.stderr = TRUE,
      intern = TRUE
    ) %>%
      as.numeric %>% `+`(., 1L) %>%
      as.logical
  }
  
  test_root <-
    function(...)
      tryCatch(
        expr = test(...),
        error = function(e)
          return(FALSE),
        warning = function(w)
          return(FALSE)
      )
  
  i <- 1L
  key_true <- FALSE
  while (i <= attempt && !key_true) {
    key_root <-
      getPass::getPass(glue("{emoji(\"closed_lock_with_key\")} Enter your ROOT OS password (attempt {i} of {attempt}): "))
    key_true <- test_root(key_root = key_root)
    
    if (!key_true && attempt == i)
      stop("\rSorry. Apparently you don't is the administrator of the operating system. You missed all three attempts.")
    
    i <- i + 1L
  }
  key_root
}

run_command <- function(x, key_root = NULL) {
  if (is.null(key_root))
    system(command = glue("{x}"), ignore.stderr = TRUE)
  else
    system(command = glue("echo {key_root} | sudo -S -k {x}"), ignore.stderr = TRUE)
} 
  

# loop_root <- function(x, attempt = 3L, sudo = TRUE) {
#   if (sudo) {
#     i <- 1L
#     key_true <- 1L
#     while (i <= attempt && key_true != 0L) {
#       key_true <- glue("sudo -kS {x}") %>%
#         system(input = getPass::getPass(
#           glue("Enter your ROOT OS password (attempt {i} of {attempt}): ")
#         ),
#         ignore.stderr = TRUE)
#       if (key_true != 0L && attempt == i)
#         stop(
#           "Sorry. Apparently you don't is the administrator of the operating system. You missed all three attempts."
#         )
#       i <- i + 1L
#     }
#   } else {
#     glue("{x}") %>% system
#   }
# }

#' @importFrom withr with_dir
#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom git2r checkout
#' @importFrom fs dir_delete
#' @importFrom fs dir_exists
#' @importFrom cli rule col_red symbol style_bold
#' @importFrom emojifont emoji
compiler_openblas <-
  function(download,
           openblas_version = NULL, key_root) {
    # download <- download_openblas(x = openblas_version)
    
    if (!exist())
      stop(
        glue(
          "{emoji(\"red_circle\")} GNU GCC not installed. Install GNU GCC Compiler (C and Fortran) on your operating system."
        )
      )
    if (!exist("make"))
      stop(
        glue(
          "{emoji(\"red_circle\")} GNU Make not installed. Install GNU Make on your operating system."
        )
      )
    
    if (is.null(openblas_version)) {
      git2r::checkout(object = download$path_openblas,
                      branch = download$last_version)
    } else {
      git2r::checkout(object = download$path_openblas,
                      branch = glue("{openblas_version}"))
    }
    
    # make --------------------------------------------------------------------
    
    with_dir(new = "/tmp/openblas",
             code = run_command(x = "make -j $(nproc)", key_root = NULL))
    
    # Install OpenBLAS --------------------------------------------------------
    
    
    if (!dir_exists(path = "/opt"))
      run_command(x = "mkdir /opt", key_root = key_root)
    
    "cp {dir_blas()$path}{dir_blas()$file_blas} /opt/{dir_blas()$file_blas}" %>%
      glue %>%
      run_command(key_root)
      # loop_root(attempt = 5L, sudo = is_sudo()$blas)
    
    with_dir(
      new = "/tmp/openblas",
      code = run_command("make install PREFIX=/opt/OpenBLAS", key_root = key_root)
    )
    
    download
    
  }

#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_detect
error_r <- function() {
  #current_directory <- "{R.home()}/bin" %>% glue
  run_r <- "$(which R) --no-save&" %>%
    glue %>%
    system(intern = TRUE)
  
  error <- any(FALSE, str_detect(run_r, pattern = "Error:"))
  
  if (length(error) == 0L)
    error <- TRUE
  
  error
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
#' @importFrom cli rule symbol style_bold
#' @importFrom emojifont emoji
#' @seealso \code{\link{rcompiler}}, \code{\link{last_version_r}}
#' @examples
#' # ropenblas()
#' @export
ropenblas <- function(x = NULL, restart_r = TRUE) {
  if (Sys.info()[[1L]] != "Linux")
    stop(
      "Sorry, this package for now configures {style_bold(\"R\")} to use the {style_bold(\"OpenBLAS\")} library on Linux systems.\n"
    )
  
  if (!connection())
    stop(
      glue(
        "{emoji(\"red_circle\")} You apparently have no internet connection.\n"
      )
    )
  
  if (identical(caller_env(), global_env())){
    root <- sudo_key()
  } else {
    root <- env_get(env = env_ropenblas_compiler_r, nm = "root") # get(x = "root", envir = env_ropenblas_compiler_r)
    rm(env_ropenblas_compiler_r, envir = global_env())
  }

  initial_blas <- dir_blas()$file_blas
  download <- download_openblas(x)
  
  if (!is.null(x) && glue("v{x}") > download$last_version)
    stop(
      glue(
        "{emoji(\"red_circle\")} Version {style_bold({x})} does not exist. The latest version of {style_bold(\"OpenBLAS\")} is {style_bold({substr(download$last_version, 2L, nchar(download$last_version))})}."
      )
    )
  
  if (!is.null(x)) {
    if (dir_blas()$use_openblas) {
      if (glue("v{dir_blas()$version_openblas}") < download$last_version) {
        cat("\n")
        if (glue("v{x}") != download$last_version) {
          answer <-
            "{emoji(\"large_blue_circle\")} The latest version of {style_bold(\"OpenBLAS\")} is {style_bold({substr(download$last_version, 2L, nchar(download$last_version))})}. Do you want to install this version?" %>%
            glue %>%
            answer_yes_no
          
          validate_answer(answer)
          
        } else {
          answer <- "y"
        }
        
        if (answer %in% c("y", "yes")) {
          # checkout(repo, download$version)
          compiler_openblas(download = download,
                            openblas_version = download$last_version,
                            key_root = root)
        } else {
          # checkout(repo, glue("v{x}"))
          compiler_openblas(download = download,
                            openblas_version = glue("v{x}"),
                            key_root = root)
        }
      } else {
        if (glue("v{dir_blas()$version_openblas}") == download$last_version) {
          answer <-
            "{emoji(\"large_blue_circle\")} The latest version of {style_bold(\"OpenBLAS\")} is already in use. Do you want to compile and link again?" %>%
            glue %>%
            answer_yes_no
          
          validate_answer(answer)
          
          if (answer %in% c("y", "yes")) {
            # checkout(repo, glue("v{x}"))
            compiler_openblas(download = download,
                              openblas_version = glue("v{x}"),
                              key_root = root)
          } else {
            return(warning("Ok, procedure interrupted!"))
          }
        } else {
          stop(
            glue(
              "{emoji(\"red_circle\")} There is no {style_bold(\"OpenBLAS\")} version {style_bold({x})}. The latest version is {style_bold({{substr(download$last_version, 2L, nchar(download$last_version))}})}."
            )
          )
        }
      }
    } else {
      if (glue("v{x}") < download$last_version) {
        answer <-
          "{emoji(\"large_blue_circle\")} The latest version is {style_bold({{substr(download$last_version, 2L, nchar(download$last_version))}})}. Want to consider the latest version?" %>%
          glue %>%
          answer_yes_no
        
        validate_answer(answer)
        
        if (answer %in% c("y", "yes")) {
          # checkout(repo, download$version)
          compiler_openblas(download = download,
                            openblas_version = download$last_version,
                            key_root = root)
        } else {
          # checkout(repo, glue("v{x}"))
          compiler_openblas(download = download,
                            openblas_version = glue("v{x}"),
                            key_root = root)
        }
        
      } else {
        # checkout(repo, glue("v{x}"))
        compiler_openblas(download = download,
                          openblas_version = glue("v{x}"),
                          key_root = root)
      }
      
    }
  } else {
    if (dir_blas()$use_openblas) {
      if (glue("v{dir_blas()$version}") < download$last_version) {
        # checkout(repo, download$version)
        compiler_openblas(download = download,
                          openblas_version = download$last_version,
                          key_root = root)
      } else {
        answer <-
          "{emoji(\"large_blue_circle\")} The latest version of {style_bold(\"OpenBLAS\")} is already in use. Do you want to compile and link again?" %>%
          glue %>%
          answer_yes_no
        validate_answer(answer)
        
        if (answer %in% c("n", "no")) {
          return(warning("Ok, procedure interrupted!"))
        } else {
          # checkout(repo, download$version)
          compiler_openblas(download = download,
                            openblas_version = download$last_version,
                            key_root = root)
        }
        
      }
    } else {
      # checkout(repo, download$version)
      compiler_openblas(download = download,
                        openblas_version = download$last_version,
                        key_root = root)
    }
    
  }
  
  # cat(
  #   "\n\nYou must be the system administrator. You must install the following dependencies on your operating system (Linux):
  # 
  #     1 - Make: GNU Make utility to maintain groups of programs;
  #     2 - GNU GCC: The GNU Compiler Collection - C frontends;
  #     3 - GNU GCC Fortran: The GNU Compiler Collection - Fortran frontends.
  # 
  # "
  # )
  
  if (!str_detect(dir_blas()$file_blas, "libopenblas")) {
    glue(
      "ln -snf /opt/OpenBLAS/lib/libopenblas.so {dir_blas()$path}{dir_blas()$file_blas}"
    ) %>% run_command(key_root = root) #loop_root(attempt = 5L, sudo =  is_sudo()$blas)
          
  }
  
  if (error_r()) {
    "mv /opt/OpenBLAS/{initial_blas} {dir_blas()$path}" %>%
      glue %>%
      run_command(key_root = root) #loop_root(attempt = 5L, sudo = TRUE)
    
    cat("\n")
    
    cat(rule(
      width = 50L,
      center = glue("{style_bold(\"Procedure Incompleted\")}"),
      col = "red",
      background_col = "gray90",
      line = 2L
    ))
    
    "{emoji(\"red_circle\")} Some error has occurred. No changes have been made." %>%
      glue %>%
      warning %>%
      return
    
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
  
  cat(rule(
    width = 50L,
    center = glue("{style_bold(\"Procedure Completed\")}"),
    col = "blue",
    background_col = "gray90",
    line = 2L
  ))
  
  cat("\n")
  
  if (is.null(x)) {
    "{emoji(\"white_check_mark\")} {style_bold(\"OpenBLAS\")} version {style_bold({{substr(download$last_version, 2L, nchar(download$last_version))}})}." %>%
      glue %>%
      cat
    
  } else {
    "{emoji(\"white_check_mark\")} {style_bold(\"OpenBLAS\")} version {style_bold({x})}." %>%
      glue %>%
      cat
  }
  
}

#' @importFrom stringr str_remove
#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom RCurl getURL
#' @importFrom XML getHTMLLinks
#' @title \R language versions
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
#' # last_version_r(major = NULL)
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
#' @importFrom cli style_bold symbol
download_r <- function(x) {
  if (dir.exists("/tmp/r"))
    "/tmp/r" %>% unlink(recursive = TRUE)
  
  "/tmp/r" %>% dir.create %>% glue
  
  path_r <- "/tmp/r"
  
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

attention <- function(x) {
  cat("\n")
  
  cat(rule(
    width = 60L,
    center = glue("{style_bold(\"VERY ATTENTION\")}"),
    col = "red",
    background_col = "gray90",
    line = 2L
  ))
  
  cat("\n")
  
  glue(
    'The {style_bold("ropenblas")} package depends on versions of {style_bold("R")} ',
    '{style_bold(symbol$geq)} {style_bold("3.1.0")}. Therefore,',
    'you will not be able to \nuse this package to go back to a later version of {style_bold("R")}!'
  ) %>% cat
  
  answer <- NULL
  for (i in 1L:3L) {
    answer[i] <-
      answer_yes_no(text = glue("{emoji(\"large_blue_circle\")} Do you understand? ({i} of 3)"))
    validate_answer(answer[i])
  }
  
  answer
}

#' @importFrom glue glue
#' @importFrom fs dir_exists
#' @importFrom magrittr "%>%"
#' @importFrom emojifont emoji
change_r <- function (x, change = TRUE, key_root) {
  exist_version_r <- "/opt/R/{x}" %>%
    glue %>%
    dir.exists
  
  dir_r  <-  paste(system(command = "which R", intern = TRUE))
  dir_rscript  <-
    paste(system(command = "which Rscript", intern = TRUE))
  
  if (change) {
    "ln -sf /opt/R/{x}/bin/R {dir_r}"  %>%
      glue %>%
     run_command(key_root = key_root)
    
    "ln -sf /opt/R/{x}/bin/Rscript {dir_rscript}" %>%
      glue %>%
      run_command(key_root = key_root)
  }
  
  cat("\n")
  
  cat(rule(
    width = 50L,
    center = glue("{style_bold(\"Procedure Completed\")}"),
    col = "blue",
    background_col = "gray90",
    line = 2L
  ))
  
   cat("\n")
  
   "{emoji(\"white_check_mark\")} {style_underline(style_bold(\"R\"))} version {style_underline(style_bold({x}))}." %>%
     glue %>%
     cat

   cat("\n")

   "{emoji(\"arrows_counterclockwise\")} The roles are active after terminating the current {style_underline(style_bold(\"R\"))} session ..." %>%
      glue %>%
      style_bold %>%
      cat
}

fix_openblas_link <- function(restart_r = FALSE, key_root) {
  path_blas <-
    system("Rscript -e 'sessionInfo()$BLAS[1L]'", intern = TRUE) %>% 
    tail(n = 1L) %>% 
    str_match(string = .,pattern = "/([^;]*).so")
  
  path_blas <- path_blas[1L, 1L]
  
  fix <- str_detect(string = path_blas, pattern = "openblas")
  
  if (!fix)  
      glue("ln -snf /opt/OpenBLAS/lib/libopenblas.so {path_blas}") %>% run_command(key_root = key_root)
  
  if (restart_r) {
    if (rstudioapi::isAvailable()) {
      tmp <- rstudioapi::restartSession() # .rs.restartR()
    } else {
      .refresh_terminal()
    }
  }

}

#' @importFrom withr with_dir
#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom git2r checkout
#' @importFrom fs dir_delete
#' @importFrom fs dir_exists path_split
#' @importFrom stringr str_extract
#' @importFrom emojifont emoji
compiler_r <- function(r_version = NULL,
                       with_blas = NULL,
                       complementary_flags = NULL,
                       key_root) {
  if (is.null(r_version))
    r_version <- last_version_r()$last_version
  
  dir_initial_blas <-
    glue("{dir_blas()$path_blas}{dir_blas()$file_blas}")
  

  if ("/opt/R/{r_version}" %>% glue %>% dir.exists) {
    answer <-
      "{emoji(\"large_blue_circle\")} R version already compiled: (yes - changes without recompiling) and (no - compiles again)"  %>%
      glue %>% 
      answer_yes_no
    
    validate_answer(answer)
    
    if (answer %in% c("y", "yes")) {
      fix_openblas_link(restart_r = FALSE, key_root = key_root)
      return(change_r(r_version, key_root = key_root))
    }
  }
  
  dir_r  <-  paste(system(command = "which R", intern = TRUE))
  dir_rscript <-
    paste(system(command = "which Rscript", intern = TRUE))
  
  download <- download_r(x = r_version)
  
  if (is.null(with_blas)) {
    with_blas <-  "-L/opt/OpenBLAS/lib \\
     -I/opt/OpenBLAS/include \\
     -lpthread \\
     -lm" %>%
      glue
  }
  
  if (is.null(complementary_flags))
    complementary_flags <- ""
  
  configure <-
    "./configure \\
     --prefix=/opt/R/{r_version} \\
     --enable-memory-profiling \\
     --enable-R-shlib \\
     --enable-threads=posix \\
     --with-blas=\"{with_blas}\" \\
     {complementary_flags}" %>%
    glue
  
  if (dir.exists("/opt/OpenBLAS/lib/") && dir_blas()$use_openblas) {
    # configure ---------------------------------------------------------------
    
    with_dir(new = download,
             code = run_command(x = configure, key_root = NULL)) # loop_root(configure, sudo = FALSE))
    
    # make --------------------------------------------------------------------
    
    with_dir(new = download,
             code = run_command(x = "make -j $(nproc)", key_root = NULL)) # loop_root("make -j $(nproc)", sudo = FALSE))
    
    # make install ------------------------------------------------------------
    
    with_dir(
      new = download,
      code = run_command(x = "make install PREFIX=/opt/R/{r_version}", key_root = key_root)
    )
    
    # creating symbolic links -------------------------------------------------

    "ln -sf /opt/R/{r_version}/bin/R {dir_r}"  %>%
      glue %>%
      run_command(key_root = key_root) # loop_root(attempt = 5L, sudo =  is_sudo()$r)

    "ln -sf /opt/R/{r_version}/bin/Rscript {dir_rscript}" %>%
      glue %>%
      run_command(key_root = key_root) # loop_root(attempt = 5L, sudo =  is_sudo()$rscript)
    
    fix_openblas_link(restart_r = FALSE, key_root = key_root)
    
  } else {
    
    # env_ropenblas_compiler_r <<- env(empty_env(), new_ropenblas = ropenblas, root = key_root)
    assign(x = "env_ropenblas_compiler_r", value = env(new_ropenblas = ropenblas, root = key_root), envir = global_env())  
    exec(.fn = "new_ropenblas", .env = env_ropenblas_compiler_r, x = NULL, restart = FALSE) 
    
    # configure ---------------------------------------------------------------
    
    with_dir(new = download,
             code = run_command(x = configure, key_root = NULL))
    
    # make --------------------------------------------------------------------
    
    with_dir(new = download,
             code = run_command(x = "make -j $(nproc)", key_root = NULL))
    
    # make install ------------------------------------------------------------
    
    with_dir(
      new = download,
      code = run_command(x = "make install PREFIX=/opt/R/{r_version}", key_root = key_root)
    )
    
    fix_openblas_link(restart_r = FALSE, key_root = key_root)
    
  }
  
  cat("\n")
  
  "{emoji(\"white_check_mark\")} {style_underline(style_bold(\"R\"))} version {style_underline(style_bold({r_version}))}." %>%
    glue %>%
    cat
  
  cat("\n")
  
  "{emoji(\"arrows_counterclockwise\")} The roles are active after terminating the current {style_underline(style_bold(\"R\"))} session ...\n\n" %>%
    glue %>%
    style_bold %>%
    cat
  
  # # build library directory -------------------------------------------------
  # 
  # version <- str_extract(string = r_version, pattern = "\\d.\\d")
  # 
  # path_library <- unlist(path_split(.libPaths()[1L]))
  # path_library <- path_join(path_library[-length(path_library)])
  # 
  # dir_path <- glue("{path_library}/{version}")
  # 
  # if (!dir_exists(path = dir_path))
  #   dir_create(path = dir_path)
  
}

#' @importFrom fs dir_exists
#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom getPass getPass
#' @importFrom cli rule symbol style_bold
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
                      with_blas = NULL,
                      complementary_flags = NULL) {
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
  
  root <- sudo_key()
  
  compiler_r(
    r_version = x,
    with_blas = with_blas,
    complementary_flags = complementary_flags,
    key_root = root
  )
  
}

#' @importFrom magrittr "%>%"
#' @importFrom fs file_exists dir_create
#' @importFrom git2r clone tags remote_ls
#' @importFrom glue glue
#' @importFrom stringr str_extract
#' @importFrom stats na.omit
#' @title OpenBLAS library versions
#' @details This function automatically searches \href{https://www.openblas.net/}{\strong{OpenBLAS}} library versions in the official \href{https://github.com/xianyi/OpenBLAS}{\strong{GitHub}} project.
#' \enumerate{
#'    \item \code{last_version}: Returns the latest stable version of the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library.
#'    \item \code{versions}: All stable versions of the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library.
#'    \item \code{n}: Total number of versions.
#' }
#' @seealso \code{\link{last_version_r}}, \code{\link{ropenblas}}, \code{\link{rcompiler}}
#' @examples
#' # last_version_openblas()
#' @export
last_version_openblas <- function() {
  if (!connection())
    stop("You apparently have no internet connection.\n")
  
  pulls <- "https://github.com/xianyi/OpenBLAS.git" %>%
    remote_ls %>%
    names
  
  versions <-
    pulls %>% str_extract(pattern = "v[:digit:][:punct:][:graph:]+") %>%
    na.omit %>%
    str_remove(pattern = "\\^\\{\\}") %>%
    unique %>%
    str_remove(pattern = "^v")
  
  list(
    last_version = versions[length(versions)],
    versions = versions,
    n = length(versions)
  )
  
}

#' @importFrom magrittr "%>%"
#' @importFrom glue glue
#' @importFrom fs dir_exists
#' @importFrom cli style_bold style_underline symbol
#' @importFrom emojifont emoji
#' @title Linking the OpenBLAS library with \R again
#' @description The \code{link_again} function links again the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library with the \R language, being useful to correct problems
#' of untying the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library that is common when the operating system is updated.
#' @param restart_r If \code{TRUE} (default), a new section of \R is started after linking the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library.
#' @details The function \code{link_again} be able to link again the \R language with the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library. Thus, link_again will only make the
#' relinkagem when in some previous section of \R the ropenblas function has been used for the initial binding of the \R language with the
#' \href{https://www.openblas.net/}{\strong{OpenBLAS}} library.
#'
#' Relinking is useful in situations of updating the operating system. In some update, it is possible that the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library compiled
#' in the \code{/opt} directory is unlinked. In this scenario, when the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library has already been compiled using the ropenblas
#' function, the \code{link_again} function performs a new link without the need to recompile, thus making the process less time
#' consuming.
#' @note In situations where there was a disconnection due to an update of the operating system, the `ropenblas` function can be used to
#' re-link the OpenBLAS library with the \R language, however, it will be necessary to compile the
#' \href{https://www.openblas.net/}{\strong{OpenBLAS}} library again. If you are interested in recompiling the
#' \href{https://www.openblas.net/}{\strong{OpenBLAS}} library and linking with \R, use the \code{\link{ropenblas}} function. If the
#' interest is to take advantage of a previous compilation of the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library, the
#' function \code{link_again} may be useful.
#' @seealso \code{\link{ropenblas}}
#' @examples
#' # link_again()
#' @export
link_again <- function(restart_r = TRUE) {
  if (dir_blas()$use_openblas) {
    "{emoji(\"large_blue_circle\")} Linking again is not necessary. {style_underline(style_bold(\"R\"))} \\
      already uses the {style_underline(style_bold(\"OpenBLAS\"))} library. You can stay calm." %>%
      glue
  } else {
    if (!dir.exists("/opt/OpenBLAS/lib"))
      "{emoji(\"large_blue_circle\")} Run the {style_underline(style_bold(\"ropenblas()\"))} function ..." %>%
      glue
    else {
      root <- sudo_key()
      glue(
        "ln -snf /opt/OpenBLAS/lib/libopenblas.so {dir_blas()$path}{dir_blas()$file_blas}"
      ) %>% run_command(key_root = root) # loop_root(attempt = 5L, sudo =  is_sudo()$blas)
      
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
      
      cat(rule(
        width = 50L,
        center = glue("{style_bold(\"Procedure Completed\")}"),
        col = "blue",
        background_col = "gray90",
        line = 2L
      ))
      
      cat("\n")
      
      if (restart_r) {
        "{emoji(\"white_check_mark\")} {style_underline(style_bold(\"OpenBLAS\"))}." %>%
          glue %>%
          cat
      } else {
        "{emoji(\"white_check_mark\")} {style_underline(style_bold(\"OpenBLAS\"))} will be used in the next section." %>%
          glue %>%
          cat
      }
      
    }
  }
}
