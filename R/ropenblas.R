answer_yes_no <- function(text) {
  readline(prompt = glue("{text} (yes/no): ")) %>%
    tolower
}

#' @importFrom pingr is_online
#' @importFrom cli col_red symbol
connection <- function() {
  if (is_online())
    TRUE
  else{
    cat(glue(
      "\r{cli::col_red(cli::symbol$bullet)} You apparently have no internet connection. "
    ))
    FALSE
  }
}

#' @importFrom magrittr "%>%"
#' @importFrom glue glue
#' @importFrom stringr str_extract
#' @importFrom stats na.omit
#' @importFrom git2r remote_ls
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
    stop("")
  
  all <- 
    system(command = "git ls-remote --tags https://github.com/xianyi/OpenBLAS.git | sort -t '/' -k 3 -V",
           intern = T) %>% 
    str_extract(pattern = "v[:digit:][:punct:][:graph:]+") %>%
    str_remove(pattern = "\\^\\{\\}") %>%
    na.omit %>%
    unique %>% 
    str_remove(pattern = "^v")
  last <- all[length(all)]
  
  list(
    last_version = last,
    versions = all,
    n = length(all)
  )
  
}

comp_last  <- function(x, comp = "<") {
  all <- last_version_openblas()$versions
  last <- last_version_openblas()$last_version
  
  glue("match(x, all) {comp} match(last, all)") %>% 
    parse(text = .) %>% 
    eval
}

download_openblas <- function(x = NULL) {
  if (dir.exists("/tmp/openblas"))
    unlink("/tmp/openblas", recursive = TRUE)
  
  dir.create("/tmp/openblas")
  path_openblas <- "/tmp/openblas"
  
  repo_openblas <-
    git2r::clone(
      url = "https://github.com/xianyi/OpenBLAS.git", 
      local_path = path_openblas,
      branch = "develop"
    )
  
  last_version <- last_version_openblas()$last_version
  
  if (is.null(x))
    x <- last_version
  
  list(
    last_version = last_version,
    path_openblas = path_openblas,
    repo_openblas = repo_openblas
  )
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

#' @importFrom cli style_bold col_red symbol
validate_answer <- function(x) {
  if (!(x %in% c("y", "no", "yes", "n")))
    stop(glue(
      "\r{cli::col_red(cli::symbol$bullet)} Invalid option. Procedure interrupted."
    ))
}

modern_openblas <- function(x) {
  if (!stringr::str_detect(dir_blas()$file, "openblas"))
    return(FALSE)
  version_sistem <- dir_blas()$file %>%
    stringr::str_extract(pattern = "[0-9]+.[0-9]+.[0-9]+")
  x > version_sistem
}

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
      getPass::getPass(
        glue(
          "Enter your ROOT OS password (attempt {i} of {attempt}): "
        )
      )
    key_true <- test_root(key_root = key_root)
    
    if (!key_true && attempt == i)
      stop(
        "\rSorry. Apparently you don't is the administrator of the operating system. You missed all three attempts."
      )
    
    i <- i + 1L
  }
  key_root
}

run_command <- function(x, key_root = NULL) {
  if (is.null(key_root))
    system(command = glue("{x}"), ignore.stderr = TRUE)
  else
    system(command = glue("echo {key_root} | sudo -S -k {x}"),
           ignore.stderr = TRUE)
}

#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom git2r checkout
#' @importFrom cli rule col_red symbol style_bold
compiler_openblas <-
  function(download,
           openblas_version = NULL, key_root) {
    # download <- download_openblas(x = openblas_version)
    
    if (!exist())
      stop(
        glue(
          "\r{cli::col_red(cli::symbol$bullet)} GNU GCC not installed. Install GNU GCC Compiler (C and Fortran) on your operating system."
        )
      )
    if (!exist("make"))
      stop(
        glue(
          "\r{cli::col_red(cli::symbol$bullet)} GNU Make not installed. Install GNU Make on your operating system."
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
    
    
    if (!dir.exists(paths = "/opt"))
      run_command(x = "mkdir /opt", key_root = key_root)
    
    "cp {dir_blas()$path}{dir_blas()$file_blas} /opt/{dir_blas()$file_blas}" %>%
      glue %>%
      run_command(key_root)
    
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
#' @importFrom rlang caller_env global_env env_get
#' @importFrom cli rule symbol style_bold col_green col_blue col_red
#' @seealso \code{\link{rcompiler}}, \code{\link{last_version_r}}
#' @examples
#' # ropenblas()
#' @export
ropenblas <- function(x = NULL, restart_r = TRUE) {
  if (Sys.info()[[1L]] != "Linux")
    stop(
      glue(
        "\r{cli::col_red(cli::symbol$bullet)} Sorry, this package for now configures {style_bold(\"R\")} to use the {style_bold(\"OpenBLAS\")} library on Linux systems.\n"
      )
    )
  
  if (!connection())
    stop("")
  
  if (identical(caller_env(), global_env())) {
    root <- sudo_key()
  } else {
    root <- env_get(env = env_ropenblas_compiler_r, nm = "root")
    rm(env_ropenblas_compiler_r, envir = global_env())
  }
  
  initial_blas <- dir_blas()$file_blas
  download <- download_openblas(x)
  
  if (!is.null(x) && is.na(comp_last(x, comp = ">")))
    stop(
      glue(
        "\r{cli::col_red(cli::symbol$bullet)} Version {style_bold({x})} does not exist. The latest version of {style_bold(\"OpenBLAS\")} is {style_bold({download$last_version})}."
      )
    )
  
  if (!is.null(x)) {
    if (dir_blas()$use_openblas) {
      if (comp_last(dir_blas()$version_openblas, comp = "<")) {
        cat("\n")
        if (comp_last(x, comp = "!=")) {
          answer <-
            "{cli::col_blue(cli::symbol$bullet)} The latest version of {style_bold(\"OpenBLAS\")} is {style_bold({download$last_version})}. Do you want to install this version?" %>%
            glue %>%
            answer_yes_no
          
          validate_answer(answer)
          
        } else {
          answer <- "y"
        }
        
        if (answer %in% c("y", "yes")) {
          compiler_openblas(
            download = download,
            openblas_version = glue("v{download$last_version}"),
            key_root = root
          )
        } else {
          compiler_openblas(
            download = download,
            openblas_version = glue("v{x}"),
            key_root = root
          )
        }
      } else {
        if (comp_last(dir_blas()$version_openblas, comp = "==")) {
          answer <-
            "{cli::col_blue(cli::symbol$bullet)} The latest version of {style_bold(\"OpenBLAS\")} is already in use. Do you want to compile and link again?" %>%
            glue %>%
            answer_yes_no
          
          validate_answer(answer)
          
          if (answer %in% c("y", "yes")) {
            compiler_openblas(
              download = download,
              openblas_version = glue("v{x}"),
              key_root = root
            )
          } else {
            return(warning("Ok, procedure interrupted!"))
          }
        } else {
          stop(
            glue(
              "{cli::col_red(cli::symbol$bullet)} There is no {style_bold(\"OpenBLAS\")} version {style_bold({x})}. The latest version is {style_bold({{substr(download$last_version, 2L, nchar(download$last_version))}})}."
            )
          )
        }
      }
    } else {
      if (comp_last(x, comp = "<")) {
        answer <-
          "{cli::col_blue(cli::symbol$bullet)} The latest version is {style_bold({download$last_version})}. Want to consider the latest version?" %>%
          glue %>%
          answer_yes_no
        
        validate_answer(answer)
        
        if (answer %in% c("y", "yes")) {
          compiler_openblas(
            download = download,
            openblas_version = glue("v{download$last_version}"),
            key_root = root
          )
        } else {
          compiler_openblas(
            download = download,
            openblas_version = glue("v{x}"),
            key_root = root
          )
        }
        
      } else {
        compiler_openblas(
          download = download,
          openblas_version = glue("v{x}"),
          key_root = root
        )
      }
      
    }
  } else {
    if (dir_blas()$use_openblas) {
      if (comp_last(dir_blas()$version_openblas, comp = "<")) {
        compiler_openblas(
          download = download,
          openblas_version = glue("v{download$last_version}"),
          key_root = root
        )
      } else {
        answer <-
          "{cli::col_blue(cli::symbol$bullet)} The latest version of {style_bold(\"OpenBLAS\")} is already in use. Do you want to compile and link again?" %>%
          glue %>%
          answer_yes_no
        validate_answer(answer)
        
        if (answer %in% c("n", "no")) {
          return(warning("Ok, procedure interrupted!"))
        } else {
          compiler_openblas(
            download = download,
            openblas_version = glue("v{download$last_version}"),
            key_root = root
          )
        }
        
      }
    } else {
      compiler_openblas(
        download = download,
        openblas_version = glue("v{download$last_version}"),
        key_root = root
      )
    }
    
  }
  
  glue(
    "ln -snf /opt/OpenBLAS/lib/libopenblas.so {dir_blas()$path}{dir_blas()$file_blas}"
  ) %>% run_command(key_root = root)
  
  if (error_r()) {
    "mv /opt/OpenBLAS/{initial_blas} {dir_blas()$path}" %>%
      glue %>%
      run_command(key_root = root)
    
    cat("\n")
    
    cat(rule(
      width = 50L,
      center = glue("{style_bold(\"Procedure Incompleted\")}"),
      col = "red",
      background_col = "gray90",
      line = 2L
    ))
    
    "[{style_bold(symbol$cross)}] Some error has occurred. No changes have been made." %>%
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
    "[{style_bold(cli::col_green(symbol$tick))}] {style_bold(\"OpenBLAS\")} version {style_bold({download$last_version})}." %>%
      glue %>%
      cat
    
  } else {
    "[{style_bold(cli::col_green(symbol$tick))}] {style_bold(\"OpenBLAS\")} version {style_bold({x})}." %>%
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
    stop("")
  
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
#' @importFrom glue glue
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

#' @importFrom cli style_bold symbol
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
      answer_yes_no(text = glue("{cli::col_red(cli::symbol$bullet)} Do you understand? ({i} of 3)"))
    validate_answer(answer[i])
  }
  
  answer
}

#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom cli col_green style_bold style_underline
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
  
  "[{style_bold(cli::col_green(symbol$tick))}] {style_underline(style_bold(\"R\"))} version {style_underline(style_bold({x}))}." %>%
    glue %>%
    cat
  
  cat("\n")
  
  "{cli::symbol$mustache} The roles are active after terminating the current {style_underline(style_bold(\"R\"))} session ..." %>%
    glue %>%
    style_bold %>%
    cat
}

#' @importFrom stringr str_match str_detect
#' @importFrom magrittr "%>%"
fix_openblas_link <- function(restart_r = FALSE, key_root) {
  path_blas <-
    system("Rscript -e 'sessionInfo()$BLAS[1L]'", intern = TRUE) %>%
    tail(n = 1L) %>%
    str_match(string = ., pattern = "/([^;]*).so")
  
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

#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom git2r checkout
#' @importFrom stringr str_extract
#' @importFrom withr with_dir
#' @importFrom rlang env exec
#' @importFrom cli col_blue col_green style_underline style_bold symbol
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
      "{cli::col_blue(cli::symbol$bullet)} R version already compiled: (yes - changes without recompiling) and (no - compiles again)"  %>%
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
             code = run_command(x = configure, key_root = NULL))
    
    # make --------------------------------------------------------------------
    
    with_dir(new = download,
             code = run_command(x = "make -j $(nproc)", key_root = NULL))
    
    # make install ------------------------------------------------------------
    
    with_dir(
      new = download,
      code = run_command(x = "make install PREFIX=/opt/R/{r_version}", key_root = key_root)
    )
    
    # creating symbolic links -------------------------------------------------
    
    "ln -sf /opt/R/{r_version}/bin/R {dir_r}"  %>%
      glue %>%
      run_command(key_root = key_root)
    
    "ln -sf /opt/R/{r_version}/bin/Rscript {dir_rscript}" %>%
      glue %>%
      run_command(key_root = key_root)
    
    fix_openblas_link(restart_r = FALSE, key_root = key_root)
    
  } else {
    assign(
      x = "env_ropenblas_compiler_r",
      value = env(new_ropenblas = ropenblas, root = key_root),
      envir = global_env()
    )
    exec(
      .fn = "new_ropenblas",
      .env = env_ropenblas_compiler_r,
      x = NULL,
      restart = FALSE
    )
    
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
  
  "[{style_bold(cli::col_green(symbol$tick))}] {style_underline(style_bold(\"R\"))} version {style_underline(style_bold({r_version}))}." %>%
    glue %>%
    cat
  
  cat("\n")
  
  "{cli::symbol$mustache} The roles are active after terminating the current {style_underline(style_bold(\"R\"))} session ...\n\n" %>%
    glue %>%
    style_bold %>%
    cat
}

#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom getPass getPass
#' @importFrom cli col_blue col_red symbol
#' @title Compile a version of \R on GNU/Linux systems
#' @description This function is responsible for compiling a version of the \R language.
#' @param x Version of \R you want to compile. By default (\code{x = NULL}) will be compiled the latest stable version of the \R
#' language. For example, \code{x = "3.6.2"} will compile and link \strong{R-3.6.2} version  as the major version on your system.
#' @param with_blas String, `--with-blas = NULL` by default, with flags for `--with-blas` used in the \R compilation process.
#' Details on the use of this flag can be found [**here**](https://cran.r-project.org/doc/manuals/r-devel/R-admin.html).
#' @param complementary_flags String, `complementary_flags = NULL` by default, for adding complementary flags in the [**R**](https://www.r-project.org/) language
#'  compilation process.
#' @details
#' This function is responsible for compiling a version of the [**R**](https://www.r-project.org/) language. The `x` argument is the version of [**R**](https://www.r-project.org/) that you want to compile.
#' For example, `x = "4.0.0"` will compile and link **R-4.0.0** version  as the major version on your system. By default (`x = NULL`) will be compiled the latest stable version of the [**R**](https://www.r-project.org/).
#'
#' For example, to compile the latest stable version of the [**R**](https://www.r-project.org/) language, do:
#'    ```
#'     rcompiler()
#'    ```
#' Regardless of your GNU/Linux distribution and what version of [**R**](https://www.r-project.org/) is in your repositories, you can have the latest stable version of the [**R**](https://www.r-project.org/) language compiled
#' into your computer architecture.
#'
#' You can use the `rcompiler()` function to compile different versions of [**R**](https://www.r-project.org/). For example, running `rcompiler(x = "3.6.3")` and `rcompiler()` will install versions 3.6.3 and 4.0.0 on its GNU/Linux distribution,
#' respectively. If you are in version 4.0.0 of [**R**](https://www.r-project.org/) and run the code `rcompiler(x = "3.6.3")` again, the function will identify the existence of version 3.6.3 in the system and give you the option to use the binaries
#' that were built in a previous compilation. This avoids unnecessarys compilations.
#'
#' In addition to the `x` argument, the` rcompiler()` function has two other arguments that will allow you to change and pass new compilation flags. Are they:
#' 1. `with_blas`: This argument sets the `--with-blas` flag in the R language compilation process and must be passed as a string. Details on the use of this flag
#' can be found [**here**](https://cran.r-project.org/doc/manuals/r-devel/R-admin.html). If `with_blas = NULL` (default), then it will be considered:
#'    ```
#'    ./configure --prefix=/opt/R/version_r --enable-memory-profiling --enable-R-shlib
#'     --enable-threads=posix --with-blas="-L/opt/OpenBLAS/lib -I/opt/OpenBLAS/include
#'     -lpthread -lm"
#'    ```
#'    Most likely, you will have little reason to change this aprgument. Unless you know what you're doing, consider `with_blas = NULL`. Do not change the installation directory,
#' that is, always consider `--prefix = /opt/R/version_r`, where` version_r` is a valid version of [**R**](https://www.r-project.org/). For a list of valid versions of
#' [**R**](https://www.r-project.org/), run the `last_version_r()`. Installing [**R**](https://www.r-project.org/) in the `/opt/R/version_r` directory is important because some
#' functions in the package require this. Both the [**R**](https://www.r-project.org/) language and the [**OpenBLAS**](https://www.openblas.net/) library will be installed in the `/opt` directory.
#' If this directory does not exist in your GNU/Linux distribution, it will be created;
#' 2. `complementary_flags`: String (`complementary_flags = NULL` by default) for adding complementary flags in the [**R**](https://www.r-project.org/) language compilation process.
#' Passing a string to `complementary_flags` will compile it in the form:
#'    ```
#'    ./configure --with-blas="..." complementary_flags
#'    ```
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
    stop(
      glue(
        "\r{cli::col_red(cli::symbol$bullet)} Sorry, this package for now configures R to use the OpenBLAS library on Linux systems.\n"
      )
    )
  
  if (!connection())
    stop("")
  
  if (!exist())
    stop(
      glue(
        "\r{cli::col_red(cli::symbol$bullet)} GNU GCC Compiler not installed. Install GNU GCC Compiler (C and Fortran) on your operating system.\n"
      )
    )
  if (!exist("make"))
    stop(
      glue(
        "\r{cli::col_red(cli::symbol$bullet)} GNU Make not installed. Install GNU Make on your operating system.\n"
      )
    )
  
  if (!is.null(x) && x < "3.1.0") {
    answer <- attention(x)
    if (any(answer != "y" || answer != "yes"))
      return(warning(
        glue(
          "\r{cli::col_blue(cli::symbol$bullet)} Given the answers, it is not possible to continue ..."
        )
      ))
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
#' @importFrom glue glue
#' @importFrom cli style_bold style_underline symbol col_green col_blue
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
    "{cli::col_blue(cli::symbol$bullet)} Linking again is not necessary. {style_underline(style_bold(\"R\"))} \\
      already uses the {style_underline(style_bold(\"OpenBLAS\"))} library. You can stay calm." %>%
      glue
  } else {
    if (!dir.exists("/opt/OpenBLAS/lib"))
      "{cli::col_blue(cli::symbol$bullet)} Run the {style_underline(style_bold(\"ropenblas()\"))} function ..." %>%
      glue
    else {
      root <- sudo_key()
      glue(
        "ln -snf /opt/OpenBLAS/lib/libopenblas.so {dir_blas()$path}{dir_blas()$file_blas}"
      ) %>% run_command(key_root = root)
      
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
        "[{style_bold(cli::col_green(symbol$tick))}] {style_underline(style_bold(\"OpenBLAS\"))}." %>%
          glue %>%
          cat
      } else {
        "[{style_bold(cli::col_green(symbol$tick))}] {style_underline(style_bold(\"OpenBLAS\"))} will be used in the next section." %>%
          glue %>%
          cat
      }
      
    }
  }
}

#' @importFrom fs file_show
#' @title R News file
#' @description Returns the contents of the \href{https://cran.r-project.org/doc/manuals/r-release/NEWS.html}{NEWS.html} file in the standard browser installed on the operating system.
#' @param pdf If `FALSE` (default), the \href{https://cran.r-project.org/doc/manuals/r-release/NEWS.html}{NEWS.html} file will open in the browser, otherwise
#' \href{https://cran.r-project.org/doc/manuals/r-release/NEWS.pdf}{NEWS.pdf} will be opened.
#' @param dev If `FALSE` (default), it will not show changes made to the language development version. 
#' To see changes in the development version, do `dev = TRUE`.
#' @details The \href{https://cran.r-project.org/doc/manuals/r-release/NEWS.html}{NEWS.html} file contains the main changes from the recently released versions of the \R language. 
#' The goal is to facilitate the query by invoking it directly from the \R command prompt. The \link{rnews} function is 
#' analogous to the \link{news} function of the **utils** package. However, using the \link{news} command in a terminal style 
#' bash shell is possible to receive a message like:
#' ```  
#' news()
#' starting httpd help server ... done
#' Error in browseURL(url) : 'browser' must be a non-empty character string
#' ```
#' This is an error that may occur depending on the installation of \R. Always prefer the use of the news
#' function but if you need to, use the \link{rnews} function.
#' @export
rnews <- function(pdf = FALSE, dev = FALSE) {
  
  extension <-
    ifelse(pdf,
           "pdf",
           "html")
  
  r_v <-
    ifelse(dev,
           "r-devel",
           "r-release")
  
  "https://cran.r-project.org/doc/manuals/{r_v}/NEWS.{extension}" %>%
    glue %>% 
    file_show
}