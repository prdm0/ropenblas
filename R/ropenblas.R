answer_yes_no <- function(text) {
  readline(prompt = glue::glue("{text} (yes/no): ")) %>%
    tolower
}

#' @importFrom pingr is_online
#' @importFrom cli col_red symbol
connection <- function() {
  if (pingr::is_online()) {
    TRUE
  } else {
    cat(glue::glue(
      "\r{cli::col_red(cli::symbol$bullet)} You apparently have no internet connection. "
    ))
    FALSE
  }
}

#' @importFrom magrittr "%>%"
#' @importFrom glue glue
#' @importFrom stringr str_extract str_remove
#' @importFrom stats na.omit
#' @importFrom git2r remote_ls
#' @title OpenBLAS library versions
#' @details This function automatically searches \href{https://www.openblas.net/}{\strong{OpenBLAS}} library versions in the official \href{https://github.com/OpenMathLib/OpenBLAS}{\strong{GitHub}} project.
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
  if (!connection()) {
    stop("")
  }

  all <-
    system(
      command = "git ls-remote --tags https://github.com/OpenMathLib/OpenBLAS.git | sort -t '/' -k 3 -V",
      intern = TRUE
    ) %>%
    stringr::str_extract(pattern = "v[:digit:][:punct:][:graph:]+") %>%
    stringr::str_remove(pattern = "\\^\\{\\}") %>%
    stats::na.omit() %>%
    unique() %>%
    stringr::str_remove(pattern = "^v")
  last <- all[length(all)]

  list(
    last_version = last,
    versions = all,
    n = length(all)
  )
}

comp_last <- function(x, comp = "<") {
  all <- last_version_openblas()$versions
  last <- last_version_openblas()$last_version

  glue::glue("match(x, all) {comp} match(last, all)") %>%
    parse(text = .) %>%
    eval
}

download_openblas <- function(x = NULL) {
  if (dir.exists("/tmp/openblas")) {
    unlink("/tmp/openblas", recursive = TRUE)
  }

  dir.create("/tmp/openblas")
  path_openblas <- "/tmp/openblas"

  repo_openblas <-
    git2r::clone(
      url = "https://github.com/OpenMathLib/OpenBLAS.git",
      local_path = path_openblas,
      branch = "develop"
    )

  last_version <- last_version_openblas()$last_version

  if (is.null(x)) {
    x <- last_version
  }

  list(
    last_version = last_version,
    path_openblas = path_openblas,
    repo_openblas = repo_openblas
  )
}

dir_blas <- function() {
  # Uses robust functions to extract path and filename
  blas_full_path <- sessionInfo()$BLAS
  file_blas <- basename(blas_full_path)
  path_blas <- dirname(blas_full_path)

  if (stringr::str_detect(file_blas, "openblas")) {
    use_openblas <- TRUE
    version_openblas <-
      stringr::str_extract(file_blas, pattern = "[0-9]+.[0-9]+.[0-9]+")
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
    function(...) {
      tryCatch(
        system(...),
        error = function(e) {
          FALSE
        }
      )
    }
  result <- glue::glue("{x} --version") %>% nsystem(intern = TRUE)
  ifelse(length(result) == 1L, FALSE, TRUE)
}

#' @importFrom cli style_bold col_red symbol
validate_answer <- function(x) {
  if (!(x %in% c("y", "no", "yes", "n"))) {
    stop(glue::glue(
      "\r{cli::col_red(cli::symbol$bullet)} Invalid option. Procedure interrupted."
    ))
  }
}

modern_openblas <- function(x) {
  if (!stringr::str_detect(dir_blas()$file, "openblas")) {
    return(FALSE)
  }
  version_sistem <- dir_blas()$file %>%
    stringr::str_extract(pattern = "[0-9]+.[0-9]+.[0-9]+")
  x > version_sistem
}

#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_warn
#' @importFrom getPass getPass
sudo_key <- function(attempt = 3L) {
  # Helper function to check for the existence of a command in the system
  command_exists <- function(cmd) {
    return(nzchar(Sys.which(cmd)))
  }

  # Helper function to check if the user has root privileges
  test_privileges <- function() {
    user <- suppressWarnings(system(
      "sudo -n whoami",
      intern = TRUE,
      ignore.stderr = TRUE
    ))
    return(identical(user, "root"))
  }

  # 1. Check if privileges are already active
  if (system("sudo -n true", ignore.stderr = TRUE, ignore.stdout = TRUE) == 0) {
    if (test_privileges()) {
      cli::cli_alert_success("Administrator privileges already active.")
      return(invisible(TRUE))
    }
  }

  cli::cli_alert_info("Administrator privileges are required to continue.")

  for (i in 1L:attempt) {
    password <- NULL

    is_gui_available <- nzchar(Sys.getenv("DISPLAY"))
    prompt_text <- glue::glue(
      "Please enter your password to allow system modifications.\nAttempt {i} of {attempt}."
    )

    if (is_gui_available && command_exists("zenity")) {
      if (i == 1L) {
        cli::cli_alert_info("Using graphical prompt for password entry...")
      }
      tryCatch(
        {
          password <- system(
            glue::glue(
              'zenity --password --title="Authentication Required" --text="{prompt_text}" 2>/dev/null'
            ),
            intern = TRUE
          )
          if (length(password) == 0L) {
            cli::cli_warn("Authentication cancelled by user.")
            return(FALSE)
          }
        },
        warning = function(w) {
          cli::cli_warn("Authentication cancelled by user.")
          return(FALSE)
        }
      )
    } else {
      if (i == 1L && is_gui_available && !command_exists("zenity")) {
        cli::cli_warn(
          "Package 'zenity' not found. Falling back to command-line password prompt."
        )
      }
      if (i == 1L) {
        cli::cli_alert_info("Using command-line prompt for password entry.")
      }
      password <- getPass::getPass(glue::glue(
        "Please enter your password for sudo (attempt {i} of {attempt}):"
      ))
    }

    if (is.null(password) || nchar(password) == 0) {
      cli::cli_warn("No password provided.")
      if (i == attempt) {
        cli::cli_alert_danger("Authentication failed after {attempt} attempts.")
        return(FALSE)
      }
      next
    }

    if (
      system2(
        "sudo",
        args = c("-S", "-v"),
        input = password,
        stdout = FALSE,
        stderr = FALSE
      ) ==
        0
    ) {
      if (test_privileges()) {
        cli::cli_alert_success("Authentication successful. Privileges granted.")
        return(invisible(TRUE))
      } else {
        cli::cli_alert_danger(
          "Password correct, but user lacks sufficient privileges."
        )
        return(FALSE)
      }
    } else {
      cli::cli_alert_danger("Authentication failed. Incorrect password.")
    }
  }

  cli::cli_alert_danger("Authentication failed after {attempt} attempts.")
  return(FALSE)
}

run_command <- function(x, key_root = NULL) {
  final_command <- if (isTRUE(key_root)) {
    paste("sudo", x)
  } else {
    x
  }
  cli::cli_inform(c("i" = "Running command: {.code {final_command}}"))

  exit_code <- system(final_command)

  invisible(exit_code)
}

#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_detect
error_r <- function() {
  run_r <- glue::glue("$(which R) --no-save&") %>%
    system(intern = TRUE)

  error <- any(FALSE, stringr::str_detect(run_r, pattern = "Error:"))

  if (length(error) == 0L) {
    error <- TRUE
  }

  error
}

#' @importFrom cli cli_alert_info
prompt_restart <- function() {
  cli::cli_alert_info(
    "IMPORTANT: Please restart your R session manually for the changes to take effect."
  )
}

#' @importFrom git2r checkout
#' @importFrom withr with_dir
compiler_openblas <- function(download, openblas_version = NULL) {
  version_to_checkout <- if (is.null(openblas_version)) {
    download$last_version
  } else {
    # Ensures 'v' is in the tag name
    if (!startsWith(openblas_version, "v")) {
      paste0("v", openblas_version)
    } else {
      openblas_version
    }
  }

  git2r::checkout(object = download$path_openblas, branch = version_to_checkout)

  # make (does not need sudo)
  withr::with_dir(
    new = "/tmp/openblas",
    code = run_command("make -j $(nproc)", key_root = FALSE)
  )

  if (!dir.exists("/opt")) {
    run_command("mkdir /opt", key_root = TRUE)
  }

  blas_info <- dir_blas()
  run_command(
    glue::glue(
      "cp {blas_info$path_blas}/{blas_info$file_blas} /opt/{blas_info$file_blas}_bak"
    ),
    key_root = TRUE
  )

  withr::with_dir(
    new = "/tmp/openblas",
    code = run_command("make install PREFIX=/opt/OpenBLAS", key_root = TRUE)
  )
}

#' @title Download, Compile and Link OpenBLAS Library with R
#' @author Pedro Rafael D. Marinho (e-mail: \email{pedro.rafael.marinho@gmail.com})
#' @description Links R with an optimized version of the \href{https://netlib.org/blas/}{\strong{BLAS}} library (\href{https://www.openblas.net/}{\strong{OpenBLAS}}).
#' @details The \code{ropenblas()} function will only work on Linux systems. When calling the \code{ropenblas()}
#' function on Windows, no settings will be made. Only a warning message will be issued informing you that the
#' configuration can only be performed on Linux systems.
#'
#' The function will automatically download the latest version of the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library. However, it is possible to
#' inform old versions to the single argument of \code{ropenblas()}. The \code{ropenblas()} function downloads,
#' compiles and links R to use the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library. Everything is done very simply, just by loading the library and
#' invoking the function \code{ropenblas()}.
#'
#' Considering using the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library rather than \href{https://netlib.org/blas/}{\strong{BLAS}} may bring extra optimizations for your code and improved
#' computational performance for your simulations, since \href{https://www.openblas.net/}{\strong{OpenBLAS}} is an optimized implementation of the \href{https://netlib.org/blas/}{\strong{BLAS}} library.
#' @note You do not have to use the \code{ropenblas()} function in every R session. Once the function is used, R
#' will always consider using the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library in future sessions.
#' @param x \href{https://www.openblas.net/}{\strong{OpenBLAS}} library version to be considered. By default, \code{x = NULL}.
#' @param restart_r If \code{TRUE}, a new R session is started after compiling and linking the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library.
#' @details You must install the following dependencies on your operating system (Linux):
#' \enumerate{
#'    \item \strong{GNU Make};
#'    \item \strong{GNU GCC Compiler (C and Fortran)}.
#' }
#' Your Linux operating system may already be configured to use the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library. Therefore, R will most likely already be linked to this library. To find out if R is using the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library,
#'  in R, do:
#'
#' \code{extSoftVersion()["BLAS"]}
#'
#' If R is using the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library, something like \code{/any_directory/libopenblas.so} should be returned. Therefore, the name 'openblas' should be in the returned \strong{s}hared \strong{o}bject (file with a \strong{.so} extension).
#'
#' If the \code{ropenblas()} function can identify that R is using the version of \href{https://www.openblas.net/}{\strong{OpenBLAS}} you wish to configure, a warning message will be returned asking if you would really like to proceed with the
#'  configuration again.
#'
#' The \code{ropenblas()} function will download the desired version of the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library, compile and install it in the \code{/opt} directory of your operating system. If the directory does not exist, it will
#'  be created so that the installation can be completed. Subsequently, files from the version of \href{https://netlib.org/blas/}{\strong{BLAS}} used in R will be symbolically linked to the shared object files of the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library version compiled and installed in \code{/opt}.
#'
#' You must be the operating system administrator to use this library. Therefore, do not attempt to use it without telling your system administrator. If you have the ROOT password, you will be responsible for everything you do on your operating system. Other details can also be found \href{https://prdm0.github.io/ropenblas/index.html}{\strong{here}}.
#' @return Returns a message informing you if the procedure occurred correctly. You will also be able to receive information about
#' missing dependencies.
#' @importFrom glue glue
#' @importFrom getPass getPass
#' @importFrom magrittr "%>%"
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
  if (Sys.info()[[1L]] != "Linux") {
    stop("Sorry, this package only configures R on Linux systems.")
  }
  if (!connection()) {
    stop("An internet connection is required.")
  }

  if (!sudo_key()) {
    stop("Aborting due to failed authentication.")
  }

  d_info <- dir_blas()
  download <- download_openblas(x)
  version_target <- if (is.null(x)) download$last_version else x

  if (!version_target %in% last_version_openblas()$versions) {
    stop(glue::glue("Version {version_target} of OpenBLAS does not exist."))
  }

  should_compile <- FALSE
  if (d_info$use_openblas) {
    if (d_info$version_openblas == version_target) {
      ans <- answer_yes_no(glue::glue(
        "Version {version_target} is already in use. Recompile?"
      ))
      if (ans %in% c("y", "yes")) should_compile <- TRUE
    } else {
      ans <- answer_yes_no(glue::glue(
        "Switch from OpenBLAS {d_info$version_openblas} to {version_target}?"
      ))
      if (ans %in% c("y", "yes")) should_compile <- TRUE
    }
  } else {
    cli::cli_alert_info(
      "Standard BLAS in use. Preparing to compile and link OpenBLAS {version_target}."
    )
    should_compile <- TRUE
  }

  if (!should_compile) {
    # Using `return` with `invisible()` to exit the function cleanly.
    cli::cli_alert_warning("Ok, procedure interrupted!")
    return(invisible())
  }

  compiler_openblas(download = download, openblas_version = version_target)

  target_path <- file.path(d_info$path_blas, d_info$file_blas)
  link_command <- paste(
    "ln -snf /opt/OpenBLAS/lib/libopenblas.so",
    shQuote(target_path)
  )
  run_command(link_command, key_root = TRUE)

  # --- Verification and Finalization ---
  if (error_r()) {
    cli::cli_alert_danger(
      "An error was detected after linking. Reverting changes..."
    )

    backup_path <- file.path("/opt", paste0(d_info$file_blas, "_bak"))
    original_path <- file.path(d_info$path_blas, d_info$file_blas)
    restore_command <- paste("mv", shQuote(backup_path), shQuote(original_path))
    run_command(restore_command, key_root = TRUE)

    cli::cli_alert_warning("Procedure failed and changes were reverted.")
    return(invisible())
  }

  cli::cli_rule(center = "Procedure Completed")
  cli::cli_alert_success(
    "OpenBLAS version {version_target} successfully linked."
  )

  if (restart_r) {
    prompt_restart()
  }

  return(invisible())
}

#' @importFrom stringr str_remove str_extract
#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom rvest read_html html_elements html_attr
#' @title R language versions
#' @seealso \code{\link{ropenblas}}, \code{\link{rcompiler}}
#' @examples
#' # last_version_r()
#' @export
last_version_r <- function() {
  cli::cli_alert_info("Checking for the latest R version on CRAN...")
  base_url <- "https://cloud.r-project.org/src/base/"

  # Scrape the main page to find the latest major version directory (e.g., "R-4/")
  main_page <- rvest::read_html(base_url)
  major_version_dirs <- main_page %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    grep("^R-[0-9]/", ., value = TRUE)

  latest_major_dir <- tail(sort(major_version_dirs), 1)

  # Scrape the major version page
  version_page_url <- paste0(base_url, latest_major_dir)
  version_page <- rvest::read_html(version_page_url)

  # Find all source archive links (.tar.gz or .tar.xz)
  archive_links <- version_page %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    grep("^R-[0-9]+\\.[0-9]+\\.[0-9]+\\.tar\\.(gz|xz)$", ., value = TRUE)

  if (length(archive_links) == 0) {
    stop("Could not find any R version archives on the CRAN page.")
  }

  # Extract only the version numbers
  versions <- stringr::str_extract(archive_links, "[0-9]+\\.[0-9]+\\.[0-9]+")

  # Sort using package_version to correctly handle version numbers and get the latest
  latest_version <- tail(sort(unique(package_version(versions))), 1)

  cli::cli_alert_success("Latest R version found: {latest_version}")
  return(as.character(latest_version))
}

#' @importFrom utils untar
#' @importFrom glue glue
download_r <- function(x) {
  if (dir.exists("/tmp/r")) {
    unlink("/tmp/r", recursive = TRUE)
  }
  dir.create("/tmp/r")
  path_r <- "/tmp/r"

  cli::cli_alert_info("Verifying available archive format for R version {x}...")

  major_version <- substr(x, 1L, 1L)
  base_url <- glue::glue(
    "https://cloud.r-project.org/src/base/R-{major_version}/"
  )

  # Try the .tar.xz extension first (more modern and smaller)
  url_xz <- glue::glue("{base_url}R-{x}.tar.xz")
  # Then try the .tar.gz extension (legacy)
  url_gz <- glue::glue("{base_url}R-{x}.tar.gz")

  final_url <- NULL

  # The `download.file` function with `method = "libcurl"` and quiet = TRUE returns 0 on success.
  # We use this to check for the file's existence without downloading the full content.
  # We need a temporary file for the test.
  temp_file_check <- tempfile()
  on.exit(unlink(temp_file_check), add = TRUE)

  # `try` is used to silence the error if the download fails (file does not exist)
  if (
    suppressWarnings(try(
      download.file(
        url_xz,
        destfile = temp_file_check,
        method = "libcurl",
        quiet = TRUE
      ),
      silent = TRUE
    )) ==
      0
  ) {
    final_url <- url_xz
    cli::cli_alert_info("Found .tar.xz format.")
  } else if (
    suppressWarnings(try(
      download.file(
        url_gz,
        destfile = temp_file_check,
        method = "libcurl",
        quiet = TRUE
      ),
      silent = TRUE
    )) ==
      0
  ) {
    final_url <- url_gz
    cli::cli_alert_info("Found .tar.gz format.")
  } else {
    cli::cli_abort(
      "Could not find a downloadable archive for R version {x} on CRAN."
    )
  }

  destfile_path <- file.path(path_r, basename(final_url))

  cli::cli_alert_info("Downloading from {final_url}...")
  download.file(url = final_url, destfile = destfile_path)

  cli::cli_alert_info("Unpacking archive...")
  # untar() handles .gz, .bz2, and .xz automatically
  untar(destfile_path, exdir = path_r)

  # The name of the unpacked directory is always R-{version}
  source_dir <- file.path(path_r, paste0("R-", x))

  if (!dir.exists(source_dir)) {
    # Fallback in case the directory name logic fails
    all_dirs <- list.dirs(path_r, full.names = TRUE, recursive = FALSE)
    source_dir <- all_dirs[grepl(paste0("^R-", x), basename(all_dirs))]
    if (length(source_dir) != 1) {
      cli::cli_abort("Could not identify the unpacked R source directory.")
    }
  }

  return(source_dir)
}

#' @importFrom cli style_bold symbol
attention <- function(x) {
  cat("\n")

  cat(cli::rule(
    width = 60L,
    center = glue::glue("{cli::style_bold(\"VERY ATTENTION\")}"),
    col = "red",
    background_col = "gray90",
    line = 2L
  ))

  cat("\n")

  glue::glue(
    'The {cli::style_bold("ropenblas")} package depends on versions of {cli::style_bold("R")} ',
    '{cli::style_bold(cli::symbol$geq)} {cli::style_bold("3.1.0")}. Therefore,',
    'you will not be able to \nuse this package to go back to a later version of {cli::style_bold("R")}!'
  ) %>%
    cat

  answer <- NULL
  for (i in 1L:3L) {
    answer[i] <-
      answer_yes_no(
        text = glue::glue(
          "{cli::col_red(cli::symbol$bullet)} Do you understand? ({i} of 3)"
        )
      )
    validate_answer(answer[i])
  }

  answer
}

#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom cli col_green style_bold style_underline
change_r <- function(x, change = TRUE, key_root) {
  exist_version_r <- glue::glue("/opt/R/{x}") %>%
    dir.exists

  dir_r <- paste(system(command = "which R", intern = TRUE))
  dir_rscript <-
    paste(system(command = "which Rscript", intern = TRUE))

  if (change) {
    glue::glue("ln -sf /opt/R/{x}/bin/R {dir_r}") %>%
      run_command(key_root = key_root)

    glue::glue("ln -sf /opt/R/{x}/bin/Rscript {dir_rscript}") %>%
      run_command(key_root = key_root)
  }

  cat("\n")

  cat(cli::rule(
    width = 50L,
    center = glue::glue("{cli::style_bold(\"Procedure Completed\")}"),
    col = "blue",
    background_col = "gray90",
    line = 2L
  ))

  cat("\n")

  glue::glue(
    "[{cli::style_bold(cli::col_green(cli::symbol$tick))}] {cli::style_underline(cli::style_bold(\"R\"))} version {cli::style_underline(cli::style_bold({x}))}."
  ) %>%
    cat

  cat("\n")

  glue::glue(
    "{cli::symbol$mustache} The changes will be active after terminating the current {cli::style_underline(cli::style_bold(\"R\"))} session ..."
  ) %>%
    cli::style_bold() %>%
    cat
}

#' @importFrom stringr str_match str_detect
#' @importFrom magrittr "%>%"
fix_openblas_link <- function(restart_r = FALSE, key_root) {
  path_blas <-
    system("Rscript -e 'sessionInfo()$BLAS[1L]'", intern = TRUE) %>%
    tail(n = 1L) %>%
    stringr::str_match(string = ., pattern = "/([^;]*).so")

  path_blas <- path_blas[1L, 1L]

  fix <- stringr::str_detect(string = path_blas, pattern = "openblas")

  if (!fix) {
    glue::glue("ln -snf /opt/OpenBLAS/lib/libopenblas.so {path_blas}") %>%
      run_command(key_root = key_root)
  }
}

#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom git2r checkout
#' @importFrom stringr str_extract
#' @importFrom withr with_dir
#' @importFrom rlang env exec
#' @importFrom cli col_blue col_green style_underline style_bold symbol
compiler_r <- function(r_version = NULL, key_root = NULL) {
  # Check for required packages
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("Please install the 'glue' package: install.packages('glue')")
  }
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("Please install the 'cli' package: install.packages('cli')")
  }
  if (!requireNamespace("withr", quietly = TRUE)) {
    stop("Please install the 'withr' package: install.packages('withr')")
  }

  # Get latest R version if none specified
  if (is.null(r_version)) {
    r_version <- last_version_r()
  }

  install_dir <- glue::glue("/opt/R/{r_version}")

  # Check if already installed
  if (dir.exists(install_dir)) {
    question <- glue::glue(
      "{cli::col_blue(cli::symbol$bullet)} R version {r_version} is already compiled. ",
      "Recompile? (no - just set as default, yes - recompile)"
    )
    answer <- answer_yes_no(question)
    validate_answer(answer)

    if (answer %in% c("n", "no")) {
      cli::cli_alert_info("Switching to R {r_version} without recompiling.")
      return(change_r(r_version, key_root = key_root))
    }
  }

  # Get current R paths
  dir_r <- tryCatch(
    system("which R", intern = TRUE),
    error = function(e) "/usr/local/bin/R"
  )
  dir_rscript <- tryCatch(
    system("which Rscript", intern = TRUE),
    error = function(e) "/usr/local/bin/Rscript"
  )

  # Download R source
  download_path <- download_r(x = r_version)

  # Configure command with fallback options if dependencies are missing
  configure_cmd <- glue::glue(
    "./configure --prefix={install_dir} \\
     --enable-R-shlib \\
     --enable-memory-profiling \\
     --with-x=yes \\
     --with-cairo=yes \\
     --with-libpng=yes \\
     --with-jpeglib=yes \\
     --with-libtiff=yes \\
     --with-readline=yes"
  )

  cli::cli_h1("Compiling R {r_version}")

  withr::with_dir(
    new = download_path,
    code = {
      # Configure
      cli::cli_alert_info("Running: ./configure...")
      result <- run_command(x = configure_cmd, key_root = NULL)

      if (result != 0) {
        cli::cli_alert_danger("Configure failed, trying with reduced features")
        configure_cmd <- glue::glue(
          "./configure --prefix={install_dir} \\
           --enable-R-shlib \\
           --with-readline=no"
        )
        result <- run_command(x = configure_cmd, key_root = NULL)
        if (result != 0) {
          stop(
            "Configure failed even with reduced features. Please install required dependencies."
          )
        }
      }

      # Make
      cli::cli_alert_info("Running: make...")
      run_command(x = "make -j$(nproc)", key_root = NULL)

      # Install
      cli::cli_alert_info("Running: make install...")
      run_command(
        x = glue::glue("make install"),
        key_root = key_root
      )
    }
  )

  cli::cli_h1("Creating Symbolic Links")

  # Create symlinks only if paths exist
  if (nzchar(dir_r)) {
    run_command(
      glue::glue("sudo ln -sf {install_dir}/bin/R {dir_r}"),
      key_root = key_root
    )
    cli::cli_alert_success("Symbolic link for R created at {dir_r}.")
  }

  if (nzchar(dir_rscript)) {
    run_command(
      glue::glue("sudo ln -sf {install_dir}/bin/Rscript {dir_rscript}"),
      key_root = key_root
    )
    cli::cli_alert_success(
      "Symbolic link for Rscript created at {dir_rscript}."
    )
  }

  cli::cli_alert_success(
    "R version {.strong {r_version}} successfully compiled and installed!"
  )
  cli::cli_alert_warning(
    "Changes will take effect after you restart your R session."
  )
}


#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom getPass getPass
#' @importFrom cli col_blue col_red symbol
#' @title Compile a version of R on GNU/Linux systems
#' @description This function is responsible for compiling a version of the R language.
#' @param x Version of R you want to compile. By default (\code{x = NULL}) the latest stable version of the R
#' language will be compiled. For example, \code{x = "4.5.0"} will compile and link \strong{R-4.5.0} as the major version on your system.
#' @details
#' This function is responsible for compiling a version of the [**R**](https://www.r-project.org/) language. The `x` argument is the version of [**R**](https://www.r-project.org/) that you want to compile.
#' For example, `x = "4.5.1"` will compile and link the **R-4.5.1** version as the major version on your system. By default (`x = NULL`), the latest stable version of [**R**](https://www.r-project.org/) will be compiled.
#'
#' For example, to compile the latest stable version of the [**R**](https://www.r-project.org/) language, do:
#'    ```
#'     rcompiler()
#'    ```
#' Regardless of your GNU/Linux distribution and what version of [**R**](https://www.r-project.org/) is in your repositories, you can have the latest stable version of the [**R**](https://www.r-project.org/) language compiled
#' into your computer's architecture.
#'
#' You can use the `rcompiler()` function to compile different versions of [**R**](https://www.r-project.org/). For example, running `rcompiler(x = "3.6.3")` and `rcompiler()` will install versions 3.6.3 and 4.0.0 on your GNU/Linux distribution,
#' respectively. If you are on version 4.0.0 of [**R**](https://www.r-project.org/) and run the code `rcompiler(x = "3.6.3")` again, the function will identify the existence of version 3.6.3 on the system and give you the option to use the binaries
#' that were built in a previous compilation. This avoids unnecessary compilations.
#' @seealso \code{\link{ropenblas}}, \code{\link{last_version_r}}
#' @return Returns a message informing you if the procedure occurred correctly. You will also be able to receive information about
#' missing dependencies.
#' @examples
#' # rcompiler()
#' @export
rcompiler <- function(x = NULL) {
  # --- Initial System Checks ---

  # 1. Ensures the script is running on a Linux system.
  if (Sys.info()[["sysname"]] != "Linux") {
    stop(
      glue::glue(
        "{cli::col_red(cli::symbol$cross)} Sorry, this function is designed to compile R on Linux systems."
      )
    )
  }

  # 2. Checks for an internet connection to download the source code.
  # (Note: `connection()` is a helper function you should have defined)
  if (!connection()) {
    stop(
      "Could not establish an internet connection. Check your network."
    )
  }

  # 3. Checks if essential compilation tools are installed.
  # (Note: `exist()` is a helper function you should have defined)
  if (!exist("gcc")) {
    stop(
      glue::glue(
        "{cli::col_red(cli::symbol$cross)} GNU GCC compiler is not installed. Please install it (e.g., 'build-essential' or 'gcc' packages)."
      )
    )
  }
  if (!exist("make")) {
    stop(
      glue::glue(
        "{cli::col_red(cli::symbol$cross)} GNU Make is not installed. Please install it (e.g., 'build-essential' or 'make' packages)."
      )
    )
  }

  # 4. Warns the user about compiling very old R versions.
  if (!is.null(x) && x < "3.1.0") {
    # (Note: `attention()` is a helper function you should have defined for user interaction)
    answer <- attention(x)
    # Checks if the answer was not affirmative.
    if (!answer %in% c("y", "yes")) {
      return(
        warning(
          glue::glue(
            "{cli::col_yellow(cli::symbol$warning)} Compilation cancelled by the user."
          )
        )
      )
    }
  }

  # --- Start of Compilation ---

  cli::cli_h1("System checks complete. Requesting permissions.")

  # Gets the superuser password for commands that require privileges.
  # (Note: `sudo_key()` is a helper function you should have defined)
  root <- sudo_key()

  # Calls the main compilation function with simplified parameters.
  compiler_r(
    r_version = x,
    key_root = root
  )
}

#' @importFrom magrittr "%>%"
#' @importFrom glue glue
#' @importFrom cli style_bold style_underline symbol col_green col_blue
#' @title Relink the OpenBLAS library with R
#' @description The \code{link_again} function relinks the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library with the R language, which is useful for correcting
#' unlinking problems that are common when the operating system is updated.
#' @param restart_r If \code{TRUE} (default), a new R session is started after linking the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library.
#' @details The \code{link_again} function can relink the R language with the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library. Thus, link_again will only perform the
#' relinking when the ropenblas function has been used in a previous R session for the initial binding of the R language with the
#' \href{https://www.openblas.net/}{\strong{OpenBLAS}} library.
#'
#' Relinking is useful in situations of operating system updates. In some updates, it is possible that the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library compiled
#' in the \code{/opt} directory gets unlinked. In this scenario, when the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library has already been compiled using the ropenblas
#' function, the \code{link_again} function performs a new link without the need to recompile, thus making the process less time
#' consuming.
#' @note In situations where there was a disconnection due to an operating system update, the `ropenblas` function can be used to
#' re-link the OpenBLAS library with the R language, however, it will be necessary to compile the
#' \href{https://www.openblas.net/}{\strong{OpenBLAS}} library again. If you are interested in recompiling the
#' \href{https://www.openblas.net/}{\strong{OpenBLAS}} library and linking with R, use the \code{\link{ropenblas}} function. If the
#' interest is to take advantage of a previous compilation of the \href{https://www.openblas.net/}{\strong{OpenBLAS}} library, the
#' \code{link_again} function may be useful.
#' @seealso \code{\link{ropenblas}}
#' @examples
#' # link_again()
#' @export
link_again <- function(restart_r = TRUE) {
  if (dir_blas()$use_openblas) {
    glue::glue(
      "{cli::col_blue(cli::symbol$bullet)} Relinking is not necessary. {cli::style_underline(cli::style_bold(\"R\"))} \\
      already uses the {cli::style_underline(cli::style_bold(\"OpenBLAS\"))} library. You can rest easy."
    )
  } else {
    if (!dir.exists("/opt/OpenBLAS/lib")) {
      glue::glue(
        "{cli::col_blue(cli::symbol$bullet)} Please run the {cli::style_underline(cli::style_bold(\"ropenblas()\"))} function first..."
      )
    } else {
      root <- sudo_key()
      glue::glue(
        "ln -snf /opt/OpenBLAS/lib/libopenblas.so {dir_blas()$path}{dir_blas()$file_blas}"
      ) %>%
        run_command(key_root = root)

      .refresh_terminal <- function() {
        system("R")
        q("no")
      }

      cat("\n")

      cat(cli::rule(
        width = 50L,
        center = glue::glue("{cli::style_bold(\"Procedure Completed\")}"),
        col = "blue",
        background_col = "gray90",
        line = 2L
      ))

      cat("\n")

      if (restart_r) {
        glue::glue(
          "[{cli::style_bold(cli::col_green(cli::symbol$tick))}] {cli::style_underline(cli::style_bold(\"OpenBLAS\"))}."
        ) %>%
          cat
      } else {
        glue::glue(
          "[{cli::style_bold(cli::col_green(cli::symbol$tick))}] {cli::style_underline(cli::style_bold(\"OpenBLAS\"))} will be used in the next session."
        ) %>%
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
#' @details The \href{https://cran.r-project.org/doc/manuals/r-release/NEWS.html}{NEWS.html} file contains the main changes from the recently released versions of the R language.
#' The goal is to facilitate the query by invoking it directly from the R command prompt. The \link{rnews} function is
#' analogous to the \link{news} function of the **utils** package. However, using the \link{news} command in a terminal style
#' bash shell, it is possible to receive a message like:
#' ```
#' news()
#' starting httpd help server ... done
#' Error in browseURL(url) : 'browser' must be a non-empty character string
#' ```
#' This is an error that may occur depending on the installation of R. Always prefer the use of the news
#' function, but if you need to, use the \link{rnews} function.
#' @export
rnews <- function(pdf = FALSE, dev = FALSE) {
  extension <-
    ifelse(pdf, "pdf", "html")

  r_v <-
    ifelse(dev, "r-devel", "r-release")

  glue::glue(
    "https://cran.r-project.org/doc/manuals/{r_v}/NEWS.{extension}"
  ) %>%
    fs::file_show()
}
