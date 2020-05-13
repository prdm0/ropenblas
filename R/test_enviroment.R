ropenblas <- function() {
   if (identical(caller_env(), global_env())) {
      senha <- sudo_key()
   } else {
      senha <- get(x = "root", envir = e)
      rm(e, envir = global_env())
   }
   senha
}

rcompiler <- function() {
   root <- "asdf1234"
   e <<- env(empty_env(), f = ropenblas, root = root)
   ropenblas()
}