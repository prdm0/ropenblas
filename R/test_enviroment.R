ropenblas <- function() {
  
   if (identical(caller_env(), global_env())){
     senha <- sudo_key()
   } else {
     senha <- get(x = "root", envir = e)
   }
   senha
}

rcompiler <- function() {
  root <- "asdf1234"
  e <- env(empty_env(), f = ropenblas, root = root)
  exec(.fn = "f", .env = e) 
}