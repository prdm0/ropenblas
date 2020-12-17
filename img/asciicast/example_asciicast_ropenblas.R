#' Title: Using the ropenblas() function
#' Rows: 14

# Installing, compiling, and linking the OpenBLAS version 0.3.13
# library to the R language.

# Run the command below eliminating the comment. This process may 
# take a few minutes. You must enter the ROOT password when 
# requested.

# ropenblas::ropenblas(x = "0.3.13")

# Verifying that the OpenBLAS library is linked
extSoftVersion()["BLAS"]