# ropenblas package

<img src="https://raw.githubusercontent.com/prdm0/ropenblas/master/logo.png" height="270" width="250" align="right" />

[**ropenblas**](https://prdm0.github.io/ropenblas/) is a package designed to facilitate the linking of the library [**OpenBLAS**](https://www.openblas.net/) with the language [**R**](https://www.r-project.org/). The package, which works only for Linux systems, will automatically download the latest source code from the [**OpenBLAS**](https://www.openblas.net/) library and compile the code. The package will automatically bind the language [**R**](https://www.r-project.org/) to use the [**OpenBLAS**](https://www.openblas.net/) library. Everything will be done automatically regardless of the Linux distribution you are using.
  
You can also specify older versions of the [**OpenBLAS**](https://www.openblas.net/) library. Automatically, if no version is specified, the [**ropenblas**](https://prdm0.github.io/ropenblas/) package will consider the latest version of the library [**OpenBLAS**](https://www.openblas.net/).


Considering using the [**OpenBLAS**](https://www.openblas.net/) library rather than the [**BLAS**](http://www.netlib.org/blas/) may bring extra optimizations for your code and improved computational performance for your simulations, since [**OpenBLAS**](https://www.openblas.net/) is an optimized implementation of the library [**BLAS**](http://www.netlib.org/blas/).

Some of the reasons why it is convenient to link [**R**](https://www.r-project.org/) language to the use of [**BLAS**](http://www.netlib.org/blas/) optimized alternatives can be found [**here**](https://csantill.github.io/RPerformanceWBLAS/). Several other [**benchmarks**](https://en.wikipedia.org/wiki/Benchmarking) that point to improved computing performance by considering the library [**OpenBLAS**](https://www.openblas.net/) can be found on the internet.

## Dependencies

You must install the following dependencies on your operating system (Linux):

   1 - **make**: GNU make utility to maintain groups of programs; <br/>
   
   2 - **gcc**: The GNU Compiler Collection - C and C++ frontends; <br/>
   
   3 - **gcc-fortran**: The GNU Compiler Collection - Fortran frontends.

## Installation

Installing the [**ropenblas**](https://prdm0.github.io/ropenblas/) library is easy and will require you to have installed the **devtools** package. This will allow you to install the [**ropenblas**](https://prdm0.github.io/ropenblas/) package directly from GitHub. To install, after installing the **devtools** package, do:

```
devtools::install_github(repo = "prdm0/ropenblas", force = TRUE)
```

## Use

Installing, compiling, and linking the [**OpenBLAS**](https://www.openblas.net/) version **0.3.7** library to the [**R**](https://www.r-project.org/) language:

```
library(ropenblas)
ropenblas(x = "0.3.7")
```

**Notes**: 

   - You do not have to in every section of [**R**](https://www.r-project.org/) make use of the `ropenblas()` function. Once the function is used, [**R**](https://www.r-project.org/) will always consider using the [**OpenBLAS**](https://www.openblas.net/) library in future sections.

   - [**OpenBLAS**](https://www.openblas.net/) versions tested: 0.3.0, 0.3.1, 0.3.2, 0.3.3, 0.3.4, 0.3.5, 0.3.6 and 0.3.7. These are the values that will be passed to `x` in `ropenblas(x)`. 
  
## Details

Your linux operating system may already be configured to use the [**OpenBLAS**](https://www.openblas.net/) library. Therefore, most likely [**R**](https://www.r-project.org/) will already be linked to this library. To find out if the [**R**](https://www.r-project.org/) language is using the [**OpenBLAS**](https://www.openblas.net/) library, at [**R**](https://www.r-project.org/), do:

```
extSoftVersion()["BLAS"]
```

If [**R**](https://www.r-project.org/) is using the [**OpenBLAS**](https://www.openblas.net/) library, something like `/any_directory/libopenblas.so` should be returned. Therefore, there should be the name **openblas** in the **s**hared **o**bject returned (file extension **.so**).

If the `ropenblas()` function can identify that the [**R**](https://www.r-project.org/) language is using the version of [**OpenBLAS**](https://www.openblas.net/) you wish to configure, a warning message will be returned asking if you really would like to proceed with the configuration again.

## Advantages

Some advantages of using the [**ropenblas**](https://prdm0.github.io/ropenblas/) library:

   - Everything is done within the [**R**](https://www.r-project.org/) language;
   
   - The procedure will be the same for any linux distribution;
   
   - [**OpenBLAS**](https://www.openblas.net/) will be compiled and you choose which build version to link to [**R**](https://www.r-project.org/);
   
   - You will not need to know linux well. In some distributions it may not be as simple for a less experienced user to compile the [**OpenBLAS**](https://www.openblas.net/) library and link with [**R**](https://www.r-project.org/).