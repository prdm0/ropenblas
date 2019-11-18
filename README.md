# ropenblas package

**ropenblas** is a package designed to facilitate the linking of the library [**OpenBLAS**](https://www.openblas.net/) with the language [**R**](https://www.r-project.org/). The package, which works only for Linux / * Nix systems, will automatically download the latest source code from the [**OpenBLAS**](https://www.openblas.net/) library and compile the code. The package will automatically bind the language [**R**](https://www.r-project.org/) to use the [**OpenBLAS**](https://www.openblas.net/) library. Everything will be done automatically regardless of the Linux distribution you are using.

You can also specify older versions of the [**OpenBLAS**](https://www.openblas.net/) library. Automatically, if no version is specified, the **ropenblas** package will consider the latest version of the library [**OpenBLAS**](https://www.openblas.net/).


Considering using the [**OpenBLAS**](https://www.openblas.net/) library rather than the [**BLAS**](http://www.netlib.org/blas/) may bring extra optimizations for your code and improved computational performance for your simulations, since [**OpenBLAS**](https://www.openblas.net/) is an optimized implementation of the library [**BLAS**](http://www.netlib.org/blas/).

Some of the reasons why it is convenient to link R language to the use of [**BLAS**](http://www.netlib.org/blas/) optimized alternatives can be found [**here**](https://csantill.github.io/RPerformanceWBLAS/). Several other [** benchmarks**](https://en.wikipedia.org/wiki/Benchmarking) that point to improved computing performance by considering the library [**OpenBLAS**](https://www.openblas.net/) can be found on the internet.

## Installation

Installing the **ropenblas** library is easy and will require you to have installed the **devtools** package. This will allow you to install the **ropenblas** package directly from GitHub. To install, after installing the **devtools** package, do:

```
devtools::install_github(repo = "prdm0/ropenblas")
```

## Use

Installing, compiling, and linking the [**OpenBLAS**] (https://www.openblas.net/) version **0.3.7** library to the [**R**](https://www.r-project.org/) language:

```
library(ropenblas)
ropenblas(x = "0.3.7")
```