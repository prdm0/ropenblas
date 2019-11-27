# ropenblas package

<img src="https://raw.githubusercontent.com/prdm0/ropenblas/master/logo.png" height="270" width="250" align="right" />

The [**ropenblas**](https://prdm0.github.io/ropenblas/) is a package designed to facilitate the linking of the library [**OpenBLAS**](https://www.openblas.net/) with the language [**R**](https://www.r-project.org/). The package, which works only for Linux systems, will automatically download the latest source code from the [**OpenBLAS**](https://www.openblas.net/) library and compile the code. The package will automatically bind the language [**R**](https://www.r-project.org/) to use the [**OpenBLAS**](https://www.openblas.net/) library. Everything will be done automatically regardless of the Linux distribution you are using.
  
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

The `ropenblas()` function will download the desired version of the library [**OpenBLAS**](https://www.openblas.net/), compile and install the library in the `/opt` directory of your operational system. If the directory does not exist, it will be created so that the installation can be completed. Subsequently, files from the version of [**BLAS**](http://www.netlib.org/blas/) used in [**R**](https://www.r-project.org/) will be symbolically linked to the shared object files of the library version [**OpenBLAS**](https://www.openblas.net/) compiled and installed in `/opt`.

You must be the operating system administrator to use this library. Therefore, do not attempt to use it without telling your system administrator. If you have the ROOT password, you will be responsible for everything you do on your operating system.

## Advantages

Some advantages of using the [**ropenblas**](https://prdm0.github.io/ropenblas/) library:

   - Everything is done within the [**R**](https://www.r-project.org/) language;

   - The procedure will be the same for any Linux distribution;

   - The [**OpenBLAS**](https://www.openblas.net/) library will be compiled and you will choose which build version to bind to [**R**](https://www.r-project.org/), regardless of your Linux distribution;

   - If your GNU/Linux distribution does not have updated versions of [**OpenBLAS**](https://www.openblas.net/), it matters little. The ropenblas package fetches the latest stable release of the [**OpenBLAS**](https://www.openblas.net/) library development account on GitHub;

   - You do not need to know Linux well. In some distributions, it may not be so simple for a less experienced user to compile and link the library to the [**OpenBLAS**](https://www.openblas.net/) library with the [**R**](https://www.r-project.org/) language;

   - It is much easier to direct a person to link [**OpenBLAS**](https://www.openblas.net/) with [**R**](https://www.r-project.org/) saying "run `ropenblas()` within [**R**](https://www.r-project.org/)" than asking that person to verify that an unoptimized version of [**BLAS**](http://www.netlib.org/blas/) installed on the system. Then you have to guide the removal of the unoptimized version of [**BLAS**](http://www.netlib.org/blas/) and guide it to the installation of the library [**OpenBLAS**](https://www.openblas.net/) through the most diverse procedures depending on the GNU/Linux distribution used;

   - As stated earlier, the procedure works for any Linux and this includes Android. If your Android is capable of running privileged commands (ROOT) and if you have [**R**](https://www.r-project.org/) installed via Termux with the required dependencies, you can compile and link [**OpenBLAS**](https://www.openblas.net/) with [**R**](https://www.r-project.org/) using ropenblas.