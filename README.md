# ropenblas: Download, Compile and Link OpenBLAS Library with R

<img src="https://raw.githubusercontent.com/prdm0/ropenblas/master/logo.png" height="270" width="250" align="right" />

The [**ropenblas**](https://prdm0.github.io/ropenblas/) is a package designed to facilitate the linking of the library [**OpenBLAS**](https://www.openblas.net/) with the language [**R**](https://www.r-project.org/). The package, which works only for Linux systems, will automatically download the latest source code from the [**OpenBLAS**](https://www.openblas.net/) library and compile the code. The package will automatically bind the language [**R**](https://www.r-project.org/) to use the [**OpenBLAS**](https://www.openblas.net/) library. Everything will be done automatically regardless of the Linux distribution you are using.
  
You can also specify older versions of the [**OpenBLAS**](https://www.openblas.net/) library. Automatically, if no version is specified, the [**ropenblas**](https://prdm0.github.io/ropenblas/) package will consider the latest version of the library [**OpenBLAS**](https://www.openblas.net/).


Considering using the [**OpenBLAS**](https://www.openblas.net/) library rather than the [**BLAS**](http://www.netlib.org/blas/) may bring extra optimizations for your code and improved computational performance for your simulations, since [**OpenBLAS**](https://www.openblas.net/) is an optimized implementation of the library [**BLAS**](http://www.netlib.org/blas/).

Some of the reasons why it is convenient to link [**R**](https://www.r-project.org/) language to the use of [**BLAS**](http://www.netlib.org/blas/) optimized alternatives can be found [**here**](https://csantill.github.io/RPerformanceWBLAS/). Several other [**benchmarks**](https://en.wikipedia.org/wiki/Benchmarking) that point to improved computing performance by considering the library [**OpenBLAS**](https://www.openblas.net/) can be found on the internet.

## Dependencies

You must install the following dependencies on your operating system (Linux):

   1 - **GNU Make**: GNU Make utility to maintain groups of programs; <br/>
   
   2 - **GNU GCC Compiler (C/C++ and Fortran)**: The GNU Compiler Collection - C/C++ and Fortran frontends. 
   
## Installation

Installing the [**ropenblas**](https://prdm0.github.io/ropenblas/) library is easy and will require you to have installed the **devtools** package. This will allow you to install the [**ropenblas**](https://prdm0.github.io/ropenblas/) package directly from GitHub. To install, after installing the **devtools** package, do:

```
devtools::install_github(repo = "prdm0/ropenblas", force = TRUE)
```

or

```
install.packages("ropenblas")
```

**Note**: If you want to access the latest features of the [**ropenblas**](https://prdm0.github.io/ropenblas/) package, install it using the first procedure.

## Use of Functions

O pacote [**ropenblas**](https://prdm0.github.io/ropenblas/) disponibiliza atualmente três funções: `ropenblas()`, `last_version_r()` e `rcompiler()`. First of all, do:

```
library(ropenblas)
```

### 'ropenblas' function:

Installing, compiling, and linking the [**OpenBLAS**](https://www.openblas.net/) version **0.3.7** library to the [**R**](https://www.r-project.org/) language:

```
ropenblas(x = "0.3.7")
```

**Notes**: 

   - You do not have to in every section of [**R**](https://www.r-project.org/) make use of the `ropenblas()` function. Once the function is used, [**R**](https://www.r-project.org/) will always consider using the [**OpenBLAS**](https://www.openblas.net/) library in future sections.

   - [**OpenBLAS**](https://www.openblas.net/) versions tested: 0.3.0, 0.3.1, 0.3.2, 0.3.3, 0.3.4, 0.3.5, 0.3.6 and 0.3.7. These are the values that will be passed to `x` in `ropenblas(x)`; 
   
   - If `x = NULL`, the latest stable version of the [**OpenBLAS**](https://www.openblas.net/) library will be compiled and linked to [**R**](https://www.r-project.org/).
  
#### Details

Your linux operating system may already be configured to use the [**OpenBLAS**](https://www.openblas.net/) library. Therefore, most likely [**R**](https://www.r-project.org/) will already be linked to this library. To find out if the [**R**](https://www.r-project.org/) language is using the [**OpenBLAS**](https://www.openblas.net/) library, at [**R**](https://www.r-project.org/), do:

```
extSoftVersion()["BLAS"]
```

If [**R**](https://www.r-project.org/) is using the [**OpenBLAS**](https://www.openblas.net/) library, something like `/any_directory/libopenblas.so` should be returned. Therefore, there should be the name **openblas** in the **s**hared **o**bject returned (file extension **.so**).

If the `ropenblas()` function can identify that the [**R**](https://www.r-project.org/) language is using the version of [**OpenBLAS**](https://www.openblas.net/) you wish to configure, a warning message will be returned asking if you really would like to proceed with the configuration again.

The `ropenblas()` function will download the desired version of the library [**OpenBLAS**](https://www.openblas.net/), compile and install the library in the `/opt` directory of your operational system. If the directory does not exist, it will be created so that the installation can be completed. Subsequently, files from the version of [**BLAS**](http://www.netlib.org/blas/) used in [**R**](https://www.r-project.org/) will be symbolically linked to the shared object files of the library version [**OpenBLAS**](https://www.openblas.net/) compiled and installed in `/opt`.

You must be the operating system administrator to use this library. Therefore, do not attempt to use it without telling your system administrator. If you have the ROOT password, you will be responsible for everything you do on your operating system.

### 'last_version_r' function:

Given the higher version, the function will return the latest stable version of the [**R**](https://www.r-project.org/) language. Veja o exemplo:

```
> last_version_r(major = 3L)
$last_version
[1] "R-3.6.1"

$versions
 [1] "R-3.0.0" "R-3.0.1" "R-3.0.2" "R-3.0.3" "R-3.1.0" "R-3.1.1" "R-3.1.2" "R-3.1.3" "R-3.2.0" "R-3.2.1" "R-3.2.2" "R-3.2.3"
[13] "R-3.2.4" "R-3.2.5" "R-3.3.0" "R-3.3.1" "R-3.3.2" "R-3.3.3" "R-3.4.0" "R-3.4.1" "R-3.4.2" "R-3.4.3" "R-3.4.4" "R-3.5.0"
[25] "R-3.5.1" "R-3.5.2" "R-3.5.3" "R-3.6.0" "R-3.6.1"
```

or

```
> last_version_r(major = 2L)
$last_version
[1] "R-2.9.2"

$versions
 [1] "R-2.0.0"  "R-2.0.1"  "R-2.1.0"  "R-2.1.1"  "R-2.10.0" "R-2.10.1" "R-2.11.0" "R-2.11.1" "R-2.12.0" "R-2.12.1" "R-2.12.2"
[12] "R-2.13.0" "R-2.13.1" "R-2.13.2" "R-2.14.0" "R-2.14.1" "R-2.14.2" "R-2.15.0" "R-2.15.1" "R-2.15.2" "R-2.15.3" "R-2.2.0" 
[23] "R-2.2.1"  "R-2.3.0"  "R-2.3.1"  "R-2.4.0"  "R-2.4.1"  "R-2.5.0"  "R-2.5.1"  "R-2.6.0"  "R-2.6.1"  "R-2.6.2"  "R-2.7.0" 
[34] "R-2.7.1"  "R-2.7.2"  "R-2.8.0"  "R-2.8.1"  "R-2.9.0"  "R-2.9.1"  "R-2.9.2" 
```

**Note**: If `major = NULL`, the function will consider the major release number.

### 'rcompiler' function:

This function is responsible for compiling a version of the [**R**](https://www.r-project.org/) language. The `x` argument is the version of [**R**](https://www.r-project.org/) that you want to compile. For example, `x = "3.6.1"` will compile and link **R-3.6.1** version  as the major version on your system. By default (`x = NULL`) will be compiled the latest stable version of the [**R**](https://www.r-project.org/)

The `version_openblas` [**OpenBLAS**](https://www.openblas.net/) argument is a version of the library that will be linked to the [**R**](https://www.r-project.org/) code that will be compiled. By default if `version_openblas = NULL`, the latest stable version of the library [**OpenBLAS**](https://www.openblas.net/) will be linked.

For example, to compile the latest stable version of the [**R**](https://www.r-project.org/) language, do:

```
rcompiler()
```

Regardless of your GNU/Linux distribution and what version of [**R**](https://www.r-project.org/) is in your repositories, you can have the latest stable version of the [**R**](https://www.r-project.org/) language compiled into your computer architecture.

## Advantages of using ropenblas package:

Some advantages of using the [**ropenblas**](https://prdm0.github.io/ropenblas/) library:

   - Everything is done within the [**R**](https://www.r-project.org/) language;

   - The procedure will be the same for any Linux distribution;

   - The [**OpenBLAS**](https://www.openblas.net/) library will be compiled and you will choose which build version to bind to [**R**](https://www.r-project.org/), regardless of your Linux distribution;

   - If your GNU/Linux distribution does not have updated versions of [**OpenBLAS**](https://www.openblas.net/), it matters little. The ropenblas package fetches the latest stable release of the [**OpenBLAS**](https://www.openblas.net/) library development account on GitHub;

   - You do not need to know Linux well. In some distributions, it may not be so simple for a less experienced user to compile and link the library to the [**OpenBLAS**](https://www.openblas.net/) library with the [**R**](https://www.r-project.org/) language;

   - It is much easier to direct a person to link [**OpenBLAS**](https://www.openblas.net/) with [**R**](https://www.r-project.org/) saying "run `ropenblas()` within [**R**](https://www.r-project.org/)" than asking that person to verify that an unoptimized version of [**BLAS**](http://www.netlib.org/blas/) installed on the system. Then you have to guide the removal of the unoptimized version of [**BLAS**](http://www.netlib.org/blas/) and guide it to the installation of the library [**OpenBLAS**](https://www.openblas.net/) through the most diverse procedures depending on the GNU/Linux distribution used;

   - As stated earlier, the procedure works for any Linux and this includes Android. If your Android is capable of running privileged commands (ROOT) and if you have [**R**](https://www.r-project.org/) installed via Termux with the required dependencies, you can compile and link [**OpenBLAS**](https://www.openblas.net/) with [**R**](https://www.r-project.org/) using ropenblas.
   
   - With the `rcompiler()` function you can build any version of [**R**](https://www.r-project.org/) into your computer architecture, which includes the most stable version of the language.