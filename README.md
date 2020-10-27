# ropenblas: Download, Compile and Link OpenBLAS Library with R

[![last](https://www.r-pkg.org/badges/last-release/ropenblas)](https://CRAN.R-project.org/package=ropenblas)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ropenblas)](https://CRAN.R-project.org/package=ropenblas)
[![total](http://cranlogs.r-pkg.org/badges/grand-total/ropenblas)](https://CRAN.R-project.org/package=ropenblas)
[![month](https://cranlogs.r-pkg.org/badges/ropenblas)](https://CRAN.R-project.org/package=ropenblas)

<img src="https://raw.githubusercontent.com/prdm0/ropenblas/master/logo.png" height="270" width="250" align="right" />

The [**ropenblas**](https://prdm0.github.io/ropenblas/) is a package designed to facilitate the linking of the library [**OpenBLAS**](https://www.openblas.net/) with the language [**R**](https://www.r-project.org/). The package, which works only for Linux systems, will automatically download the latest source code from the [**OpenBLAS**](https://www.openblas.net/) library and compile the code. The package will automatically bind the language [**R**](https://www.r-project.org/), through the `ropenblas()` function, to use the [**OpenBLAS**](https://www.openblas.net/) library. Everything will be done automatically regardless of the Linux distribution you are using.
  
You can also specify older versions of the [**OpenBLAS**](https://www.openblas.net/) library. Automatically, if no version is specified, the [**ropenblas**](https://prdm0.github.io/ropenblas/) package will consider the latest version of the library [**OpenBLAS**](https://www.openblas.net/).


Considering using the [**OpenBLAS**](https://www.openblas.net/) library rather than the [**BLAS**](http://www.netlib.org/blas/) may bring extra optimizations for your code and improved computational performance for your simulations, since [**OpenBLAS**](https://www.openblas.net/) is an optimized implementation of the library [**BLAS**](http://www.netlib.org/blas/).

Some of the reasons why it is convenient to link [**R**](https://www.r-project.org/) language to the use of [**BLAS**](http://www.netlib.org/blas/) optimized alternatives can be found [**here**](https://csantill.github.io/RPerformanceWBLAS/). Several other [**benchmarks**](https://en.wikipedia.org/wiki/Benchmarking) that point to improved computing performance by considering the library [**OpenBLAS**](https://www.openblas.net/) can be found on the internet.

The ropenblas package, by `rcompiler()` function is also useful if you want to install different versions of the [**R**](https://www.r-project.org/) language. The different versions, specified by the user of the [**R**](https://www.r-project.org/) language, will be compiled and will also be linked to the [**OpenBLAS**](https://www.openblas.net/) library. If you want to switch between compiled versions of the [**R**](https://www.r-project.org/) language, no compilation is needed anymore. This allows you to avoid having to get your hands dirty with tedious operating system settings, regardless of your GNU/Linux distribution. Another great use of the `rcompiler()` function is that you will not be dependent on updating your GNU/Linux distribution repositories and you can always have the latest version of the [**R**](https://www.r-project.org/) language.

The use of the ropenblas package will return warnings that help you proceed with the use of the functions. If your internet is not working or if any dependency on the operating system is not present, the package will let you know. 

## Dependencies

You must install the following dependencies on your operating system (Linux):

   1 - **GNU Make**: GNU Make utility to maintain groups of programs; <br/>
   
   2 - **GNU GCC Compiler (C and Fortran)**: The GNU Compiler Collection - C and Fortran frontends. 

Do not worry that you will be notified if any of these dependencies are not installed.
   
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

## Using the package

The [**ropenblas**](https://prdm0.github.io/ropenblas/) package currently provides five functions: `ropenblas()`, `rcompiler()`, `last_version_openblas()`, `last_version_r()` and `link_again()`. First of all, do:

```
library(ropenblas)
```

### 'ropenblas' function:

Installing, compiling, and linking the [**OpenBLAS**](https://www.openblas.net/) version **0.3.12** library to the [**R**](https://www.r-project.org/) language:

```
ropenblas(x = "0.3.12")
```

**Notes**: 

   - You do not have to in every section of [**R**](https://www.r-project.org/) make use of the `ropenblas()` function. Once the function is used, [**R**](https://www.r-project.org/) will always consider using the [**OpenBLAS**](https://www.openblas.net/) library in future sections.

   - [**OpenBLAS**](https://www.openblas.net/) versions tested: 0.3.0, 0.3.1, 0.3.2, 0.3.3, 0.3.4, 0.3.5, 0.3.6, 0.3.7, 0.3.8, 0.3.9, 0.3.10, 0.3.11 and 0.3.12. These are the values that will be passed to `x` in `ropenblas(x)`; 
   
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

You will not necessarily have to run `ropenblas()` on every section of [**R**](https://www.r-project.org/). Almost always it will not be necessary. However, it may be that the [**R**](https://www.r-project.org/) is updated by the operating system (GNU/Linux). Thus, it may be that in this update the [**R**](https://www.r-project.org/) unlink with the [**OpenBLAS**](https://www.openblas.net/) library. Therefore, from time to time check using the command `extSoftVersion()["BLAS"]` if the link with [**OpenBLAS**](https://www.openblas.net/) is correct, otherwise run the command `ropenblas()` again.

### 'last_version_r' function:

Given the higher version, the function will return the latest stable version of the [**R**](https://www.r-project.org/) language. See the following example:

```
> last_version_r()
$last_version
[1] "4.0.3"

$versions
[1] "4.0.0" "4.0.1" "4.0.2" "4.0.3"

$n
[1] 4
```

or

```
> last_version_r(major = 3L)
$last_version
[1] "3.6.3"

$versions
 [1] "3.0.0"         "3.0.1"         "3.0.2"         "3.0.3"         "3.1.0"         "3.1.1"         "3.1.2"         "3.1.3"        
 [9] "3.2.0"         "3.2.1"         "3.2.2"         "3.2.3"         "3.2.4-revised" "3.2.4"         "3.2.5"         "3.3.0"        
[17] "3.3.1"         "3.3.2"         "3.3.3"         "3.4.0"         "3.4.1"         "3.4.2"         "3.4.3"         "3.4.4"        
[25] "3.5.0"         "3.5.1"         "3.5.2"         "3.5.3"         "3.6.0"         "3.6.1"         "3.6.2"         "3.6.3"        

$n
[1] 32
```

**Note**: If `major = NULL`, the function will consider the major release number.

### 'last_version_openblas' function:

The `last_version_openblas()` function automatically searches [**OpenBLAS**](https://www.openblas.net/) library versions in the official [**GitHub**](https://github.com/xianyi/OpenBLAS) project.

```
> last_version_openblas()
$last_version
[1] "0.3.12"

$versions
 [1] "0.1alpha1"   "0.1alpha2"   "0.1alpha2.1" "0.1alpha2.2" "0.1alpha2.3" "0.1alpha2.4" "0.1alpha2.5" "0.1.0"      
 [9] "0.1.1"       "0.2.0"       "0.2.1"       "0.2.2"       "0.2.3"       "0.2.4"       "0.2.5"       "0.2.6"      
[17] "0.2.7"       "0.2.8"       "0.2.9"       "0.2.9.rc1"   "0.2.9.rc2"   "0.2.10"      "0.2.10.rc1"  "0.2.10.rc2" 
[25] "0.2.11"      "0.2.12"      "0.2.13"      "0.2.14"      "0.2.15"      "0.2.16"      "0.2.16.rc1"  "0.2.17"     
[33] "0.2.18"      "0.2.19"      "0.2.20"      "0.3.0"       "0.3.1"       "0.3.2"       "0.3.3"       "0.3.4"      
[41] "0.3.5"       "0.3.6"       "0.3.7"       "0.3.8"       "0.3.9"       "0.3.10"      "0.3.11"      "0.3.12"     

$n
[1] 48
```

### 'rcompiler' function:

This function is responsible for compiling a version of the [**R**](https://www.r-project.org/) language. The `x` argument is the version of [**R**](https://www.r-project.org/) that you want to compile. For example, `x = "4.0.0"` will compile and link **R-4.0.0** version as the major version on your system. By default (`x = NULL`) will be compiled the latest stable version of the [**R**](https://www.r-project.org/).

For example, to compile the latest stable version of the [**R**](https://www.r-project.org/) language, do:

```
rcompiler()
```

Regardless of your GNU/Linux distribution and what version of [**R**](https://www.r-project.org/) is in your repositories, you can have the latest stable version of the [**R**](https://www.r-project.org/) language compiled into your computer architecture.

You can use the `rcompiler()` function to compile different versions of [**R**](https://www.r-project.org/). For example, running `rcompiler(x = "3.6.3")` and `rcompiler()` will install versions 3.6.3 and 4.0.0 on its GNU/Linux distribution, respectively. If you are in version 4.0.0 of [**R**](https://www.r-project.org/) and run the code `rcompiler(x = "3.6.3")` again, the function will identify the existence of version 3.6.3 in the system and give you the option to use the binaries that were built in a previous compilation. This avoids unnecessarys compilations.


In addition to the `x` argument, the` rcompiler()` function has two other arguments that will allow you to change and pass new compilation flags. Are they:

1. `with_blas`: This argument sets the `--with-blas` flag in the R language compilation process and must be passed as a string. Details on the use of this flag can be found [**here**](https://cran.r-project.org/doc/manuals/r-devel/R-admin.html). If `with_blas = NULL` (default), then it will be considered:

   ```
   ./configure --prefix=/opt/R/version_r --enable-memory-profiling --enable-R-shlib --enable-threads=posix
   --with-blas="-L/opt/OpenBLAS/lib -I/opt/OpenBLAS/include -lpthread -lm"
   ```

   Most likely, you will have little reason to change this argument. Unless you know what you're doing, consider `with_blas = NULL`. Do not change the installation directory, that is, always consider `--prefix = /opt/R/version_r`, where` version_r` is a valid version of [**R**](https://www.r-project.org/). For a list of valid versions of [**R**](https://www.r-project.org/), run the `last_version_r()`.
Installing [**R**](https://www.r-project.org/) in the `/opt/R/version_r` directory is important because some functions in the package require this. Both the [**R**](https://www.r-project.org/) language and the [**OpenBLAS**](https://www.openblas.net/) library will be installed in the `/opt` directory. If this directory does not exist in your GNU/Linux distribution, it will be created;
   
2. `complementary_flags`: String (`complementary_flags = NULL` by default) for adding complementary flags in the [**R**](https://www.r-project.org/) language compilation process. Passing a string to `complementary_flags` will compile it in the form:

   ```
    ./configure --with-blas="..." complementary_flags
   ```


### 'link_again' function:

The `link_again` function links again the [**OpenBLAS**](https://www.openblas.net/) library with the [**R**](https://www.r-project.org/) language, being useful to correct problems of untying the [**OpenBLAS**](https://www.openblas.net/) library that is common when the operating system is updated.

The function `link_again` be able to link again the [**R**](https://www.r-project.org/) language with the [**OpenBLAS**](https://www.openblas.net/) library. Thus, link_again will only make the relinkagem when in some previous section of
[**R**](https://www.r-project.org/) the ropenblas function has been used for the initial binding of the [**R**](https://www.r-project.org/) language with the [**OpenBLAS**](https://www.openblas.net/) library.

For example, to relink the [**OpenBLAS**](https://www.openblas.net/) library with the [**R**](https://www.r-project.org/) language, do:

```
link_again()
```

If `restart_r = TRUE` (default), a new section of [**R**](https://www.r-project.org/) is started after linking the [**OpenBLAS**](https://www.openblas.net/) library.


In situations where there was a disconnection due to an update of the operating system, the `ropenblas` function can be used to
relink the [**OpenBLAS**](https://www.openblas.net/) library with the [**R**](https://www.r-project.org/) language, however, it will be necessary to compile the [**OpenBLAS**](https://www.openblas.net/) library again. If you are interested in recompiling the
[**OpenBLAS**](https://www.openblas.net/) library and linking with [**R**](https://www.r-project.org/), use the `ropenblas` function. If the interest is to take advantage of a previous compilation of the [**OpenBLAS**](https://www.openblas.net/) library, the
function `link_again` may be useful.


### 'rnews' function:

Returns the contents of the [**NEWS.html**](https://cran.r-project.org/doc/manuals/r-release/NEWS.html) file in the standard browser installed on the operating system. The [**NEWS.html**](https://cran.r-project.org/doc/manuals/r-release/NEWS.html) file contains the main changes from the recently released versions of the [**R**](https://www.r-project.org/) language. The goal is to facilitate the query by invoking it directly from the [**R**](https://www.r-project.org/) command prompt. The rnews function is analogous to the news function of the utils package. However, using the news command in a terminal style bash shell is possible to receive a message like:

```
> news()
starting httpd help server ... done
Error in browseURL(url) : 'browser' must be a non-empty character string
```

If `pdf = FALSE` (default), the [**NEWS.html**](https://cran.r-project.org/doc/manuals/r-release/NEWS.html) file will open in the browser, otherwise [**NEWS.pdf**](https://cran.r-project.org/doc/manuals/r-release/NEWS.pdf) will be opened. If `dev = FALSE` (default), it will not show changes made to the language development version. To see changes in the development version, do `dev = TRUE`.

## Advantages of using ropenblas package:

Some advantages of using the [**ropenblas**](https://prdm0.github.io/ropenblas/) library:

   - Everything is done within the [**R**](https://www.r-project.org/) language;

   - The procedure will be the same for any Linux distribution;

   - The [**OpenBLAS**](https://www.openblas.net/) library will be compiled and you will choose which build version to bind to [**R**](https://www.r-project.org/), regardless of your Linux distribution;

   - If your GNU/Linux distribution does not have updated versions of [**OpenBLAS**](https://www.openblas.net/), it matters little. The ropenblas package fetches the latest stable release of the [**OpenBLAS**](https://www.openblas.net/) library development account on GitHub;

   - You do not need to know Linux well. In some distributions, it may not be so simple for a less experienced user to compile and link the library to the [**OpenBLAS**](https://www.openblas.net/) library with the [**R**](https://www.r-project.org/) language;

   - It is much easier to direct a person to link [**OpenBLAS**](https://www.openblas.net/) with [**R**](https://www.r-project.org/) saying "run `ropenblas()` within [**R**](https://www.r-project.org/)" than asking that person to verify that an unoptimized version of [**BLAS**](http://www.netlib.org/blas/) installed on the system. Then you have to guide the removal of the unoptimized version of [**BLAS**](http://www.netlib.org/blas/) and guide it to the installation of the library [**OpenBLAS**](https://www.openblas.net/) through the most diverse procedures depending on the GNU/Linux distribution used;

   - As stated earlier, the procedure works for any Linux and this includes Android. If your Android is capable of running privileged commands (ROOT) and if you have [**R**](https://www.r-project.org/) installed via Termux with the required dependencies, you can compile and link [**OpenBLAS**](https://www.openblas.net/) with [**R**](https://www.r-project.org/) using ropenblas;
   
   - With the `rcompiler()` function you can build any version of [**R**](https://www.r-project.org/) into your computer architecture, which includes the most stable version of the language;
   
   - What is the latest stable version of [**R**](https://www.r-project.org/)? Are you too lazy to go to the [**R**](https://www.r-project.org/) site? Run `ropenblas::last_version_r(major = NULL)`.
