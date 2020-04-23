# ropenblas 0.2.5 (development version)

* Code review;

* Identifying bugs.

# ropenblas 0.2.4

* Code review;

* Identifying bugs;

* Implementing the `link_again()` function. This function will allow you to link the R language again with the OpenBLAS library without having to recompile the OpenBLAS library. The `link_again()` function will be useful in situations where the operating system decouples the library from the OpenBLAS library. This is common in situations where the BLAS library binary file is updated.

# ropenblas 0.2.3

* Code review;

* Improve code efficiency.

# ropenblas 0.2.2 

* The `last_version_openblas()` function will be added.

# ropenblas 0.2.1

* Code review;

* Highlighting some messages;

* In the tests performed, apparently an error will not break the installed version of R.


# ropenblas 0.2.0

* Function `last_version_r(major = 3L)` implemented.  Given the higher version, the function will return the latest stable version of the R language. Major release number of R language (eg. `1L`, `2L`, `3L`, ...);

* The `rcompiler()` function has been exported. This function is responsible for compiling a version of the R language;

* It now depends on the version of R >= 3.1.0;

* Compiled versions of R will be kept. When attempting to compile a previously compiled version, you will be given the option to only switch between versions of R without compiling;

* An alert will be issued when attempting to compile and link a version of R earlier than version 3.6.1. The user will have to respond three times that they are aware that they will not be able to use the ropenblas package to return to a newer version of R.

# ropenblas 0.1.0

* Maintenance release;

* General documentation improvements;

* Check if there is no internet connection;

* The `ropenblas()` function documentation describes the details of changes that are made to the system;

* The `ropenblas()` function is capable of suggesting a stable and newer version of the OpenBLAS library. The user can decide whether to use the latest version or the one of his choice. By default if no argument is passed to the `ropenblas()` function, the latest version of the OpenBLAS library will be considered.