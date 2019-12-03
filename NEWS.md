# Development version (version 0.1.1)

* Function `last_version_r(major = 3L)` implemented.  Given the higher version, the function will return the latest stable version of the R language. Major release number of R language (eg. `1L`, `2L`, `3L`, ...);

* The `rcompiler()` function has been exported. This function is responsible for compiling a version of the R language.

# ropenblas 0.1.0

* Maintenance release;

* General documentation improvements;

* Check if there is no internet connection;

* The `ropenblas()` function documentation describes the details of changes that are made to the system;

* The `ropenblas()` function is capable of suggesting a stable and newer version of the OpenBLAS library. The user can decide whether to use the latest version or the one of his choice. By default if no argument is passed to the `ropenblas()` function, the latest version of the OpenBLAS library will be considered.


 