#' Title: Using the last_version_r() function

ropenblas::last_version_r()

# You can also specify the largest version

ropenblas::last_version_r(major = 3L)

# If major = NULL, the function will consider the major release number

ropenblas::last_version_r(major = NULL)