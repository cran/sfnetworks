# nocov start
.onLoad = function(...) {
  has_ggplot2_3.0 =
    requireNamespace("ggplot2", quietly = TRUE) &&
    utils::packageVersion("ggplot2") >= "3.0.0"

  if (has_ggplot2_3.0)
    s3_register("ggplot2::autoplot", "sfnetwork")

  has_spatstat =
    requireNamespace("spatstat.linnet", quietly = TRUE) &&
    requireNamespace("spatstat.geom", quietly = TRUE)

  if (has_spatstat) {
    suppressPackageStartupMessages({
      s3_register("spatstat.linnet::as.linnet", "sfnetwork")
    })
  }

  has_sf_1.0 =
    requireNamespace("sf", quietly = TRUE) &&
    utils::packageVersion("sf") >= "1.0"

  if (has_sf_1.0)
    s3_register("s2::as_s2_geography", "sfnetwork")

  invisible()
}

# Register S3 helper
# Code copied from: https://github.com/r-lib/vctrs/blob/master/R/register-s3.R
# As suggested here: https://vctrs.r-lib.org/reference/s3_register.html
# (To avoid taking a dependency on vctrs for this one function,
# please feel free to copy and paste the function source into your own
# package.)
s3_register = function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces = strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package = pieces[[1]]
  generic = pieces[[2]]

  caller = parent.frame()

  get_method_env = function() {
    top = topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method = function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  method_fn = get_method(method)
  stopifnot(is.function(method_fn))

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns = asNamespace(package)

      # Refresh the method, it might have been updated by `devtools::load_all()`
      method_fn = get_method(method)

      registerS3method(generic, class, method_fn, envir = ns)
    }
  )

  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }

  envir = asNamespace(package)

  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }

  invisible()
}

.onAttach = function(libname, pkgname) {
  if (sf::sf_extSoftVersion()["proj.4"] < "6.0.0") {
    packageStartupMessage(paste0(
      'Warning: It seems that you are using an old version of PROJ. If you ',
      'use the built-in roxel dataset, please remember to reassign its CRS, ',
      'i.e. sf::st_crs(roxel) = sf::st_crs("EPSG:4326"), before running any ',
      'example.'
    ))
  }
}
# nocov end
