# Helper file for standalone Shiny app deployment
# This provides functions to replace the original path_setup.R logic

# Get the app root directory (where this file is located)
get_app_root <- function() {
  if (exists(".shiny_app_root")) {
    return(.shiny_app_root)
  }
  
  # Try to get script directory
  script_path <- tryCatch({
    if (!is.null(sys.frames()[[1]]$ofile)) {
      dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE))
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  if (!is.null(script_path) && dir.exists(script_path)) {
    return(script_path)
  }
  
  # Fallback to working directory
  return(getwd())
}

# Create global app root variable
.shiny_app_root <- get_app_root()
app_root <- .shiny_app_root

app_file <- function(path) {
  file.path(app_root, path)
}

app_source <- function(path, local = parent.frame(), ...) {
  source_local <- local
  if (isTRUE(local)) {
    source_local <- parent.frame()
  }
  source(app_file(path), local = source_local, ...)
}

# Override source_root to work with the standalone app structure
source_root <- function(path, ...) {
  app_source(path, ...)
}

# Keep project_root for backward compatibility
project_root <- app_root
