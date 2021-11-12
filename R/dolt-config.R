#' Get and set Dolt configuration variables
#'
#' @param params What parameters to get or set. Can include `user.name`, `user.email`,
#' and `user.creds`.  For `dolt_config_set`, this should be a named character
#' vector or list with parameter names and values.
#' @param global Set global or database-specific credentials
#' @param local_dir if not `global`, what local database to set variables for
#' @export
#' @importFrom jsonlite fromJSON
#' @name dolt-config
#' @seealso dolt_vars
dolt_config_get <- function(params = NULL, global = TRUE,
                            local_dir = Sys.getenv("DOLT_DIR")) {

  vars <- fromJSON(dolt_config_file(global, local_dir))
  if (!is.null(params)) vars <- vars[params]
  vars
}

#' @export
#' @rdname dolt-config
#' @importFrom jsonlite write_json
dolt_config_set <- function(params, global = TRUE,
                            local_dir = Sys.getenv("DOLT_DIR")) {
  vars <- dolt_config_get(global = global, local_dir = local_dir)
  vars[names(params)] <- params
  vars <- vars[order(names(vars))]
  write_json(vars,
             dolt_config_file(global, local_dir),
             auto_unbox = TRUE)
  invisible(vars)
}

dolt_config_file <- function(global = TRUE, local_dir = Sys.getenv("DOLT_DIR")) {
  if (global) {
    config_file <- file.path(Sys.getenv("HOME"),
                             Sys.getenv("DOLT_ROOT_PATH",  ".dolt"),
                             "config_global.json")
  } else {
    config_file <- file.path(local_dir, ".dolt", "config.json")
  }
  config_file
}

