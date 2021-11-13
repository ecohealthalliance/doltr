#' Find and check for the presence of a dolt binary
#' @export
#' @rdname dolt-binary
#' @importFrom processx run
is_dolt_installed <- function() {
  ret <- run(dolt_path(), "version")
  out <- isTRUE(ret$status == 0)
  if (out) {
    stdout <- ret$stdout
    attr(out, "version") <-
      numeric_version(regmatches(stdout, regexpr("[0-9\\.]+", stdout)))
  }
  out
}

#' @export
#' @rdname dolt-binary
dolt_version <- function() {
  installed <- is_dolt_installed()
  stopifnot(installed)
  attr(installed, "version")
}

#' @export
#' @rdname dolt-binary
dolt_path <- function() {
  Sys.getenv("DOLT_PATH", Sys.which("dolt"))
}

#' Initiate a dolt database directory
#'
#' @param dir path to the directory. Will be created if it does not exist
#' @importFrom processx run
#' @export
dolt_init <- function(dir = Sys.getenv("DOLT_DIR", "doltdb")) {
  if (!dir.exists(dir)) dir.create(dir)
  run(dolt_path(), "init", wd = dir, echo_cmd = Sys.getboolenv("DOLT_VERBOSE"))
  return(dir)
}

#' Export data from a dolt database
#' @param dir path to dolt database on-disk
#' @param format the export data format. One of `"sql"`, `"csv"`, or `"json"`
#' @param out the location on-disk for export. In the case of `"sql"`, format,
#'   a single file path (default `doltdump.sql`), otherwise a directory for all
#'   tables to be dumped as separate files (default "doltdump")
#' @param overwrite whether to overwrite existing files/directories.
#' @importFrom processx run
#' @importFrom R.utils getAbsolutePath
#' @return the path(s) of exported files
#' @export
dolt_dump <- function(format = c("sql", "csv", "json"),
                      out = NULL,
                      overwrite = FALSE,
                      dir = Sys.getenv("DOLT_DIR", "doltdb")) {

  format = match.arg(format)
  args <- c(paste0("--result-format=", format))
  if (format == "sql") {
    if (is.null(out)) out <- "doltdump.sql"
    out <- getAbsolutePath(out)
    args <- c(args, paste0("--file-name=", out))
  } else {
    if (is.null(out)) out <- "doltdump"
    out <- getAbsolutePath(out)
    args <- c(args, paste0("--directory=", out))
  }
  if (overwrite) args <- c(args, "--force")
  run(dolt_path(), c("dump", args), wd = dir, echo_cmd = Sys.getboolenv("DOLT_VERBOSE"))

  if (format == "sql") {
    return(out)
  } else {
    return(list.files(out, full.names = TRUE))
  }

}

# #' Log in to DoltHub and manage credentials
# #'
# #' DoltHub login credentials are stored as [JSON Web keys](https://cran.r-project.org/web/packages/jose/vignettes/jwk.html)
# #' in you `~/.dolt/creds` directory, and the key named in your
# #' [`user.creds` config variable][dolt_config_get()] is used for push and pull requests
# #' to DoltHub.
# #'
# #' @param creds A credential, typically the name of a credential file in
# #' ~/.dolt/creds.
# #' @export
# dolt_login <- function(creds = NULL) {
#   if (!interactive() && is.null(creds))
#     stop("Creating new credentials must be done interactively.")
#   args <- character(0)
#   if (!is.null(creds)) args <- creds
#   out <- run(dolt_path(), c("login", args), stdout = "", stderr = "")
#   stopifnot(out$status == 0)
#   out$status == 0
# }
#
# #' @export
# #' @rdname dolt-creds
# dolt_creds_ls <- function() {
#   list.files(file.path(Sys.getenv("DOLT_ROOT_DIR", "~/.dolt"), "creds"))
# }
#
# #' @export
# #' @rdname dolt-creds
# dolt_creds_use <- function(creds) {
#   all_creds <- dolt_creds_ls()
#   creds_file <- all_creds[grepl(creds, all_creds, fixed=TRUE)]
#   if (length(creds_file) != 1)
#     stop("`creds` does not match one and only one existing credential")
#   dolt_config_set(list(user.creds = sub("\\.jwk$", "", creds_file)))
# }
#
# #' @export
# #' @rdname dolt-creds
# dolt_creds_rm <- function(creds) {
#   all_creds <- dolt_creds_ls()
#   creds_file <- all_creds[grepl(creds, all_creds, fixed=TRUE)]
#   if (length(creds_file) != 1)
#     stop("`creds` does not match one and only one existing credential")
#   file.remove(creds_file)
#   TRUE
# }
#
#
# #' @export
# #' @rdname dolt-creds
# dolt_creds_check <- function(creds = NULL, endpoint = "doltremoteapi.dolthub.com:443") {
#   if (is.null(creds)) creds <- dolt_config_get("user.creds")[[1]]
#   out <- run(
#     dolt_path(),
#     c("creds", "check", "--creds", creds, "--endpoint", endpoint),
#     stdout = "", stderr = "", echo_cmd = Sys.getboolenv("DOLT_VERBOSE"))
#
#   out$status == 0
# }
#
