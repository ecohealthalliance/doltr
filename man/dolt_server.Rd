% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server.R
\name{dolt_server}
\alias{dolt_server}
\title{Start up a dolt SQL server and return the server process handle}
\usage{
dolt_server(
  dir = Sys.getenv("DOLT_DIR", "doltdb"),
  username = Sys.getenv("DOLT_USERNAME", "root"),
  password = Sys.getenv("DOLT_PASSWORD", ""),
  port = Sys.getenv("DOLT_PORT", 3306L),
  host = Sys.getenv("DOLT_HOST", "127.0.0.1"),
  find_port = TRUE,
  find_server = TRUE,
  multi_db = FALSE,
  autocommit = TRUE,
  read_only = FALSE,
  log_level = "info",
  log_out = NULL,
  timeout = 28800000,
  query_parallelism = 2,
  max_connections = 100,
  config_file = Sys.getenv("DOLT_CONFIG_FILE", "")
)
}
\arguments{
\item{dir}{The dolt directory to serve}

\item{username}{The username. Defaults to "root"}

\item{password}{The login password.  Defaults to empty.}

\item{port}{The TCP port for connections. Defaults to 3306.}

\item{host}{The IP of the host. Defaults to the local machine, \verb{127.0.0.1}}

\item{find_port}{if TRUE, switch to a different port if \code{port} is used by
another process}

\item{find_server}{if TRUE, find a server process serving the same directory
rather than starting a new one. Note that other server options will be
ignored. This allows the server to be used across R sessions. Note that to
make best use of this you may want to turn off the "Quit child processes on
exit" option in RStudio project options.}

\item{multi_db}{Serve multiple databases? If \code{TRUE}, \code{dir} should be a
directory with multiple subdirectories that are dolt databases}

\item{autocommit}{Automatically commit database changes to the working set?
If \code{FALSE}, anything not manually committed will be lost.}

\item{read_only}{should the database only allow read_only connections?}

\item{log_level}{Defines the level of logging provided. Options are "trace",
debug", "info", "warning", "error", and "fatal" (default "info").}

\item{log_out}{Where logging output should be directed.  If \code{"|"} it is passed
to \code{std_out()}, if \code{NULL} (default), it is suppressed.  Can also take
a filename. See \code{\link[processx:run]{processx::run()}}.}

\item{timeout}{Defines the timeout, in seconds, used for connections
(default \code{28800000})}

\item{query_parallelism}{Set the number of go routines spawned to handle each
query (default \code{2})}

\item{max_connections}{Set the number of connections handled by the server
(default \code{100})}

\item{config_file}{The path to a YAML config file to set these and additional
server configuration values.  See options in the
\href{https://docs.dolthub.com/interfaces/cli#dolt-sql-server}{dolt documentation}.}
}
\value{
A \code{dolt_server} object that is also a \code{\link[ps:ps_handle]{ps::ps_handle()}}
}
\description{
Start up a dolt SQL server and return the server process handle
}
