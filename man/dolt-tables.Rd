% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dolt-tables.R
\name{dolt_branches}
\alias{dolt_branches}
\alias{dolt_remotes}
\alias{dolt_docs}
\alias{dolt_log}
\title{Dolt System Tables}
\usage{
dolt_branches(conn = dolt(), collect = NULL, show_sql = NULL)

dolt_remotes(conn = dolt(), collect = NULL, show_sql = NULL)

dolt_docs(conn = dolt(), collect = NULL, show_sql = NULL)

dolt_log(conn = dolt(), collect = NULL, show_sql = NULL)
}
\arguments{
\item{conn}{the database connection}

\item{collect}{whether to collect the result into R or return a \code{\link[dbplyr:tbl_lazy]{dbplyr::tbl_lazy()}}
to be further processed before collecting.  Defaults to \code{TRUE}, can be set with the \link[=dolt_vars]{environment variable}
\code{DOLT_COLLECT}.}

\item{show_sql}{Whether to print the SQL statements used internally to fetch
the data. Useful for learning how Dolt works internally. Defaults to \code{FALSE}, can
be set with the environment variable \code{DOLT_VERBOSE}.}
}
\description{
These functions query the dolt database for system tables that describe the
database version history and structure.
}
