% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nettskjema_auth.R
\name{nettskjema_user_create}
\alias{nettskjema_user_create}
\title{Create Nettskjema API user}
\usage{
nettskjema_user_create(ip_version = c("v4", "v6"))
}
\arguments{
\item{ip_version}{IP version to look up. Either "v4" (default) or "v6".}
}
\value{
No return value, opens a browser for creating a API user.
}
\description{
Opens OS browser to create API user.
}
\examples{
\dontrun{
nettskjema_user_create()

# Turn off ip detection
nettskjema_user_create(ip = FALSE)
}
}
