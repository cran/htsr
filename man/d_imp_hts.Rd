% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d_imp_hts.R
\name{d_imp_hts}
\alias{d_imp_hts}
\title{Import a hts file into a data base}
\usage{
d_imp_hts(db.sqlite, file_hts, table, bku = TRUE)
}
\arguments{
\item{db.sqlite}{Full name of the data base}

\item{file_hts}{Full name of hts file to import}

\item{table}{Table}

\item{bku}{Automatic Backup TRUE (default) / FALSE}
}
\value{
Actualized data base
}
\description{
Import a hts file into a tshm sqlite base
}
\details{
The main table where the data have to be removed must be selected with one the
following abbreviation: WL (water level), DI (discharge), WE (weather), PR (precipitation)
or QU (quality)
If records already exist during the same interval, they are removed and replaced.
}
\author{
P. Chevallier - jan 2019
}
