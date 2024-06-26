% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_and_store_data.R
\name{fetch_and_store_data}
\alias{fetch_and_store_data}
\title{A function to store data directly from BDL}
\usage{
fetch_and_store_data(
  variableID = "",
  level = "",
  year = "",
  name = "",
  output_dir = getwd()
)
}
\arguments{
\item{variableID}{ID of the data set.}

\item{level}{Level of aggregation as a number. If left blank function downloads all available years.}

\item{year}{Year of the variable. If left blank - package downloads all available data.}

\item{name}{Name of the data set.}

\item{output_dir}{Directory of desired location.}
}
\value{
Data set from BDL. Stored in the specified location as RDS file.
}
\description{
With this function user can download data directly from BDL directly into their project. We recommend using our complementary Shiny app to see the Variable IDs.
}
\examples{
fetch_and_store_data(variableID = "3507",level = "5",year = "2020", name = "xdxd")
}
\author{
Mateusz Pliszka Łukasz Janisiów Dominika Lach
}
