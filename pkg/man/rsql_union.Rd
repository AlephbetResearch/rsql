% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsql_functions.R
\name{rsql_union}
\alias{rsql_union}
\title{Create a SQL union of two select statements}
\usage{
rsql_union(e1, e2, ..., all = TRUE)
}
\arguments{
\item{e1}{first statement}

\item{e2}{second statement}

\item{...}{additional statements}

\item{all}{logical value, whether or not to UNION ALL}
}
\description{
Create a SQL union of two select statements
}
\examples{
library(rsql)
sel1 = tab.select(.(x))
sel2 = tab.select(.(y))
to_sql(rsql_union(sel1,sel2))

}
