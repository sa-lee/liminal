% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tour-utils.R
\name{compute_half_range}
\alias{compute_half_range}
\title{Compute range of axes for a tour}
\usage{
compute_half_range(.data, center = TRUE)
}
\arguments{
\item{.data}{A numeric matrix}

\item{center}{Subtract \code{colMeans(.data)} from each column in \code{.data}?
Default is TRUE.}
}
\value{
A numeric vector of length 1.
}
\description{
Compute range of axes for a tour
}
\details{
This function computes the maximum squared
Euclidean distance of rows in a matrix like object. Mostly used
internally for setting up xy-axis ranges for a tour animation.
}
\examples{
mv <- matrix(rnorm(300), ncol = 3)

compute_half_range(mv)

compute_half_range(mv, center = FALSE)
}
