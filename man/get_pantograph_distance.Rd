% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotters.R
\name{get_pantograph_distance}
\alias{get_pantograph_distance}
\title{Get offset from the plane of anchor points for a given pantograph set up}
\usage{
get_pantograph_distance(pos1, pos2, segment_length, segment_number)
}
\arguments{
\item{pos1}{First anchor point of pantograph}

\item{pos2}{Second anchor point of pantograph}

\item{segment_length}{The length of scissor segments}

\item{segment_number}{The number of scissor segments}
}
\value{
numeric
}
\description{
Get offset from the plane of anchor points for a given pantograph set up
}
\examples{
get_pantograph_distance(c(0, 0), c(1, 1), 3, 1)
}

