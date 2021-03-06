% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/secondaryscripts.R
\name{DataAggregator}
\alias{DataAggregator}
\title{Aggregate Traffic Count Data over Time}
\usage{
DataAggregator(data, n)
}
\arguments{
\item{data}{The data that the user wishes to aggregate.}

\item{n}{The number of minutes that the user wishes to aggregate over. Note that this number
must be a factor of the number of minutes in a day (1440 minutes). For example, 15 is a valid
number, whereas 23 is not.}
}
\value{
A compressed version of the format of \code{data}, where the data has been summarised into
\code{n} minute intervals.
}
\description{
Motorway Incident Detection and Automatic Signalling (MIDAS) is a
sensor based network along UK motorways, and is designed to collect data regarding
traffic flows, average speeds and road occupancy, amongst other features, on the
road network. This data can be accessed via the MIDAS website
\url{https://www.midas-data.org.uk/} (login required) in the form of .tcd.bz2 files.

The function \code{DataAggregator} is designed to collect and summarise data the dataframe
generated by the \link[ScrapeMIDAS]{RoadData} function over a given number of minutes.
}
\examples{
fifteenMinuteDataSummary <- DataAggregator(trafficData, 15)
hourlyData_M4 <- DataAggregator(M4_Road_Data, 60)

}
\author{
RAC Foundation, Luke Hudlass-Galley
}
