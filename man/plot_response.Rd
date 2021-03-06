% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/response.R
\name{plot_response}
\alias{plot_response}
\title{Plot output from \code{response()}}
\usage{
plot_response(data, order_n = FALSE, response_lim = NULL)
}
\arguments{
\item{data}{Data frame. Output from \code{response()}.}

\item{order_n}{Logical. If \code{TRUE}, groups are ordered by number of observations.}

\item{response_lim}{Optional x axis limits passed to \code{ggplot()} e.g. \code{c(0,1)}.}
}
\description{
A mean and confidence interval is plotted by group. The axes are flipped so that
groups are displayed on the y-axis, with response on the x-axis. A vertical dashed
line shows the overall mean of the data. Colours indicate whether the confidence interval
crosses the global mean (blue), or is entirely above it (green) or below it (red).
}
