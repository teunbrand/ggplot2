% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/facet-.R
\name{get_strip_labels}
\alias{get_strip_labels}
\title{Accessing a plot's facet strip labels}
\usage{
get_strip_labels(plot = get_last_plot())
}
\arguments{
\item{plot}{A ggplot or build ggplot object.}
}
\value{
\code{NULL} if there are no labels, otherwise a list of data.frames
containing the labels.
}
\description{
This functions retrieves labels from facet strips with the labeller applied.
}
\examples{
# Basic plot
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point()

get_strip_labels(p) # empty facets
get_strip_labels(p + facet_wrap(year ~ cyl))
get_strip_labels(p + facet_grid(year ~ cyl))
}
\keyword{internal}
