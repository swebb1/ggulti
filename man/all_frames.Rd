% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggulti.R
\name{all_frames}
\alias{all_frames}
\title{Helper function used by plot_play() function. Takes a dataframe of objects/arrows and fills so that each object/arrow appears in all frames}
\usage{
all_frames(df, frames)
}
\arguments{
\item{df}{Dataframe of objects/arrows}

\item{frames}{Total number of frames to fill}
}
\value{
A data.frame
}
\description{
Helper function used by plot_play() function. Takes a dataframe of objects/arrows and fills so that each object/arrow appears in all frames
}
\examples{
all_frames(throws,3)
}
