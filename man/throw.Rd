% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggulti.R
\name{throw}
\alias{throw}
\title{Convenience function to create a pitch_arrow dataframe representing a throw between two players in an object list.}
\usage{
throw(
  object_list,
  label,
  show = F,
  from,
  to,
  frame = 1,
  throw_frame,
  alpha = 1,
  arrow_shape = "straight",
  space_x = NA,
  space_y = NA,
  label_pos = "start_down"
)
}
\arguments{
\item{object_list}{List of pitch_objects (required)}

\item{label}{Name for the arrow (required)}

\item{show}{Show the label in plots : default = F}

\item{from}{Label of the throwing object}

\item{to}{Label of the receiving object or "space" if into empty space (provide space_x and space_y)}

\item{frame}{Vector of frames to show the arrow in : default = 1}

\item{throw_frame}{The frame representing the throw and receive positions of the to and from objects}

\item{alpha}{Colour opacity of the arrow : default = 1}

\item{arrow_shape}{Shape of the arrow : "straight" (default), "bhrc", "fhrc", "bhio", "fhio" (give curved throwing shapes)}

\item{space_x}{x-coord of a throw into space (must specify to = "space")}

\item{space_y}{y-coord of a throw into space (must specify to = "space")}

\item{label_pos}{Position of label relative to start or end of arrow : "start_down" (default), "start_up", "end_down", "end_up"}
}
\value{
A data.frame
}
\description{
Convenience function to create a pitch_arrow dataframe representing a throw between two players in an object list.
}
\examples{
throw(player_list,"T1", from = "H1", to = "C1", frame = 1:3, throw_frame = 3)
throw(player_list,"T1", from = "H1", to = "space", space_x = 5, space_y = 90, arrow_shape = "bhrc", frame = 2:3, throw_frame = 3)
}
