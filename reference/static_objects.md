# Create multiple a list of pitch_objects which do not move throughout all frames. Objects are represented with geom_point in the plot_play() function. Static objects could be cones or players in a queue.

Create multiple a list of pitch_objects which do not move throughout all
frames. Objects are represented with geom_point in the plot_play()
function. Static objects could be cones or players in a queue.

## Usage

``` r
static_objects(
  label,
  show = F,
  object = "Offense",
  x,
  y,
  frame = 1,
  alpha = 0.5,
  label_pos = "down"
)
```

## Arguments

- label:

  Name for the object (required)

- show:

  Show the label in plots : default = F

- object:

  Name for the type of object : "Offense" (default), "Defense", "Coach",
  "Cone", "Disc" are recognised but can be anything.

- x:

  Vector of x-coords covered by the object (required)

- y:

  Vector of y-coords covered by the object (required)

- frame:

  Vector of frames to show the object in : default = 1

- alpha:

  Colour opacity of the object : default = 0.5

- label_pos:

  Position of label relative to object : "down" (default), "up", "left",
  "right"

## Value

A data.frame

## Examples
