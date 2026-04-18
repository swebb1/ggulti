# Convenience function to create a pitch_arrow dataframe representing a throw between two players in an object list.

Convenience function to create a pitch_arrow dataframe representing a
throw between two players in an object list.

## Usage

``` r
throw(
  object_list,
  label,
  show = F,
  from,
  to,
  frame = 1,
  throw_frame = NA,
  catch_frame = NA,
  release_frame = NA,
  alpha = 1,
  arrow_shape = "straight",
  space_x = NA,
  space_y = NA,
  label_pos = "start_down"
)
```

## Arguments

- object_list:

  List of pitch_objects (required)

- label:

  Name for the arrow (required)

- show:

  Show the label in plots : default = F

- from:

  Label of the throwing object

- to:

  Label of the receiving object or "space" if into empty space (provide
  space_x and space_y)

- frame:

  Vector of frames to show the arrow in : default = 1

- throw_frame:

  The frame representing the throw position of the to object

- catch_frame:

  The frame representing the receive positions of the from object

- alpha:

  Colour opacity of the arrow : default = 1

- arrow_shape:

  Shape of the arrow : "straight" (default), "bhrc", "fhrc", "bhio",
  "fhio" (give curved throwing shapes)

- space_x:

  x-coord of a throw into space (must specify to = "space")

- space_y:

  y-coord of a throw into space (must specify to = "space")

- label_pos:

  Position of label relative to start or end of arrow : "start_down"
  (default), "start_up", "end_down", "end_up"

## Value

A data.frame

## Examples

``` r
throw(player_list,"T1", from = "H1", to = "C1", frame = 1:3, throw_frame = 3)
#> Error: object 'player_list' not found
throw(player_list,"T1", from = "H1", to = "space", space_x = 5, space_y = 90, arrow_shape = "bhrc", frame = 2:3, throw_frame = 3)
#> Error: object 'player_list' not found
```
