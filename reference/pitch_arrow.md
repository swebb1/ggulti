# Create a pitch_arrow dataframe. Arrows are represented with geom_segment or geom_curve in the plot_play() function. Arrows are usually player movements (cuts) or throws on the field.

Create a pitch_arrow dataframe. Arrows are represented with geom_segment
or geom_curve in the plot_play() function. Arrows are usually player
movements (cuts) or throws on the field.

## Usage

``` r
pitch_arrow(
  label = "",
  show = F,
  object = "Offense",
  type = "Cut",
  x,
  y,
  xend,
  yend,
  frame = 1,
  alpha = 1,
  arrow_shape = "straight",
  label_pos = "start_down"
)
```

## Arguments

- label:

  Name for the arrow (required)

- show:

  Show the label in plots : default = F

- object:

  Attribute an arrow to an object type to match colours : "Offense"
  (default), "Defense", "Coach", "Cone", "Disc" are recognised but can
  be anything.

- type:

  What the arrow represents : "Cut" (default), "Throw", "Label" are
  recognised but can be anything

- x:

  x-coord for start of arrow (required)

- y:

  y-coord for start of arrow (required)

- xend:

  x-coord for end of arrow (required)

- yend:

  y-coord for end of arrow (required)

- frame:

  Vector of frames to show the arrow in : default = 1

- alpha:

  Colour opacity of the arrow : default = 1

- arrow_shape:

  Shape of the arrow : "straight" (default), "bhrc", "fhrc", "bhio",
  "fhio" (give curved throwing shapes)

- label_pos:

  Position of label relative to start or end of arrow : "start_down"
  (default), "start_up", "end_down", "end_up"

## Value

A data.frame

## Examples

``` r
pitch_arrow("T1", type = "Throw", x = 5, y = 37, xend = 5, yend = 90, frame = 3)
#>   label show  object  type x  y xend yend frame alpha arrow_shape  label_pos
#> 1    T1      Offense Throw 5 37    5   90     3     1    straight start_down
pitch_arrow("C1", x = 5, y = 37, xend = 5, yend = 90, frame = 3)
#>   label show  object type x  y xend yend frame alpha arrow_shape  label_pos
#> 1    C1      Offense  Cut 5 37    5   90     3     1    straight start_down
```
