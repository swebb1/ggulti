# Create a pitch_object dataframe. Objects are represented with geom_point in the plot_play() function. Objects are usually players on the field.

Create a pitch_object dataframe. Objects are represented with geom_point
in the plot_play() function. Objects are usually players on the field.

## Usage

``` r
pitch_object(
  label = "",
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

``` r
pitch_object("H1", T, x = c(15,15,5), y = c(25,35,37), frame = 1:3)
#>   label show  object  x  y frame alpha label_pos
#> 1    H1   H1 Offense 15 25     1   0.5      down
#> 2    H1   H1 Offense 15 35     2   0.5      down
#> 3    H1   H1 Offense  5 37     3   0.5      down
pitch_object("D1", F, object = "Defense", x = c(13,13,8), y = c(25,35,37), frame = 1:3)
#>   label show  object  x  y frame alpha label_pos
#> 1    D1      Defense 13 25     1   0.5      down
#> 2    D1      Defense 13 35     2   0.5      down
#> 3    D1      Defense  8 37     3   0.5      down
pitch_object("q1", F, object = "Cone", x = c(13), y = c(26), frame = 1:3)
#>   label show object  x  y frame alpha label_pos
#> 1    q1        Cone 13 26     1   0.5      down
#> 2    q1        Cone 13 26     2   0.5      down
#> 3    q1        Cone 13 26     3   0.5      down
```
