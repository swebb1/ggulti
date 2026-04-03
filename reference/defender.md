# Convenience function to create a Defense pitch_object dataframe which tracks another objects movements.

Convenience function to create a Defense pitch_object dataframe which
tracks another objects movements.

## Usage

``` r
defender(
  player,
  label = "",
  show = NA,
  object = "Defense",
  xjust = -1,
  yjust = -1,
  frame = NA,
  alpha = NA,
  label_pos = NA
)
```

## Arguments

- player:

  Name of the object_dataframe to track (required)

- label:

  Name for the object (required)

- show:

  Show the label in plots : default = Same as player object, T, F

- object:

  Name for the type of object : "Defense" (default), "Offense", "Coach",
  "Cone", "Disc" are recognised but can be anything.

- xjust:

  Vector of adjustments to player x-coords to offset defenders position
  : default = -1

- yjust:

  Vector of adjustments to player y-coords to offset defenders position
  : default = -1

- frame:

  Vector of frames to show the object in : default = same as player
  object

- alpha:

  Colour opacity of the object : default = same as player object

- label_pos:

  Position of label relative to object : default = same as player
  object, "down" "up", "left", "right"

## Value

A data.frame

## Examples

``` r
defender(player_list[[1]], "D1", x = c(-1,-1,5), y = c(2,2,5))
#> Error: object 'player_list' not found
```
