# Plotting function to create final graphic with a pitch, objects and arrows.

Plotting function to create final graphic with a pitch, objects and
arrows.

## Usage

``` r
plot_play(
  pitch = ggpitch(),
  arrow_list = NULL,
  object_list = NULL,
  static_frame = 1,
  animate = F,
  animation_res = 150,
  animation_width = 800,
  animation_height = 800,
  shadow = F,
  transition_length = 1,
  state_length = 0.5,
  keep_arrows = F,
  show_all = F,
  default_obj_size = 8,
  default_obj_shape = 19,
  default_obj_col = "#009E73",
  pv = ggulti_plot_values
)
```

## Arguments

- pitch:

  A pitch ggplot object create by ggpitch() : default = ggpitch()

- arrow_list:

  List of pitch_arrow dataframes

- object_list:

  List of pitch_object dataframes

- static_frame:

  Which frame to show in a static plot : default = 1

- animate:

  Output a gif instead of an image : F (default), T

- shadow:

  Add a shadow to objects in animation : T, F (default)

- transition_length:

  Transition time (s) for animations : default = 1

- state_length:

  State time (s) for animations : default = 0.5

- keep_arrows:

  Show arrows in all frames and only apply frames to objects : F
  (default), T

- show_all:

  Show all frames together in one image : F (default), T

- default_obj_size:

  Default point size for objects : default = 8

- default_obj_shape:

  Default shape for objects : default = 19

- default_obj_col:

  Default colour for objects : default = \#009E73

- animate_res:

  Manually set animation resolution : default = 80

## Value

A ggplot object

## Examples

``` r
plot_play(pitch,arrow_list,object_list,static_frame=2)
#> Error: object 'arrow_list' not found
```
