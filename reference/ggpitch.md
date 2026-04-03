# Generate a pitch map as a ggplot object. The 'type' parameter controls the section of pitch. Default size is set to 100 x 37, with endzones 18 deep.

Generate a pitch map as a ggplot object. The 'type' parameter controls
the section of pitch. Default size is set to 100 x 37, with endzones 18
deep.

## Usage

``` r
ggpitch(
  type = "full",
  colour = "dimgray",
  fill = "white",
  endzone_fill = "#D55E00",
  linewidth = 0.5,
  alpha = 1,
  endzone_alpha = 0.2,
  linetype = "solid",
  xmin = 0,
  xmax = 37,
  ymin = 0,
  ymax = 100,
  zone_depth = 18
)
```

## Arguments

- type:

  Style of pitch: "full" (default), "half_attack", "half_defend",
  "blank", "void"

- colour:

  Colour of pitch outline : default = "dimgrey"

- fill:

  Fill colour of central zone : default = "white"

- endzone_fill:

  Fill colour of endzone : default = \#AA4422

- linewidth:

  Width of lines : default = 0.5

- alpha:

  Fill opacity of central zone : default = 1

- endzone_alpha:

  Fill opacity of endzone : default = 0.2

- linetype:

  Linetype of lines : default = solid

- xmin:

  Start x coordinate : default = 0

- xmax:

  End x coordinate : default = 37

- ymin:

  Start y coordinate : default = 0

- ymax:

  End y coordinate : default = 100

- zone_depth:

  Depth of endzones : default = 18

## Value

A ggplot object

## Examples

``` r
ggpitch()

ggpitch("half_attack",endzone_fill="blue")
```
