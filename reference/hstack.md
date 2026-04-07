# Helper function to print a list of 7 players in horizontal stack formation

Helper function to print a list of 7 players in horizontal stack
formation

## Usage

``` r
hstack()
```

## Value

A tibble

## Examples

``` r
hstack
#> function () 
#> {
#>     players <- list(pitch_object("H1", show = T, x = c(8.5), 
#>         y = c(22), alpha = 0.8, frame = 1), pitch_object("H2", 
#>         show = T, x = c(19), y = c(22), alpha = 0.8, frame = 1), 
#>         pitch_object("H3", show = T, x = c(29), y = c(22), alpha = 0.8, 
#>             frame = 1), pitch_object("C1", show = T, x = c(3), 
#>             y = c(40), alpha = 0.8, frame = 1), pitch_object("C2", 
#>             show = T, x = c(12.5), y = c(40), alpha = 0.8, frame = 1), 
#>         pitch_object("C3", show = T, x = c(24.5), y = c(40), 
#>             alpha = 0.8, frame = 1), pitch_object("C4", show = T, 
#>             x = c(34), y = c(40), alpha = 0.8, frame = 1))
#>     bind_rows(players)
#> }
#> <bytecode: 0x560dd87558e0>
#> <environment: namespace:ggulti>
```
