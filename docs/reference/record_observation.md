# Record the observation object

Record the observation object

## Usage

``` r
record_observation(obs, heatmap_id = shiny_env$current_heatmap_id)
```

## Arguments

- obs:

  Observation object returned by
  [`shiny::observe()`](https://rdrr.io/pkg/shiny/man/observe.html) or
  [`shiny::observeEvent()`](https://rdrr.io/pkg/shiny/man/observeEvent.html).

- heatmap_id:

  The Heatmap ID.
