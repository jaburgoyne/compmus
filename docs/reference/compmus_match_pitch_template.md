# Match chroma vectors against templates

Compares chroma vectors in a data frame against a list of templates,
most likely key or chord profiles.

## Usage

``` r
compmus_match_pitch_template(
  dat,
  templates,
  method = "cosine",
  norm = "euclidean"
)
```

## Arguments

- dat:

  A data frame containing chroma vectors in a `pitches` column.

- templates:

  A data frame with a `name` column for each template and the templates
  themselves in a `template` column.

- method:

  A character string indicating which distance metric to use (see
  [`compmus_long_distance`](https://jaburgoyne.github.io/compmus/reference/compmus_long_distance.md)).
  Default is cosine distance.

- norm:

  An optional character string indicating the method for pre-normalising
  each vector with
  [`compmus_normalise`](https://jaburgoyne.github.io/compmus/reference/compmus_normalise.md).
  Default is Euclidean.

## Value

A tibble with columns `start`, `duration`, `name`, and `d`.
