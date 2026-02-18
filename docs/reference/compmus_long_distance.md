# Pairwise distances in long format

We use a number of distance measures in Computational Musicology.
`compmus_long_distance` brings them together into one place, along with
common alternative names. In order to support plotting, the distances
are returned in long format rather than matrix format. It is designed
for convenience, not speed.

## Usage

``` r
compmus_long_distance(xdat, ydat, feature, method = "euclidean")

compmus_self_similarity(dat, feature, method = "euclidean")
```

## Arguments

- xdat, ydat, dat:

  Data frames with `start` and `duration` columns.

- feature:

  An (unquoted) column name over which to compute distances.

- method:

  A character string indicating which distance metric to use (see
  Details). Default is Euclidean distance.

## Value

A tibble with columns `xstart`, `xduration`, `ystart`, `yduration`, and
`d`.

## Details

The following methods are supported.

- `manhattan`,`citybolock`,`taxicab`,`L1`,`totvar`:

  Manhattan distance.

- `euclidean`,`L2`:

  Euclidean distance.

- `chebyshev`,`maximum`:

  Chebyshev distance.

- `pearson`,`correlation`:

  Pearson's pseudo-distance.

- `cosine`:

  Cosine pseudo-distance.

- `angular`:

  Angular distance.

- `aitchison`:

  Aitchison distance.

## Functions

- `compmus_self_similarity()`: Self-similarity matrices in long format
