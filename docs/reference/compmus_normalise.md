# Normalise vectors

We use a number of vector normalisation strategies in Computational
Musicology. This function brings them together into one place, along
with common alternative names.

## Usage

``` r
compmus_normalise(v, method = "euclidean")

compmus_normalize(v, method = "euclidean")
```

## Arguments

- v:

  A numeric vector.

- method:

  A character string indicating which normalization to use (see
  Details). Default is the Euclidean norm.

## Details

The following methods are supported.

- `identity`,`id`:

  No normalisation.

- `harmonic`:

  Harmonic mean.

- `manhattan`,`L1`:

  Manhattan (L1) norm.

- `euclidean`,`L2`:

  Euclidean (L2) norm.

- `chebyshev`,`maximum`:

  Chebyshev (maximum) norm.

- `aitchison`,`clr`:

  Aitchison's clr transformation.

- `softmax`:

  Softmax.

## Functions

- `compmus_normalize()`: Normalise vectors
