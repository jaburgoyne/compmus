#' Wrangle chroma/timbre vectors
#'
#' These functions gather chroma or timbre data frames from Sonic Visualiser
#' into a nested format for further processing.
#'
#' @param dat Data frame from Sonic Visualiser based on a chroma- or cepstrogram
#'   layer.
#'
#' @export
compmus_wrangle_chroma <- function(dat) {
  dat |>
    dplyr::rename(start = TIME) |>
    dplyr::mutate(duration = dplyr::lead(start) - start) |>
    # TODO: This is a quick hack assuming all durations are equal.
    dplyr::mutate(
      duration = dplyr::if_else(is.na(duration), dplyr::lag(duration), duration)
    ) |>
    tidyr::nest(pitches = c(C, `C#`, D, Eb, E, F, `F#`, G, Ab, A, Bb, B))
}

#' @describeIn compmus_wrangle_chroma Wrangle chroma/timbre vectors
#' @export
compmus_wrangle_timbre <- function(dat) {
  dat |>
    dplyr::rename(start = TIME) |>
    dplyr::mutate(duration = dplyr::lead(start) - start) |>
    # TODO: This is a quick hack assuming all durations are equal.
    dplyr::mutate(
      duration = dplyr::if_else(is.na(duration), dplyr::lag(duration), duration)
    ) |>
    dplyr::rename_with(
      \(x) sprintf("mfcc_%02d", as.integer(stringr::str_extract(x, "\\d+"))),
      tidyselect::starts_with("BIN")
    ) |>
    tidyr::nest(timbre = mfcc_01:mfcc_21)
}
