#' Gather chroma/timbre vectors
#'
#' These functions gather chroma or timbre vectors into long format.
#'
#' @param dat Data frame with a \code{pitches} or \code{timbre} column.
#'
#' @export
compmus_gather_chroma <- function(dat) {
  dat %>%
    dplyr::mutate(pitches = purrr::map(pitches, dplyr::bind_rows)) %>%
    tidyr::unnest(pitches) %>%
    tidyr::gather("pitch_class", "value", C:B) %>%
    dplyr::mutate(pitch_class = forcats::fct_shift(factor(pitch_class), 3))
}

#' @describeIn compmus_gather_chroma Gather chroma/timbre vectors
#' @importFrom magrittr %>%
#' @export
compmus_gather_timbre <- function(dat) {
  dat %>%
    dplyr::mutate(timbre = purrr::map(timbre, dplyr::bind_rows)) %>%
    tidyr::unnest(timbre) %>%
    tidyr::gather("basis", "value", c01:c12)
}

.circshift <- function(v, n) {
  if (n == 0) v else c(utils::tail(v, n), utils::head(v, -n))
}

#' Transpose chroma vectors to C
#'
#' Given a key estimate, transpose chroma vectors back to C.
#'
#' @param dat Data frame with a \code{pitches} column containing chroma
#'   vectors.
#' @param key Character string naming the global or local tonal centre.
#'
#' @importFrom magrittr %>%
#' @export
compmus_c_transpose <- function(dat, key) {
  KEY_SHIFTS <-
    list(
      C = 0,
      `C#` = -1,
      `Db` = -1,
      D = -2,
      `D#` = -3,
      `Eb` = -3,
      E = -4,
      F = -5,
      `F#` = -6,
      `Gb` = -6,
      G = -7,
      `G#` = -8,
      `Ab` = -8,
      A = -9,
      `A#` = -10,
      `Bb` = -10,
      B = -11
    )
  key <- ifelse(is_character(key), KEY_SHIFTS[[key]], -key)
  dat %>%
    dplyr::mutate(
      pitches =
        purrr::map(
          pitches,
          . %>%
            .circshift(key) %>%
            purrr::set_names(
              c(
                "C", "C#|Db", "D", "D#|Eb",
                "E", "F", "F#|Gb", "G",
                "G#|Ab", "A", "A#|Bb", "B"
              )
            )
        )
    )
}
