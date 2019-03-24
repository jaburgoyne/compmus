#' @importFrom magrittr %>%
.sample_tempogram <- function(y, f_s, window_size, hop_size, window_function, cyclic, bpms)
{
    window <- window_function(window_size)
    if (cyclic) {
        bpm_octaves = rep(bpms, 5)
        bpms = bpms %>% tcrossprod(2^(-2:2)) %>% as.vector()
    } else {
        bpm_octaves = bpms
    }
    bases <-
        exp(tcrossprod(-2 * pi * 1i * (bpms / 60) / f_s, 0:(window_size - 1)))
    if (length(y) > window_size)
        starts <- seq(1, length(y) - window_size, by = hop_size)
    else
        starts <- 1
    windowed <- matrix(0, window_size, length(starts))
    for (n in 1:length(starts))
        windowed[ , n] <- window * y[starts[n]:(starts[n] + window_size - 1)]
    (bases %*% windowed) %>%
        abs %>%
        magrittr::raise_to_power(2) %>%
        as.vector %>%
        tibble::tibble(
            time = rep((starts + window_size / 2) / f_s, each = length(bpms)),
            bpm = rep(bpm_octaves, times = length(starts)),
            power = .) %>%
        dplyr::group_by(time, bpm) %>%
        dplyr::summarise(power = sum(power)) %>%
        dplyr::mutate(power = power / max(power)) %>%
        dplyr::ungroup()
}

#' Compute a tempogram from Spotify segment onsets
#'
#' Computes a Fourier-based tempogram based on onsets of Spotify segments.
#' Returns a tibble with \code{time}, \code{bpm}, and \code{power} columns.
#' Power is normalised to a max of 1 (Chebyshev norm) within each time point.
#'
#' @param track_analysis Spotify audio analysis as returned by
#'   \code{\link{get_tidy_audio_analysis}}.
#' @param window_size Window size in seconds (default 8).
#' @param hop_size Hop size in seconds (default 1).
#' @param cyclic Boolean stating whether the tempogram should be cyclic (default
#'   not).
#' @param bpms Vector of tempi in beats per minute to include in the tempogram
#'   (default 30--200 for non-cyclic and 80--160 for cyclic, inclusive of all
#'   integer tempi).
#' @param window_function Window function for the Fourier analysis (default
#'   Hamming).
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' library(tidyverse)
#' get_tidy_audio_analysis('6PJasPKAzNLSOzxeAH33j2') %>%
#'     tempogram(window_size = 4, hop_size = 2)
tempogram <- function(track_analysis, window_size = 8, hop_size = 1, cyclic = FALSE, bpms = if (cyclic) 80:160 else 30:600, window_function = signal::hamming)
{
    if (tibble::is_tibble(track_analysis))
        track_analysis <- track_analysis %>% purrr::flatten()

    assertthat::assert_that(
        track_analysis %>% rlang::has_name('duration'),
        track_analysis %>% rlang::has_name('segments'),
        track_analysis %>%
            purrr::pluck('segments') %>%
            rlang::has_name('start'),
        track_analysis %>%
            purrr::pluck('segments') %>%
            rlang::has_name('confidence'),
        msg = 'track_analysis is not a Spotify audio analysis.')

    onsets <-
        track_analysis %>%
        purrr::pluck('segments') %>%
        dplyr::pull(start) %>%
        magrittr::multiply_by(44100) %>%
        round()
    confidence <-
        track_analysis %>%
        purrr::pluck('segments') %>%
        dplyr::pull(confidence)

    novelty <- rep(0, 44100 * track_analysis[['duration']])
    novelty[onsets + 1] = confidence

    .sample_tempogram(
        novelty,
        f_s = 44100,
        window_size = 44100 * window_size,
        hop_size = 44100 * hop_size,
        cyclic = cyclic,
        bpms = bpms,
        window_function = window_function)
}
