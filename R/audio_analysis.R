#' Get Spotify audio analysis tidily
#'
#' Fetches the Spotify audio analysis for a track using list columns rather than
#' lists of lists.
#'
#' See the
#' \href{https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-analysis/}{Spotify
#' developer documentation} for details about the information included in an
#' audio analysis.
#'
#' @param track_uri A string with a Spotify track URI.
#' @param ... Additional parameters passed to
#'   \code{\link[spotifyr]{get_track_audio_analysis}}.
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_tidy_audio_analysis('6IQILcYkN2S2eSu5IHoPEH')
get_tidy_audio_analysis <- function(track_uri, ...)
{
    spotifyr::get_track_audio_analysis(track_uri, ...) %>%
        list() %>% purrr::transpose() %>% tibble::as_tibble() %>%
        dplyr::mutate_at(
            dplyr::vars(meta, track),
            . %>% purrr::map(tibble::as_tibble)) %>%
        tidyr::unnest(cols = c(meta, track)) %>%
        dplyr::select(
            analyzer_version,
            duration,
            dplyr::contains('fade'),
            dplyr::ends_with('confidence'),
            bars:segments) %>%
        dplyr::mutate_at(
            dplyr::vars(bars, beats, tatums, sections),
            . %>% purrr::map(dplyr::bind_rows)) %>%
        dplyr::mutate(
            segments =
                purrr::map(
                    segments,
                    . %>%
                        tibble::as_tibble() %>%
                        dplyr::mutate(
                            pitches =
                                purrr::map(
                                    pitches,
                                    purrr::set_names,
                                    c(
                                        'C', 'C#|Db', 'D', 'D#|Eb',
                                        'E', 'F', 'F#|Gb', 'G',
                                        'G#|Ab', 'A', 'A#|Bb', 'B')),
                            timbre =
                                purrr::map(
                                    timbre,
                                    purrr::set_names,
                                    c(
                                        'c01', 'c02', 'c03', 'c04',
                                        'c05', 'c06', 'c07', 'c08',
                                        'c09', 'c10', 'c11', 'c12')))))
}

#' Add Spotify audio analysis to a data frame
#'
#' Fetches and joins the Spotify audio analysis for every Spotify URI in a data
#' frame.
#'
#' @param df A data frame with a \code{track_uri} column.
#' @param ... Parameters passed on to \code{\link{get_tidy_audio_analysis}}.
#'
#' @seealso \code{\link{get_tidy_audio_analysis}}
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' library(tidyverse)
#' get_playlist_audio_features('spotify', '37i9dQZF1DX21bRPJuEN7r') %>%
#'     slice(1:5) %>%
#'     add_audio_analysis
add_audio_analysis <- function(df, ...)
{
    df %>%
        dplyr::mutate(
            track.uri = stringr::str_remove(track.uri, 'spotify:track:'),
            analysis = purrr::map(track.uri, get_tidy_audio_analysis, ...)) %>%
        tidyr::unnest(analysis)
}
