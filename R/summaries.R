#' Summarise possibly vector-valued columns
#'
#' Summarises vector-based features in nested list columns.
#'
#' The following methods are supported.
#'
#' @section Central Tendency:
#'
#' \describe{
#'     \item{\code{mean}}{arithmetic mean}
#'     \item{\code{aitchison},\code{acentre},\code{acenter}}{Aitchison centre}
#'     \item{\code{rms}}{root mean square}
#'     \item{\code{max}}{maximum}}
#'
#' @section Dispersion:
#'
#' \describe{
#'     \item{\code{sd}}{standard deviation}
#'     \item{\code{asd}}{standard deviation of clr-transformed components}
#'     \item{\code{sdsq}}{standard deviation of squares}
#'     \item{\code{varration}}{variation ratio}}
#'
#' @param dat A tibble containing list columns.
#' @param feature The vector-valued column of \code{dat} to summarise (unquoted).
#' @param method A character string indicating which summarisation method to use
#'   (see Details). Default is the arithmetic mean.
#' @param norm An optional character string indicating the method for
#'   pre-normalising each vector with \code{\link{compmus_normalise}}.
#'
#' @importFrom stats sd
#' @importFrom magrittr %>%
#' @importFrom rlang !! := enquo
#' @export

#' @examples
#' library(tidyverse)
#' get_tidy_audio_analysis('5ZLkc5RY1NM4FtGWEd6HOE') %>%
#'     compmus_align(bars, segments) %>%
#'     select(bars) %>% unnest(bars) %>%
#'     mutate(
#'         pitches =
#'             map(
#'                 segments,
#'                 compmus_summarise, pitches,
#'                 method = 'rms', norm = 'euclidean'))
compmus_summarise <- compmus_summarize <- function(dat, feature, method = 'mean', norm = 'id')
{
    feature <- enquo(feature)

    ## Support functions
    ## TODO: Add geometric median and Chebyshev center.
    ## TODO: Search for minimum sum of angular distances in hyper-quadrant I.

    clr     <- function(v) {lv = log(v); lv - mean(lv)}
    softmax <- function(v) {exp(v) / sum(exp(v))}
    square  <- function(v) v^2
    not_max <- function(v) v != max(v)

    ## Method aliases

    METHODS <-
        list(
            ## Central tendencies
            mean      = list( identity , mean , identity ),
            aitchison = list( clr      , mean , softmax  ),
            acenter   = list( clr      , mean , softmax  ),
            acentre   = list( clr      , mean , softmax  ),
            rms       = list( square   , mean , sqrt     ),
            max       = list( identity , max  , identity ),
            ## Dispersions
            sd        = list( identity , sd   , identity ),
            asd       = list( clr      , sd   , identity ),
            sdsq      = list( square   , sd   , identity ),
            varratio  = list( not_max  , mean , identity ))

    ## Function selection

    if (!is.na(i <- pmatch(method, names(METHODS))))
        dat %>%
        dplyr::transmute(
            !!feature :=
                purrr::map(
                    !!feature,
                    . %>%
                        compmus_normalise(norm) %>%
                        (METHODS[[i]][[1]]) %>%
                        dplyr::bind_rows())) %>%
        tidyr::unnest(!!feature) %>%
        dplyr::summarise_all(METHODS[[i]][[2]]) %>%
        purrr::map_dbl(1) %>%
        (METHODS[[i]][[3]])
    else
        stop('The method name is ambiguous or the method is unsupported.')
}

#' @importFrom magrittr %>%
.compmus_align_helper <- function(start0, duration0, inner)
{
    end0 <- start0 + duration0

    inner %>%
        dplyr::filter(start < end0) %>%
        dplyr::filter(
            pmin(end, end0) - pmax(start, start0) >=
                pmin(duration, duration0) / 2) %>%
        dplyr::select(-end)
}

#' @importFrom magrittr %>%
#' @importFrom rlang !! := enquo
.compmus_align_reduce <- function(outer, inner, name)
{
    outer %>%
        dplyr::mutate(
            !!name :=
                purrr::map2(
                    start,
                    duration,
                    .compmus_align_helper,
                    inner %>% dplyr::mutate(end = start + duration)))
}

#' Aligns lower-level Spotify segmentations with higher-level segmentations
#'
#' Embeds the \code{inner} segments as a list column with sub-tibbles
#' corresponding to each \code{outer} segment.
#'
#' @param dat A tibble.
#' @param outer,inner Tibble columns with \code{start} and \code{duration}
#'   columns.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !! := enquo
#' @export
#'
#' @examples
#' library(tidyverse)
#' get_tidy_audio_analysis('5ZLkc5RY1NM4FtGWEd6HOE') %>%
#'     compmus_align(bars, segments) %>%
#'     select(bars) %>% unnest(bars)
compmus_align <- function(dat, outer, inner)
{
    outer <- enquo(outer)
    inner <- enquo(inner)

    dat %>%
        dplyr::mutate(
            !!outer :=
                purrr::map2(!!outer, !!inner, .compmus_align_reduce, inner)) %>%
        dplyr::select(-!!inner)
}
