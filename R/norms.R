#' Normalise vectors
#'
#' We use a number of vector normalisation strategies in Computational
#' Musicology. This function brings them together into one place, along with
#' common alternative names.
#'
#' The following methods are supported.
#' \describe{
#'     \item{\code{identity},\code{id}}{No normalisation.}
#'     \item{\code{harmonic}}{Harmonic mean.}
#'     \item{\code{manhattan},\code{L1}}{Manhattan (L1) norm.}
#'     \item{\code{euclidean},\code{L2}}{Euclidean (L2) norm.}
#'     \item{\code{chebyshev},\code{maximum}}{Chebyshev (maximum) norm.}
#'     \item{\code{aitchison},\code{clr}}{Aitchison's clr transformation.}
#'     \item{\code{softmax}}{Softmax.}}
#'
#' @param v A numeric vector.
#' @param method A character string indicating which normalization to use (see
#'   Details). Default is the Euclidean norm.
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' library(tidyverse)
#' get_tidy_audio_analysis('6IQILcYkN2S2eSu5IHoPEH') %>%
#'     select(segments) %>% unnest(segments) %>%
#'     mutate(pitches = map(pitches, compmus_normalise, 'euclidean'))
compmus_normalise <- compmus_normalize <- function(v, method = 'euclidean')
{
    ## Supported functions

    harmonic  <- function(v) v * sum(1 / abs(v))
    manhattan <- function(v) v / sum(abs(v))
    euclidean <- function(v) v / sqrt(sum(v^2))
    chebyshev <- function(v) v / max(abs(v))
    clr       <- function(v) {lv <- log(v); lv - mean(lv)}
    softmax   <- function(v) {exp(v) / sum(exp(v))}

    ## Method aliases

    METHODS <-
        list(
            identity  = identity,
            harmonic  = harmonic,
            manhattan = manhattan,
            L1        = manhattan,
            euclidean = euclidean,
            L2        = euclidean,
            chebyshev = chebyshev,
            maximum   = chebyshev,
            aitchison = clr,
            clr       = clr,
            softmax   = softmax)

    ## Function selection

    if (!is.na(i <- pmatch(method, names(METHODS))))
        METHODS[[i]](v)
    else
        stop('The method name is ambiguous or the method is unsupported.')
}

#' Pairwise distances in long format
#'
#' We use a number of distance measures in Computational Musicology.
#' \code{compmus_long_distance} brings them together into one place, along with
#' common alternative names. In order to support plotting, the distances are
#' returned in long format rather than matrix format. It is designed for
#' convenience, not speed.
#'
#' The following methods are supported. \describe{
#' \item{\code{manhattan},\code{citybolock},\code{taxicab},\code{L1},\code{totvar}}{Manhattan
#' distance.} \item{\code{euclidean},\code{L2}}{Euclidean distance.}
#' \item{\code{chebyshev},\code{maximum}}{Chebyshev distance.}
#' \item{\code{pearson},\code{correlation}}{Pearson's pseudo-distance.}
#' \item{\code{cosine}}{Cosine pseudo-distance.} \item{\code{angular}}{Angular
#' distance.} \item{\code{aitchison}}{Aitchison distance.} }
#'
#' @param xdat,ydat,dat Data frames.
#' @param feature An (unquoted) column name over which to compute distances.
#' @param method A character string indicating which distance metric to use (see
#'   Details). Default is Euclidean distance.
#' @return A tibble with columns \code{xstart}, \code{xduration}, \code{ystart},
#'   \code{yduration}, and \code{d}.
#'
#' @importFrom stats cor
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @export
#'
#' @examples
#' library(tidyverse)
#' tallis <-
#'     get_tidy_audio_analysis('2J3Mmybwue0jyQ0UVMYurH') %>%
#'     select(segments) %>% unnest(segments) %>%
#'     mutate(pitches = map(pitches, compmus_normalise, 'manhattan'))
#' chapelle <-
#'     get_tidy_audio_analysis('4ccw2IcnFt1Jv9LqQCOYDi') %>%
#'     select(segments) %>% unnest(segments) %>%
#'     mutate(pitches = map(pitches, compmus_normalise, 'manhattan'))
#'
#' compmus_long_distance(tallis, chapelle, pitches, method = 'euclidean')
#'
#' compmus_self_similarity(tallis, pitches, method = 'aitchison')
compmus_long_distance <- function(xdat, ydat, feature, method = 'euclidean')
{

    feature <- enquo(feature)

    ## Supported functions

    manhattan <- function(x, y) sum(abs(x - y))
    euclidean <- function(x, y) sqrt(sum((x - y) ^ 2))
    chebyshev <- function(x, y) max(abs(x - y))
    pearson   <- function(x, y) 1 - cor(x, y)
    cosine    <- function(x, y)
    {
        1 - sum(compmus_normalise(x, 'euc') * compmus_normalise(y, 'euc'))
    }
    angular   <- function(x, y) 2 * acos(1 - cosine(x, y)) / pi
    aitchison <- function(x, y)
    {
        euclidean(compmus_normalise(x, 'clr'), compmus_normalise(y, 'clr'))
    }

    ## Method aliases

    METHODS <-
        list(
            manhattan   = manhattan,
            cityblock   = manhattan,
            taxicab     = manhattan,
            L1          = manhattan,
            totvar      = manhattan,
            euclidean   = euclidean,
            L2          = euclidean,
            chebyshev   = chebyshev,
            maximum     = chebyshev,
            pearson     = pearson,
            correlation = pearson,
            cosine      = cosine,
            angular     = angular,
            aitchison   = aitchison)

    ## Function selection

    if (!is.na(i <- pmatch(method, names(METHODS))))
        dplyr::bind_cols(
            tidyr::crossing(
                xdat %>% dplyr::select(xstart = start, xduration = duration),
                ydat %>% dplyr::select(ystart = start, yduration = duration)),
            xdat %>% dplyr::select(x = !!feature) %>%
                tidyr::crossing(ydat %>% dplyr::select(y = !!feature)) %>%
                dplyr::transmute(d = purrr::map2_dbl(x, y, METHODS[[i]])))
    else
        stop('The method name is ambiguous or the method is unsupported.')
}

#' @describeIn compmus_long_distance Self-similarity matrices in long format
#' @importFrom rlang !! enquo
#' @export
compmus_self_similarity <- function(dat, feature, method = 'euclidean')
{
    feature <- enquo(feature)
    compmus_long_distance(dat, dat, !!feature, method)
}
