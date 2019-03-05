# Copyright 2017 Charlie Thompson
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# These functions are borrowed from spotifyr v. 1.1.

.get_track_popularity <- function(tracks, access_token = spotifyr::get_spotify_access_token()) {

    num_loops <- ceiling(nrow(tracks %>% dplyr::filter(!duplicated(track_uri))) / 50)

    purrr::map_df(1:num_loops, function(this_loop) {

        uris <- tracks %>%
            dplyr::filter(!duplicated(track_uri)) %>%
            dplyr::slice(((this_loop * 50) - 49):(this_loop * 50)) %>%
            dplyr::select(track_uri) %>% .[[1]] %>% paste0(collapse = ',')

        res <- httr::RETRY('GET', url = stringr::str_glue('https://api.spotify.com/v1/tracks/?ids={uris}'), query = list(access_token = access_token), quiet = TRUE) %>% httr::content()

        content <- res$tracks

        df <- purrr::map_df(1:length(content), function(this_row) {

            this_track <- content[[this_row]]

            open_spotify_url <- ifelse(is.null(this_track$external_urls$spotify), NA, this_track$external_urls$spotify)
            preview_url <- ifelse(is.null(this_track$preview_url), NA, this_track$preview_url)

            list(
                track_uri = this_track$id,
                track_popularity = this_track$popularity,
                track_preview_url = preview_url,
                track_open_spotify_url = open_spotify_url
            )
        })

        return(df)
    })
}

.get_playlists <- function(username, playlist_uris, access_token = spotifyr::get_spotify_access_token()) {

    purrr::map_df(playlist_uris, function(this_playlist) {
        url <- stringr::str_glue('https://api.spotify.com/v1/users/{username}/playlists/', this_playlist)

        content <- httr::RETRY('GET', url, query = list(access_token = access_token), quiet = TRUE) %>% httr::content()

        playlist_list <- content %>%
            list %>%
            list

        purrr::map_df(1:length(playlist_list), function(this_playlist) {

            tmp <- playlist_list[[this_playlist]]
            purrr::map_df(1:length(tmp), function(this_row) {

                tmp2 <- tmp[[this_row]]

                if (!is.null(tmp2)) {
                    name <- ifelse(is.null(tmp2$name), NA, tmp2$name)
                    uri <- ifelse(is.null(tmp2$id), NA, tmp2$id)
                    snapshot_id <- ifelse(is.null(tmp2$snapshot_id), NA, tmp2$snapshot_id)

                    if (length(tmp2$images) > 0) {
                        img <- tmp2$images[[1]]$url
                    } else {
                        img <- NA
                    }

                    list(
                        playlist_name = name,
                        playlist_uri = uri,
                        playlist_tracks_url = tmp2$tracks$href,
                        playlist_num_tracks = tmp2$tracks$total,
                        snapshot_id = snapshot_id,
                        playlist_img = img
                    )
                } else {
                    return(tibble::tibble())
                }
            })
        }) %>% dplyr::filter(!is.na(playlist_uri), !is.na(playlist_name))
    })

}

.get_playlist_tracks <- function(playlists, access_token = spotifyr::get_spotify_access_token(), future_plan = 'multiprocess') {

    map_args <- list(
        1:nrow(playlists),
        function(this_playlist) {

            num_loops <- ceiling(playlists$playlist_num_tracks[this_playlist] / 100)

            if (num_loops == 0) {
                df <- tibble::tibble()
            } else {
                df <- purrr::map_df(1:num_loops, function(this_loop) {
                    res <- httr::RETRY('GET', url = playlists$playlist_tracks_url[this_playlist], query = list(access_token = access_token, limit = 100, offset = (100 * this_loop) - 100), quiet = TRUE, times = 10) %>% httr::content()

                    if (!is.null(res$error)) {
                        stop(stringr::str_glue('{res$error$message} ({res$error$status})'))
                    }

                    content <- res$items

                    if (length(content) == 0) {
                        track_info <- tibble::tibble()
                    } else {
                        track_info <- purrr::map_df(1:length(content), function(this_row) {

                            this_track <- content[[this_row]]

                            if (is.null(this_track$added_at)) {
                                track_added_at <- NA
                            } else {
                                track_added_at <- this_track$added_at
                            }

                            if (!is.null(this_track$track$id)) {

                                list(
                                    playlist_name = playlists$playlist_name[this_playlist],
                                    playlist_img = playlists$playlist_img[this_playlist],
                                    track_name = this_track$track$name,
                                    track_uri = this_track$track$id,
                                    artist_name = this_track$track$artists[[1]]$name,
                                    album_name = this_track$track$album$name,
                                    album_img = ifelse(length(this_track$track$album$images) > 0, this_track$track$album$images[[1]]$url, ''),
                                    track_added_at = as.POSIXct(track_added_at, format = '%Y-%m-%dT%H:%M:%SZ')
                                )
                            }
                        })
                    }
                })
            }
            return(df)
        }
    )

    do.call(purrr::map_df, map_args)

}

.get_track_audio_features <- function(tracks,
                                      access_token = spotify::get_spotify_access_token()
) {

    audio_feature_vars <- c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness',
                            'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature')

    # create lookup to classify key: https://developer.spotify.com/web-api/get-audio-features/
    pitch_class_lookup <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')

    if (!'data.frame' %in% class(tracks)) {
        sapply(tracks, .check_uri) #internal function uri checker
        num_loops <- ceiling(sum(!duplicated(tracks) / 100)) #if a character vector is inputed
    } else {
        num_loops <- ceiling(sum(!duplicated(tracks$track_uri)) / 100)
    }

    track_audio_features <- purrr::map_df(1:num_loops, function(this_loop) {

        if (!'data.frame' %in% class(tracks)) {
            uris <- paste0(tracks, collapse = ',')
        } else {
            uris <- tracks %>%
                dplyr::filter(!duplicated(track_uri)) %>%
                dplyr::slice(((this_loop * 100) - 99):(this_loop * 100)) %>%
                dplyr::select(track_uri) %>%
                .[[1]] %>%
                paste0(collapse = ',')
        }



        res <- httr::RETRY('GET',
                     url = stringr::str_glue('https://api.spotify.com/v1/audio-features/?ids={uris}'),
                     query = list(access_token = access_token),
                     quiet = TRUE, times = 10) %>%
            httr::content()

        content <- res$audio_features

        # replace nulls with NA and convert to character
        content <- purrr::map(content, function(row) {
            purrr::map(row, function(element) {
                ifelse(is.null(element), as.character(NA), as.character(element))
            })
        })

        null_results <- which(purrr::map_int(content, length) == 0)
        if (length(null_results) > 0) {
            content <- content[-null_results]
        }

        audio_features_df <- unlist(content) %>%
            matrix(nrow = length(content), byrow = T) %>%
            as.data.frame(stringsAsFactors = F)
        names(audio_features_df) <- names(content[[1]])

        return(audio_features_df)

    }) %>% dplyr::select(-c(type, uri, track_href, analysis_url)) %>%
        dplyr::rename(track_uri = id) %>%
        dplyr::mutate_at(audio_feature_vars, as.numeric) %>%
        dplyr::mutate(key = pitch_class_lookup[key + 1],
                      mode = dplyr::case_when(mode == 1 ~ 'major',
                                              mode == 0 ~ 'minor',
                                              TRUE ~ as.character(NA)),
                      key_mode = paste(key, mode))

    return(track_audio_features)
}

.check_uri <- function(track_uri) {
    is_uri <- function(x) {
        nchar(x) == 22 &
            !stringr::str_detect(x, ' ') &
            stringr::str_detect(x, '[[:digit:]]') &
            stringr::str_detect(x, '[[:lower:]]') &
            stringr::str_detect(x, '[[:upper:]]')
    }

    track_uri <- gsub('spotify:track:', '', track_uri)

    if (!is_uri(track_uri)) {
        stop('Error: Must enter a valid uri')
    }
}

#' Get features and popularity for all of a given set of playlists on Spotify
#'
#' This function, formerly part of \code{spotifyr}, returns the popularity and audio features for every song for a given set of playlists on Spotify
#' @param username String of Spotify username. Can be found on the Spotify app. (See http://rcharlie.net/sentify/user_uri.gif for example)
#' @param playlist_uris Character vector of Spotify playlist uris associated with the given \code{username}. Can be found within the Spotify App
#' @param parallelize Boolean determining to run in parallel or not. Defaults to \code{TRUE}.
#' @param future_plan String determining how `future()`s are resolved when `parallelize == TRUE`. Defaults to \code{multiprocess}.
#' @param access_token Spotify Web API token. Defaults to spotifyr::get_spotify_access_token()
#' @keywords track audio features playlists
#' @export
#' @examples
#' \dontrun{
#' playlist_username <- 'spotify'
#' playlist_uris <- c('37i9dQZF1E9T1oFsQFg98K', '37i9dQZF1CyQNOI21QVf3p')
#' my_playlist_audio_features <- get_playlist_audio_features('spotify', playlist_uris)
#' }

get_playlist_audio_features <- function(username, playlist_uris, access_token = spotifyr::get_spotify_access_token()) {

    playlists <- .get_playlists(username, playlist_uris, access_token = access_token)
    tracks <- .get_playlist_tracks(playlists, access_token = access_token)
    track_popularity <- .get_track_popularity(tracks, access_token = access_token)
    track_audio_features <- .get_track_audio_features(tracks, access_token = access_token)

    tots <- playlists %>%
        dplyr::select(-playlist_img) %>%
        dplyr::left_join(tracks, by = 'playlist_name') %>%
        dplyr::left_join(track_popularity, by = 'track_uri') %>%
        dplyr::left_join(track_audio_features, by = 'track_uri')

    return(tots)
}