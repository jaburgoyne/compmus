if (!exists("multisession_started")) {
    plan(multisession)
    multisession_started <- TRUE
}

#' Make Sweep Plot
#'
#' Function via Peter Harrison
sweep_v_line_over_plot <- function(
        plot,
        x_start,
        x_end,
        duration,
        audio_path,
        output_path,
        fps = 30,
        width = 6,
        height = 4,
        edit_plot = function(plot, interval, plot_data, ...) plot,
        pars = list(),
        ...
) {
    tmp_dir <- tempfile(pattern = "")
    R.utils::mkdirs(tmp_dir)

    n_frames <- round(duration * fps)

    x_vals <- seq(from = x_start, to = x_end, length.out = n_frames)
    ind <- seq_along(x_vals)
    tmp_path_format <- "img%07d.png"
    tmp_paths <- file.path(tmp_dir, sprintf(tmp_path_format, ind))

    message("Creating plots...")
    furrr::future_map(
        ind,
        function(i) {
            theme_set(theme_pubr())
            tmp_path <- tmp_paths[i]
            x <- x_vals[i]

            tmp_plot <-
                plot +
                geom_vline(xintercept = x)

            tmp_plot <- edit_plot(tmp_plot, interval = x, plot_data = plot$data, pars = pars)

            ggsave(
                plot = tmp_plot,
                filename = tmp_path,
                width = width,
                height = height,
                ...
            )
        },
        .progress = TRUE
    )
    message("Encoding video...")

    suppressWarnings(file.remove(output_path))

    audio_bit_rate <- "600k"
    cmd <- glue::glue(
        "ffmpeg -r {fps} -f image2 -i {tmp_dir}/{tmp_path_format} -i '{audio_path}' -vcodec libx264 -crf 25  -pix_fmt yuv420p -acodec aac -b:a {audio_bit_rate} '{output_path}'"
    )
    exit <- suppressWarnings(system(cmd))
    if (exit == 127) {
        ffmpeg_path <- "/opt/homebrew/bin/"
        cmd_2 <- paste0(ffmpeg_path, cmd)
        exit_2 <- system(cmd_2)
        if (exit_2 == 127) {
            stop(
                "An error occurred when running the ffmpeg command. ",
                "You may need to install ffmpeg before continuing. ",
                "If you have ffmpeg installed, find its path by running 'where ffmpeg', ",
                "and edit the R code in the present file to put it as the new value of ",
                "ffmpeg_path."
            )
        }
    }
}
