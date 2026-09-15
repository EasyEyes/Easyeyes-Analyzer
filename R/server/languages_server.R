# Server for the Languages tab.
#
# Renders one line-per-language plot for each measurement, and exposes a
# `downloadSpecs` reactive so the app-wide "Download All" button can include
# these plots when the Languages tab is active.

languagesTabServer <- function(id,
                               df_list,
                               experiment_names,
                               fileType,
                               data_list,
                               app_profiler = NULL) {
  moduleServer(id, function(input, output, session) {

    metadata <- reactive(language_session_metadata(data_list()))
    language_data <- reactive({
      req(df_list())
      slots <- c("reading", "rsvp", "crowding", "comfort", "beauty", "familiarity")
      lapply(df_list()[slots], add_language_column, metadata = metadata())
    })

    style_language_plot <- function(p) {
      if (is.null(p)) return(NULL)
      experiments <- unique(as.character(experiment_names()))
      experiments <- experiments[!is.na(experiments) & nzchar(experiments)]
      p + ggplot2::labs(title = paste(experiments, collapse = ", ")) + plt_theme
    }

    # Reactive plots ---------------------------------------------------------

    readingSpeedPlot <- reactive({
      req(df_list())
      app_profile_time(app_profiler, "Languages reading speed plot", {
        plot_language_reading_speed(language_data())
      })
    })

    readingProportionCorrectPlot <- reactive({
      req(df_list())
      app_profile_time(app_profiler, "Languages reading proportion correct plot", {
        plot_language_reading_proportion_correct(language_data())
      })
    })

    rsvpSpeedPlot <- reactive({
      req(df_list())
      app_profile_time(app_profiler, "Languages RSVP speed plot", {
        plot_language_rsvp_speed(language_data())
      })
    })

    crowdingPlot <- reactive({
      req(df_list())
      app_profile_time(app_profiler, "Languages crowding plot", {
        plot_language_crowding(language_data())
      })
    })

    comfortPlot <- reactive({
      req(df_list())
      app_profile_time(app_profiler, "Languages comfort plot", {
        plot_language_comfort(language_data())
      })
    })

    beautyPlot <- reactive({
      req(df_list())
      app_profile_time(app_profiler, "Languages beauty plot", {
        plot_language_beauty(language_data())
      })
    })

    familiarityPlot <- reactive({
      req(df_list())
      app_profile_time(app_profiler, "Languages familiarity plot", {
        plot_language_familiarity(language_data())
      })
    })

    # Renderers --------------------------------------------------------------

    render_language_plot <- function(plot_reactive, plot_id) {
      renderImage({
        p <- plot_reactive()
        validate(need(
          !is.null(p),
          "No valid data for this plot. Check the filters and upload results containing _language = ar, fa or ur."
        ))
        app_profile_time(app_profiler, paste("Languages image", plot_id), {
          png_plot <- apply_direct_png_theme(style_language_plot(p), profile = "plots")
          png_plot <- png_plot + ggplot2::theme(
            axis.text.x = ggplot2::element_text(size = 20, angle = 45, hjust = 1, vjust = 1)
          )
          render_plots_display_png(
            png_plot,
            width_in = 8,
            height_in = 6,
            disp_w = 700,
            limitsize = FALSE,
            use_png_theme = FALSE
          )
        })
      }, deleteFile = TRUE)
    }

    output$readingSpeedPlot <- render_language_plot(readingSpeedPlot, "readingSpeed")
    output$readingProportionCorrectPlot <-
      render_language_plot(readingProportionCorrectPlot, "readingProportionCorrect")
    output$rsvpSpeedPlot <- render_language_plot(rsvpSpeedPlot, "rsvpSpeed")
    output$crowdingPlot <- render_language_plot(crowdingPlot, "crowding")
    output$comfortPlot <- render_language_plot(comfortPlot, "comfort")
    output$beautyPlot <- render_language_plot(beautyPlot, "beauty")
    output$familiarityPlot <- render_language_plot(familiarityPlot, "familiarity")

    # Per-plot download handlers --------------------------------------------

    make_download <- function(plot_reactive, base_name) {
      downloadHandler(
        filename = function() {
          prefix <- get_short_experiment_name(experiment_names())
          if (is.null(prefix)) prefix <- ""
          paste0(prefix, "languages-", base_name, ".", fileType())
        },
        content = function(file) {
          p <- style_language_plot(plot_reactive())
          if (is.null(p)) {
            writeLines("No data available.", file)
            return(invisible())
          }
          savePlot(
            plot = p,
            filename = file,
            fileType = fileType(),
            width = 8,
            height = 6
          )
        }
      )
    }

    output$downloadReadingSpeed <- make_download(readingSpeedPlot, "reading-speed-vs-font")
    output$downloadReadingProportionCorrect <-
      make_download(readingProportionCorrectPlot, "reading-proportion-correct-vs-font")
    output$downloadRsvpSpeed <- make_download(rsvpSpeedPlot, "rsvp-speed-vs-font")
    output$downloadCrowding <- make_download(crowdingPlot, "crowding-threshold-vs-font")
    output$downloadComfort <- make_download(comfortPlot, "comfort-vs-font")
    output$downloadBeauty <- make_download(beautyPlot, "beauty-vs-font")
    output$downloadFamiliarity <- make_download(familiarityPlot, "familiarity-vs-font")

    # Download-all specs for the app-level "Download All" button ------------

    downloadSpecs <- reactive({
      specs <- list(
        list(plot = readingSpeedPlot(),               name = "reading-speed-vs-font"),
        list(plot = readingProportionCorrectPlot(),   name = "reading-proportion-correct-vs-font"),
        list(plot = rsvpSpeedPlot(),                  name = "rsvp-speed-vs-font"),
        list(plot = crowdingPlot(),                   name = "crowding-threshold-vs-font"),
        list(plot = comfortPlot(),                    name = "comfort-vs-font"),
        list(plot = beautyPlot(),                     name = "beauty-vs-font"),
        list(plot = familiarityPlot(),                name = "familiarity-vs-font")
      )
      specs <- Filter(function(s) !is.null(s$plot), specs)
      lapply(specs, function(s) plot_download_spec(
        style_language_plot(s$plot), s$name, theme = plt_theme, width = 8, height = 6
      ))
    })

    list(
      downloadSpecs = downloadSpecs
    )
  })
}
