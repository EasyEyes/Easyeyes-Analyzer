arabic_to_western <- function(x) {
  chartr("٠١٢٣٤٥٦٧٨٩", "0123456789", x)
}


pxToPt <- function(px, pxPerCm) {
  return ((px / pxPerCm) * 72) / 2.54
}

ptToPx <- function(pt, pxPerCm) {
  return ((2.54 * pt) / 72) * pxPerCm
}

# Parse fontBoundingBoxReNominalRect strings like "[(-0.2,-0.3),(0.2,0.3)]"
# into numeric coords; MATLAB-style width = nums[3]-nums[1], height = nums[4]-nums[2].
parse_font_bbox_rect_nums <- function(s) {
  nums <- stringr::str_match_all(
    as.character(s),
    "[-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?"
  )[[1]][, 1]
  nums <- suppressWarnings(as.numeric(nums))
  nums[is.finite(nums)]
}

bbox_width_from_font_rect <- function(s) {
  nums <- parse_font_bbox_rect_nums(s)
  if (length(nums) < 4) {
    return(NA_real_)
  }
  nums[3] - nums[1]
}

bbox_height_from_font_rect <- function(s) {
  nums <- parse_font_bbox_rect_nums(s)
  if (length(nums) < 4) {
    return(NA_real_)
  }
  nums[4] - nums[2]
}

# ---------------------------------------------------------------------------
# fontBoundingBoxReNominalRect scale bug (EasyEyes / Acuity24Fonts archives)
#
# Background (Denis ↔ Gus, Sep 2026):
#   - With targetSizeIsHeight=FALSE, acuity size is letter WIDTH in deg.
#   - Width = ink width of the widest glyph in fontCharacterSet, re nominal size
#     (glyphs on a shared baseline, horizontally centered; union bbox / nominal).
#   - Older CSVs wrote fontBoundingBoxReNominalRect with a spurious px↔pt scale.
#     Recover with k = pxPerCm * 2.54 / 72 (that participant's pxPerCm).
#   - Fixed EasyEyes builds report the rect (and fontBoundingBoxWidthReNominal)
#     already unitless re nominal — do NOT multiply by k again.
#   - A separate reporting bug vertically recentered the box; width is unaffected.
#     Height column fontCharacterSetHeightReNominal (renamed
#     fontBoundingBoxHeightReNominal) was already correct and is used as h_good.
#
# How to decide whether this row still needs ×k (Gus; idempotent):
#   h_maybeBuggy = height from fontBoundingBoxReNominalRect (y2 - y1)
#   h_good       = fontBoundingBoxHeightReNominal, else fontCharacterSetHeightReNominal
#   Apply ×k iff |h_good - h_maybeBuggy| > |h_good - k * h_maybeBuggy|
# ---------------------------------------------------------------------------

font_bbox_px_pt_scale <- function(px_per_cm) {
  px <- suppressWarnings(as.numeric(px_per_cm))
  k <- px * 2.54 / 72
  k[!(is.finite(px) & px > 0)] <- NA_real_
  k
}

# TRUE when the rect still looks pre-fix (needs ×k).
font_bbox_needs_px_pt_correction <- function(h_maybe_buggy, h_good, k) {
  h_m <- suppressWarnings(as.numeric(h_maybe_buggy))
  h_g <- suppressWarnings(as.numeric(h_good))
  kk <- suppressWarnings(as.numeric(k))
  ok <- is.finite(h_m) & h_m > 0 & is.finite(h_g) & h_g > 0 & is.finite(kk) & kk > 0
  out <- rep(FALSE, length(h_m))
  out[ok] <- abs(h_g[ok] - h_m[ok]) > abs(h_g[ok] - kk[ok] * h_m[ok])
  out
}

# Pick the "good" height column from a data frame / named list of vectors.
font_bbox_good_height <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }
  if (is.data.frame(df) || is.list(df)) {
    if ("fontBoundingBoxHeightReNominal" %in% names(df)) {
      return(suppressWarnings(as.numeric(df[["fontBoundingBoxHeightReNominal"]])))
    }
    if ("fontCharacterSetHeightReNominal" %in% names(df)) {
      return(suppressWarnings(as.numeric(df[["fontCharacterSetHeightReNominal"]])))
    }
  }
  NULL
}

# Nominal bounding-box width from fontBoundingBoxReNominalRect.
# Multiplies by k = pxPerCm*2.54/72 only when height_good says the rect is buggy.
# If height_good is missing, falls back to applying ×k (legacy Acuity24Fonts
# archives always needed it; safer for old data than skipping).
font_bbox_width_re_nominal <- function(rect, px_per_cm, height_good = NULL) {
  width <- vapply(as.character(rect), bbox_width_from_font_rect, numeric(1))
  height_maybe <- vapply(as.character(rect), bbox_height_from_font_rect, numeric(1))
  k <- font_bbox_px_pt_scale(px_per_cm)

  n <- length(width)
  if (is.null(height_good)) {
    height_good <- rep(NA_real_, n)
  } else {
    height_good <- suppressWarnings(as.numeric(height_good))
    if (length(height_good) == 1L && n > 1L) {
      height_good <- rep(height_good, n)
    }
  }

  needs_k <- font_bbox_needs_px_pt_correction(height_maybe, height_good, k)
  # No usable h_good → keep legacy behavior (apply ×k) for old archives.
  no_h <- !(is.finite(height_good) & height_good > 0)
  needs_k[no_h & is.finite(k) & k > 0] <- TRUE

  out <- width
  out[needs_k] <- width[needs_k] * k[needs_k]
  out[!(is.finite(width) & width > 0)] <- NA_real_
  out[needs_k & !(is.finite(k) & k > 0)] <- NA_real_
  out
}

# Helper to get first non-NA calibration parameter value
get_first_non_na <- function(values) {
  non_na_values <- values[!is.na(values) & values != ""]
  if (length(non_na_values) > 0) {
    return(non_na_values[1])
  }
  return(NA)
}


# Drop common font file extensions for axis labels (e.g. ".woff2", ".woff", ".otf", ".ttf").
strip_font_filetype <- function(fonts) {
  sub("\\.(woff2|woff|otf|ttf)$", "", as.character(fonts), ignore.case = TRUE)
}

# Short display names for font axis labels (shared by violin + font-comparison plots).
font_comparison_axis_label <- function(fonts) {
  fonts <- strip_font_filetype(fonts)
  ifelse(fonts == "AgoesaDisplayRegular", "Agoesa", fonts)
}

# Helper function to add experiment name to plot title
add_experiment_title <- function(plot, experiment_name) {
  short_name <- get_short_experiment_name(experiment_name)
  
  if (is.null(plot) || is.null(short_name) || short_name == "") {
    return(plot)
  }

 
  # Remove trailing underscore for title display
  short_name <- gsub("_$", "", short_name)

  # Native-font legend patchwork: title panel is first (experiment name above
  # measure subtitle); do not attach labs(title=) to the scatter panel or it
  # lands under the legend.
  if (isTRUE(attr(plot, "crowding24_native_legend_patchwork", exact = TRUE)) &&
      inherits(plot, "patchwork") &&
      is.list(plot$patches$plots)) {
    # Three-column row figure: put experiment name above the whole row.
    if (isTRUE(attr(plot, "crowding24_paired_row_patchwork", exact = TRUE))) {
      return(
        plot + patchwork::plot_annotation(
          title = short_name,
          theme = ggplot2::theme(
            plot.title = ggplot2::element_text(size = 9, hjust = 0)
          )
        )
      )
    }
    for (i in seq_along(plot$patches$plots)) {
      child <- plot$patches$plots[[i]]
      if (!isTRUE(attr(child, "crowding24_title_panel", exact = TRUE))) {
        next
      }
      child <- child +
        ggplot2::labs(title = short_name) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 9,
            hjust = 0,
            margin = ggplot2::margin(b = 0)
          ),
          plot.title.position = "plot"
        )
      attr(child, "crowding24_title_panel") <- TRUE
      plot$patches$plots[[i]] <- child
      return(plot)
    }
  }

  # Get the current title
  original_title <- plot$labels$title
  
  # If there's no original title, use empty string
  if (is.null(original_title)) {
    original_title <- ""
  }
  
  # Create new title with short experiment name and line break
  new_title <- paste0(short_name, "\n", original_title)
  
  # Update the plot title
  plot <- plot + labs(title = new_title)
  
  return(plot)
}

# Legend drawn inside the panel (ggplot2 3.5+). Used when plt_theme_scatter
# would otherwise force legend.position = "top" above the panel.
# Sizes match crowding24 native-font legend text after PNG theme (~22 pt).
plots_legend_inside_theme <- function(x = 0.5, y = 0.10, just = c(0.5, 0)) {
  in_lower_left <- isTRUE(all(c(x, y) == 0))
  ggplot2::theme(
    legend.position = "inside",
    legend.position.inside = c(x, y),
    legend.justification = just,
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.background = if (in_lower_left) {
      ggplot2::element_blank()
    } else {
      ggplot2::element_rect(fill = scales::alpha("white", 0.92), color = NA)
    },
    legend.key = ggplot2::element_blank(),
    legend.key.size = ggplot2::unit(5.5, "mm"),
    legend.key.spacing.x = ggplot2::unit(4, "mm"),
    legend.key.spacing.y = ggplot2::unit(1.5, "mm"),
    legend.title = ggplot2::element_text(size = 16, hjust = 0),
    legend.text = ggplot2::element_text(size = 15),
    # In the corner, keep clear of the inward axis ticks.
    legend.margin = if (in_lower_left) {
      ggplot2::margin(t = 2, r = 4, b = 8, l = 10)
    } else {
      ggplot2::margin(3, 5, 3, 5)
    }
  )
}

tag_legend_inside_panel <- function(plot, x = 0.5, y = 0.10, just = c(0.5, 0)) {
  attr(plot, "legend_inside_panel") <- list(x = x, y = y, just = just)
  plot
}

reapply_legend_inside_panel <- function(plot, inside) {
  if (!is.list(inside) || length(inside$x) != 1L || length(inside$y) != 1L) {
    return(plot)
  }
  just <- if (length(inside$just) == 2L) inside$just else c(0.5, 0)
  plot + plots_legend_inside_theme(inside$x, inside$y, just = just)
}

# Apply scatter theme, then restore an inside-panel legend if the plot was tagged.
apply_plt_theme_scatter <- function(plot) {
  if (is.null(plot) || is_placeholder_plot(plot)) {
    return(plot)
  }
  if (isTRUE(attr(plot, "crowding24_native_legend_patchwork", exact = TRUE))) {
    return(plot)
  }
  themed <- if (inherits(plot, "patchwork")) {
    plot & plt_theme_scatter
  } else {
    plot + plt_theme_scatter
  }
  reapply_legend_inside_panel(themed, attr(plot, "legend_inside_panel", exact = TRUE))
}

# Consistent plot download: PNG matches Plots-tab on-screen render when
# use_png_theme=TRUE; SVG/PDF use the same theme on a 140% larger canvas.
# Default use_png_theme=FALSE preserves legacy callers outside Plots/Languages.
savePlot <- function(plot,
                     filename,
                     fileType,
                     width = 8,
                     height = 6,
                     disp_w = NULL,
                     use_png_theme = FALSE,
                     png_theme_profile = "plots",
                     text_scale = 1,
                     vector_size_scale = 1.4,
                     limitsize = FALSE) {
  if (is.null(disp_w) || !is.finite(disp_w) || disp_w <= 0) {
    disp_w <- max(280, round(700 * (as.numeric(width)[1] / 8)))
  }
  save_plots_display_download(
    file = filename,
    plot = plot,
    file_type = fileType,
    width_in = width,
    height_in = height,
    disp_w = disp_w,
    use_png_theme = use_png_theme,
    png_theme_profile = png_theme_profile,
    text_scale = text_scale,
    limitsize = limitsize,
    vector_size_scale = vector_size_scale
  )
}

plot_download_spec <- function(plot,
                               filename,
                               theme = NULL,
                               width = 8,
                               height = 6,
                               disp_w = NULL,
                               use_png_theme = FALSE,
                               png_theme_profile = "plots",
                               text_scale = 1) {
  list(
    plot = plot,
    filename = filename,
    theme = theme,
    width = width,
    height = height,
    disp_w = disp_w,
    use_png_theme = use_png_theme,
    png_theme_profile = png_theme_profile,
    text_scale = text_scale
  )
}

plot_list_download_specs <- function(plotList,
                                     fileNames,
                                     theme = NULL,
                                     width = 8,
                                     height = 6,
                                     heights = NULL,
                                     append_index = FALSE,
                                     disp_w = NULL,
                                     use_png_theme = FALSE,
                                     png_theme_profile = "plots",
                                     text_scale = 1) {
  if (length(plotList) < 1) {
    return(list())
  }

  lapply(seq_along(plotList), function(i) {
    spec_height <- if (!is.null(heights) && length(heights) >= i) heights[[i]] else height
    plot_i <- plotList[[i]]
    attr_h <- attr(plot_i, "plots_display_height_in", exact = TRUE)
    if (is.numeric(attr_h) && length(attr_h) >= 1 && is.finite(attr_h[1]) && attr_h[1] > 0) {
      spec_height <- as.numeric(attr_h[1])
    }
    spec_width <- width
    attr_w <- attr(plot_i, "plots_display_width_in", exact = TRUE)
    if (is.numeric(attr_w) && length(attr_w) >= 1 && is.finite(attr_w[1]) && attr_w[1] > 0) {
      spec_width <- as.numeric(attr_w[1])
    }
    spec_disp_w <- disp_w
    if (is.null(spec_disp_w) || !is.finite(spec_disp_w) || spec_disp_w <= 0) {
      spec_disp_w <- max(280, round(700 * (as.numeric(spec_width)[1] / 8)))
    } else if (is.numeric(attr_w) && length(attr_w) >= 1 && is.finite(attr_w[1]) && attr_w[1] > 0) {
      spec_disp_w <- max(as.numeric(spec_disp_w), round(as.numeric(spec_disp_w) * (attr_w[1] / 7)))
    }
    filename <- if (append_index) paste0(fileNames[[i]], i) else fileNames[[i]]
    plot_download_spec(
      plot = plot_i,
      filename = filename,
      theme = theme,
      width = spec_width,
      height = spec_height,
      disp_w = spec_disp_w,
      use_png_theme = use_png_theme,
      png_theme_profile = png_theme_profile,
      text_scale = text_scale
    )
  })
}

save_download_specs_zip <- function(specs, zip_file, fileType, prefix = "", empty_message = "No plots available for this tab.") {
  owd <- setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)
  if (is.null(prefix)) {
    prefix <- ""
  }

  saved_files <- character()
  if (length(specs) > 0) {
    for (spec in specs) {
      plot <- spec$plot
      if (is_placeholder_plot(plot)) {
        next
      }
      if (!is.null(spec$theme)) {
        if (identical(spec$theme, plt_theme_scatter)) {
          plot <- apply_plt_theme_scatter(plot)
        } else {
          orig <- plot
          plot <- reapply_legend_inside_panel(
            plot + spec$theme,
            attr(orig, "legend_inside_panel", exact = TRUE)
          )
        }
      }

      filename <- paste0(prefix, spec$filename, ".", fileType)
      width <- if (is.null(spec$width)) 8 else spec$width
      height <- if (is.null(spec$height)) 6 else spec$height
      savePlot(
        plot = plot,
        filename = filename,
        fileType = fileType,
        width = width,
        height = height,
        disp_w = spec$disp_w,
        use_png_theme = isTRUE(spec$use_png_theme),
        png_theme_profile = if (is.null(spec$png_theme_profile)) "plots" else spec$png_theme_profile,
        text_scale = if (is.null(spec$text_scale)) 1 else spec$text_scale,
        vector_size_scale = 1.4
      )
      saved_files <- c(saved_files, filename)
    }
  }

  if (length(saved_files) < 1) {
    readme <- "README.txt"
    writeLines(empty_message, readme)
    saved_files <- readme
  }

  zip(zip_file, saved_files)
}

save_plot_with_error_handling <- function(plot, filename, 
                                          width = 6, height = 4,
                                          size = 5,
                                          unit = 'in', theme = NULL, 
                                          colorPalette = NULL, plotTitle = "") {
  tryCatch({
    # Apply theme and colorPalette if provided
    if (!is.null(theme)) plot <- plot + theme
    if (!is.null(colorPalette)) plot <- plot + scale_color_manual(values = colorPalette)
    ggsave(
      file = filename,
      plot = plot,
      device = svglite,
      width = width,
      height = height,
      unit = unit,
      limitsize = FALSE
    )
    list(src = filename, contenttype = 'svg')
  }, error = function(e) {
    # Show error in a ggplot-friendly way
    error_plot <- ggplot() +
      annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = paste("Error:", e$message),
        color = "red",
        size = size,
        hjust = 0.5,
        vjust = 0.5
      ) +
      theme_void() +
      labs(subtitle=plotTitle)
    # Save the error plot to a temp file
    ggsave(
      file = filename,
      plot = error_plot,
      device = svglite,
      width = width,
      height = height,
      unit = unit
    )
    list(
      src = filename,
      contenttype = 'svg',
      alt = paste0("Error in ", plotTitle)
    )
  })
}



safely_execute <- function(expression, error_message = "Error occurred", return_value = NULL) {
  result <- tryCatch({
    # Evaluate the expression
    eval(expression)
    
  }, error = function(e) {
    # If an error occurs, print the error and return specified value
    message("Error: ", e$message)
    if (!is.null(error_message)) {
      message("Context: ", error_message)
    }
    return_value
  })
  
  return(result)
}

append_plot_list <- function(plotList, fileNames, plot, fname, height = NULL, heights = list(), show_placeholder = TRUE) {
  # Default height in inches
  default_height <- 4
  if (!is.null(plot)) {
    plotList[[length(plotList) + 1]] <- plot
    fileNames[[length(fileNames) + 1]] <- fname
    heights[[length(heights) + 1]] <- ifelse(is.null(height), default_height, height)
  } else if (show_placeholder) {
    # Create a minimal placeholder - just "[X] title" on one line
    placeholder_plot <- ggplot() +
      annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = paste0(" x ", fname),
        hjust = 0.5,
        vjust = 0.5,
        size = 4.5,
        color = "#666666"
      ) +
      theme_void() +
      theme(
        # Explicitly blank ALL axis elements to override plt_theme
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()
      )
    
    # Mark this as a placeholder plot so it can be skipped during download
    class(placeholder_plot) <- c("placeholder_plot", class(placeholder_plot))
    
    plotList[[length(plotList) + 1]] <- placeholder_plot
    fileNames[[length(fileNames) + 1]] <- fname
    heights[[length(heights) + 1]] <- 4  # Small height for one line
  }
  return(list(plotList = plotList, fileNames = fileNames, heights = heights))
}

# Helper function to check if a plot is a placeholder (empty/no data)
is_placeholder_plot <- function(plot) {
  if (is.null(plot)) return(TRUE)
  if ("placeholder_plot" %in% class(plot)) return(TRUE)
  return(FALSE)
}

# Helper function to get a short experiment name for filenames
# If multiple experiments, pick the alphabetically first one
get_short_experiment_name <- function(experiment_names) {
  if (length(experiment_names) == 0) {
    return(NULL)
  }
  
  return(sort(experiment_names)[1])
}

get_stats_label <- function(data, needSlope, needCorr) {
  N = nrow(data)
  if (N == 0) return("")
  corr = format(
    round(
      cor(data$block_avg_log_WPM, 
          data$log_crowding_distance_deg, 
          method = "pearson"), 
      2), 
    nsmall = 2)
  
  
  slope <- data_for_stat %>%
    mutate(WPM = 10^(block_avg_log_WPM),
           cdd = 10^(log_crowding_distance_deg)) %>%
    do(fit = lm(WPM ~ cdd, data = .)) %>%
    transmute(coef = map(fit, tidy)) %>%
    unnest(coef) %>%
    mutate(slope = round(estimate, 2)) %>%
    filter(term == 'cdd') %>%
    select(-term)
}

get_range_breaks_length <- function(x) {
  # get the range for log log plot
  breaks <- c(0.01,0.03,0.1,0.3,1,3,10,30,100,300,1000,3000)
  maxX <- max(x) * 1.1
  minX <- min(x) * .9
  breaks <- breaks[which.max(breaks > minX): which.min(breaks < maxX)]
  length <- maxX / minX / 10 * 1.5
  return(list(
    range = c(minX, maxX),
    breaks = breaks,
    length = length
  ))
}

get_webGL <- function(data_list) {
  webGL <- tibble()
  for (i in 1:length(data_list)) {
    if ('WebGL_Report' %in% names(data_list[[i]])) {
      json_candidates <- unique(data_list[[i]]$WebGL_Report[!is.na(data_list[[i]]$WebGL_Report)])
      if (length(json_candidates) == 0) next
      json_txt <- as.character(json_candidates[1])
      json_txt <- trimws(json_txt)
      if (nchar(json_txt) >= 2 && substr(json_txt, 1, 1) == '"' && substr(json_txt, nchar(json_txt), nchar(json_txt)) == '"') {
        json_txt <- substr(json_txt, 2, nchar(json_txt) - 1)
      }
      if (grepl('""', json_txt, fixed = TRUE)) {
        json_txt <- gsub('""', '"', json_txt, fixed = TRUE)
      }
      t <- tryCatch(jsonlite::fromJSON(json_txt), error = function(e) NULL)
      if (is.null(t)) next
      if ('maxTextureSize' %in% names(t)) {
        df <- data.frame(
          participant = data_list[[i]]$participant[1],
          WebGLVersion = t$WebGL_Version,
          maxTextureSize = t$maxTextureSize,
          maxViewportSize = max(unlist(t$maxViewportSize)),
          WebGLUnmaskedRenderer = t$Unmasked_Renderer)
      } else {
        df <- data.frame(
          participant = data_list[[i]]$participant[1],
          WebGLVersion = ifelse("WebGL_Version" %in% names(t), t$WebGL_Version,""),
          maxTextureSize = ifelse("Max_Texture_Size" %in% names(t), t$Max_Texture_Size,""),
          maxViewportSize = ifelse("Max_Viewport_Dims" %in% names(t), max(unlist(t$Max_Viewport_Dims)),""),
          WebGLUnmaskedRenderer = ifelse("Unmasked_Renderer" %in% names(t), max(unlist(t$Unmasked_Renderer)),""))
      }
      df$date = data_list[[i]]$date[1]
      webGL = rbind(webGL, df)
    }
  }
  if (nrow(webGL) == 0) {
    webGL = tibble(
      participant = '',
      WebGLVersion = NA,
      maxTextureSize = NA,
      maxViewportSize = NA,
      WebGLUnmaskedRenderer = NA,
      date=NA)
  } else {
    webGL = tibble(
      participant = webGL$participant,
      date = webGL$date,
      WebGLVersion = webGL$WebGLVersion,
      maxTextureSize = as.numeric(webGL$maxTextureSize),
      maxViewportSize = as.numeric(webGL$maxViewportSize),
      WebGLUnmaskedRenderer = webGL$WebGLUnmaskedRenderer)
  }
  return(webGL)
}

get_N_text <- function(data) {
  # create a text in this format:
  #   conditionName1: N = xx\nconditionName2 N= xx
  text = c()
  t <- data %>%
    group_by(conditionName) %>%
    summarize(n = n(),
              .groups = "drop") %>%
    mutate(text = paste0(conditionName, ': N=', n))
  return(paste0(unique(t$text), collapse='\n'))
}

# Light clone for PNG theming.
# unserialize(serialize(plot)) deep-copies fat plot_env / ggproto closures
# (10–20s). Swapping layer$super also breaks aesthetic computation on many
# plots. Instead: shallow-copy the ggplot list and duplicate each layer env
# so in-place aes_params/geom_params/data edits do not touch the original.
clone_ggplot_layer_for_png_theme <- function(layer) {
  if (!is.environment(layer)) {
    return(layer)
  }
  new_layer <- new.env(parent = emptyenv())
  for (nm in ls(layer, all.names = TRUE)) {
    assign(nm, get(nm, envir = layer, inherits = FALSE), envir = new_layer)
  }
  class(new_layer) <- class(layer)
  if (is.list(new_layer$aes_params)) {
    new_layer$aes_params <- as.list(new_layer$aes_params)
  }
  if (is.list(new_layer$geom_params)) {
    new_layer$geom_params <- as.list(new_layer$geom_params)
  }
  if (is.data.frame(new_layer$data)) {
    new_layer$data <- new_layer$data[seq_len(nrow(new_layer$data)), , drop = FALSE]
  }
  new_layer
}

clone_ggplot_for_png_theme <- function(plot) {
  if (is.null(plot) || !inherits(plot, "ggplot")) {
    return(plot)
  }
  cls <- class(plot)
  kept_attrs <- attributes(plot)
  out <- as.list(unclass(plot))

  if (inherits(plot, "patchwork") && is.list(out$patches)) {
    patches <- out$patches
    if (is.list(patches$plots)) {
      patches$plots <- lapply(patches$plots, clone_ggplot_for_png_theme)
    }
    out$patches <- patches
  }

  if (is.list(out$layers)) {
    out$layers <- lapply(out$layers, clone_ggplot_layer_for_png_theme)
  }

  class(out) <- cls
  for (nm in setdiff(names(kept_attrs), c("class", "names"))) {
    attr(out, nm) <- kept_attrs[[nm]]
  }
  out
}

# Optional per-plot PNG axis scale factors (see apply_direct_png_theme).
tag_png_axis_scales <- function(plot, axis_text = 1, axis_title = 1) {
  if (is.null(plot)) {
    return(plot)
  }
  attr(plot, "png_axis_text_scale") <- as.numeric(axis_text)[1]
  attr(plot, "png_axis_title_scale") <- as.numeric(axis_title)[1]
  plot
}

copy_png_axis_scale_attrs <- function(from, to) {
  if (is.null(to)) {
    return(to)
  }
  for (nm in c("png_axis_text_scale", "png_axis_title_scale")) {
    v <- attr(from, nm, exact = TRUE)
    if (!is.null(v)) {
      attr(to, nm) <- v
    }
  }
  to
}

# Scale ggplot text/layers for on-screen PNG rendering (ragg path).
apply_direct_png_theme <- function(plot,
                                   profile = c("default", "plots", "histogram"),
                                   text_scale = 1,
                                   scale_title = FALSE,
                                   scale_subtitle = FALSE,
                                   scale_axis_title = TRUE,
                                   scale_axis_text = TRUE) {
  profile <- match.arg(profile)
  text_scale <- as.numeric(text_scale)[1]
  if (length(text_scale) != 1L || is.na(text_scale) || text_scale <= 0) text_scale <- 1
  read_axis_scale_attr <- function(plot, name) {
    raw <- attr(plot, name, exact = TRUE)
    if (is.null(raw)) return(1)
    val <- suppressWarnings(as.numeric(raw)[1])
    if (length(val) != 1L || is.na(val) || !is.finite(val) || val <= 0) 1 else val
  }
  axis_text_scale <- read_axis_scale_attr(plot, "png_axis_text_scale")
  axis_title_scale <- read_axis_scale_attr(plot, "png_axis_title_scale")
  default_text_layer_size <- 3
  stats_text_layer_size <- 4
  text_layer_multiplier <- 2
  lineheight_multiplier <- 0.5

  sizes <- switch(
    profile,
    default = list(
      title = 18, subtitle = 36, axis_title = 28, axis_text = 14,
      legend_title = 28, legend_text = 20, strip = 28, caption = 20
    ),
    plots = list(
      title = 18, subtitle = 36, axis_title = 28, axis_text = 20,
      legend_title = 28, legend_text = 20, strip = 28, caption = 20
    ),
    histogram = list(
      title = 14, subtitle = 29, axis_title = 22, axis_text = 20,
      legend_title = 22, legend_text = 16, strip = 22, caption = 16
    )
  )

  # Scale body text; keep filename (title) / measure subtitle unless requested
  if (isTRUE(scale_title)) sizes$title <- sizes$title * text_scale
  if (isTRUE(scale_subtitle)) sizes$subtitle <- sizes$subtitle * text_scale
  if (isTRUE(scale_axis_title)) {
    sizes$axis_title <- sizes$axis_title * text_scale * axis_title_scale
  }
  if (isTRUE(scale_axis_text)) {
    sizes$axis_text <- sizes$axis_text * text_scale * axis_text_scale
    sizes$legend_title <- sizes$legend_title * text_scale
    sizes$legend_text <- sizes$legend_text * text_scale
    sizes$strip <- sizes$strip * text_scale
    sizes$caption <- sizes$caption * text_scale
  }

  # Light-clone so layer mutations below do not touch the original plot
  # and we avoid fat unserialize(serialize(plot)).
  plot <- clone_ggplot_for_png_theme(plot)

  # Native-font legend plots are patchwork (title / legend / main).
  # Recurse into children for sizing; never apply axis text globally via `&`
  # (that reintroduces row/column numbers on the void legend panel).
  if (inherits(plot, "patchwork") &&
      isTRUE(attr(plot, "crowding24_native_legend_patchwork", exact = TRUE))) {
    png_plot <- plot
    if (is.list(png_plot$patches$plots)) {
      for (i in seq_along(png_plot$patches$plots)) {
        child <- png_plot$patches$plots[[i]]
        # wrap_elements() cells (e.g. grob legends) are sized by their builder.
        if (!(inherits(child, "ggplot") && !inherits(child, "patchwork")) ||
            inherits(child, "wrapped_patch")) {
          next
        }
        if (isTRUE(attr(child, "crowding24_legend_panel", exact = TRUE))) {
          child2 <- clone_ggplot_for_png_theme(child)
          for (layer_idx in seq_along(child2$layers)) {
            geom <- child2$layers[[layer_idx]]$geom
            if (inherits(geom, "GeomText") || inherits(geom, "GeomLabel")) {
              size <- child2$layers[[layer_idx]]$aes_params$size
              if (is.null(size)) size <- child2$layers[[layer_idx]]$geom_params$size
              size_num <- if (length(size) == 1) suppressWarnings(as.numeric(size)) else NA_real_
              if (!is.na(size_num)) {
                scaled <- size_num * text_layer_multiplier
                child2$layers[[layer_idx]]$aes_params$size <- scaled
                child2$layers[[layer_idx]]$geom_params$size <- scaled
              } else {
                ld <- child2$layers[[layer_idx]]$data
                if (is.data.frame(ld) && "text_size" %in% names(ld)) {
                  child2$layers[[layer_idx]]$data$text_size <-
                    suppressWarnings(as.numeric(ld$text_size)) * text_layer_multiplier
                }
              }
            }
          }
          # Keep axes fully suppressed after PNG theme processing.
          child2 <- child2 +
            ggplot2::theme(
              axis.text = ggplot2::element_blank(),
              axis.ticks = ggplot2::element_blank(),
              axis.title = ggplot2::element_blank(),
              axis.line = ggplot2::element_blank()
            )
          attr(child2, "crowding24_legend_panel") <- TRUE
          png_plot$patches$plots[[i]] <- child2
        } else if (isTRUE(attr(child, "crowding24_title_panel", exact = TRUE))) {
          child2 <- apply_direct_png_theme(
            child,
            profile = profile,
            text_scale = text_scale,
            scale_title = TRUE,
            scale_subtitle = TRUE,
            scale_axis_title = FALSE,
            scale_axis_text = FALSE
          )
          attr(child2, "crowding24_title_panel") <- TRUE
          png_plot$patches$plots[[i]] <- child2
        } else {
          # Inherit parent PNG axis scale tags so both scatter columns stay matched.
          child_for_theme <- copy_png_axis_scale_attrs(plot, child)
          child2 <- apply_direct_png_theme(
            child_for_theme,
            profile = profile,
            text_scale = text_scale,
            scale_title = scale_title,
            scale_subtitle = scale_subtitle,
            scale_axis_title = scale_axis_title,
            scale_axis_text = scale_axis_text
          )
          if (isTRUE(attr(child, "crowding24_main_panel", exact = TRUE))) {
            attr(child2, "crowding24_main_panel") <- TRUE
          }
          inside <- attr(child, "legend_inside_panel", exact = TRUE)
          if (is.list(inside) && length(inside$x) == 1L && length(inside$y) == 1L) {
            # Match col-1 native-font legend visual size (geom_text ~3.84 × 2 ≈ 22 pt).
            child2 <- reapply_legend_inside_panel(child2, inside) +
              ggplot2::theme(
                legend.title.position = "top",
                legend.title = ggplot2::element_text(size = 22, hjust = 0),
                legend.text = ggplot2::element_text(size = 20),
                legend.key.size = ggplot2::unit(6, "mm")
              )
            attr(child2, "legend_inside_panel") <- inside
          }
          # Paired-row scatters: lock identical axis text sizes on every main panel.
          if (isTRUE(attr(plot, "crowding24_paired_row_patchwork", exact = TRUE)) &&
              isTRUE(attr(child, "crowding24_main_panel", exact = TRUE))) {
            child2 <- child2 +
              ggplot2::theme(
                axis.title = ggplot2::element_text(
                  size = sizes$axis_title,
                  lineheight = lineheight_multiplier
                ),
                axis.title.x = ggplot2::element_text(
                  size = sizes$axis_title,
                  lineheight = lineheight_multiplier
                ),
                axis.title.y = ggplot2::element_text(
                  size = sizes$axis_title,
                  lineheight = lineheight_multiplier
                ),
                axis.text = ggplot2::element_text(
                  size = sizes$axis_text,
                  lineheight = lineheight_multiplier
                ),
                axis.text.x = ggplot2::element_text(
                  size = sizes$axis_text,
                  lineheight = lineheight_multiplier
                ),
                axis.text.y = ggplot2::element_text(
                  size = sizes$axis_text,
                  lineheight = lineheight_multiplier
                )
              )
          }
          png_plot$patches$plots[[i]] <- child2
        }
      }
    }
    # wrap_plots(title, legend, main): title+legend live in patches$plots;
    # the main scatter is the patchwork's own ggplot base and was never themed
    # above — without this, axis labels stay microscopic vs other Plots-tab PNGs.
    axis_text_x <- ggplot2::element_text(
      size = sizes$axis_text,
      lineheight = lineheight_multiplier
    )
    legend_title_size <- sizes$legend_title
    legend_text_size <- sizes$legend_text
    legend_key_theme <- ggplot2::theme()
    if (isTRUE(attr(plot, "crowding24_paired_row_patchwork", exact = TRUE))) {
      # Journal figure: legend text as large as the axis numbers.
      legend_title_size <- sizes$axis_text
      legend_text_size <- sizes$axis_text
      # Tight rows so the legend fits below the dashed r = 1 line.
      legend_key_theme <- ggplot2::theme(
        legend.key.size = ggplot2::unit(0.6 * sizes$axis_text, "pt"),
        legend.key.spacing.y = ggplot2::unit(0, "pt"),
        legend.key.spacing.x = ggplot2::unit(0.15 * sizes$axis_text, "pt")
      )
    }
    png_plot <- png_plot + legend_key_theme +
      ggplot2::theme(
        axis.title = ggplot2::element_text(
          size = sizes$axis_title,
          lineheight = lineheight_multiplier
        ),
        axis.title.x = ggplot2::element_text(
          size = sizes$axis_title,
          lineheight = lineheight_multiplier
        ),
        axis.title.y = ggplot2::element_text(
          size = sizes$axis_title,
          lineheight = lineheight_multiplier
        ),
        axis.text = ggplot2::element_text(
          size = sizes$axis_text,
          lineheight = lineheight_multiplier
        ),
        axis.text.x = axis_text_x,
        axis.text.y = ggplot2::element_text(
          size = sizes$axis_text,
          lineheight = lineheight_multiplier
        ),
        legend.title = ggplot2::element_text(
          size = legend_title_size,
          lineheight = lineheight_multiplier,
          margin = ggplot2::margin(b = 2)
        ),
        legend.text = ggplot2::element_text(
          size = legend_text_size,
          lineheight = lineheight_multiplier
        )
      )
    ee_tag <- attr(plot, "crowding24_ee_families", exact = TRUE)
    if (is.character(ee_tag) && length(ee_tag) > 0) {
      attr(png_plot, "crowding24_ee_families") <- ee_tag
    }
    height_tag <- attr(plot, "plots_display_height_in", exact = TRUE)
    if (is.numeric(height_tag) && length(height_tag) >= 1 && is.finite(height_tag[1])) {
      attr(png_plot, "plots_display_height_in") <- as.numeric(height_tag[1])
    }
    width_tag <- attr(plot, "plots_display_width_in", exact = TRUE)
    if (is.numeric(width_tag) && length(width_tag) >= 1 && is.finite(width_tag[1])) {
      attr(png_plot, "plots_display_width_in") <- as.numeric(width_tag[1])
    }
    attr(png_plot, "crowding24_native_legend_patchwork") <- TRUE
    if (isTRUE(attr(plot, "crowding24_paired_row_patchwork", exact = TRUE))) {
      attr(png_plot, "crowding24_paired_row_patchwork") <- TRUE
    }
    attr(png_plot, "plots_fit_to_content") <- attr(plot, "plots_fit_to_content", exact = TRUE)
    attr(png_plot, "crowding24_row_layout") <- attr(plot, "crowding24_row_layout", exact = TRUE)
    attr(png_plot, "crowding24_main_panel") <- TRUE
    return(png_plot)
  }

  png_plot <- plot

  # Preserve angled x tick labels from the plot theme when present (e.g. font bars)
  existing_x <- png_plot$theme$axis.text.x
  x_angle <- if (!is.null(existing_x) && !is.null(existing_x$angle) && !is.na(existing_x$angle)) existing_x$angle else NULL
  x_hjust <- if (!is.null(existing_x) && !is.null(existing_x$hjust) && !is.na(existing_x$hjust)) existing_x$hjust else NULL
  x_vjust <- if (!is.null(existing_x) && !is.null(existing_x$vjust) && !is.na(existing_x$vjust)) existing_x$vjust else NULL

  axis_text_x <- if (profile == "histogram") {
    if (isTRUE(attr(plot, "ratio_r_hist_pair", exact = TRUE))) {
      ggplot2::element_text(size = sizes$axis_text, angle = 0, hjust = 0.5, vjust = 1)
    } else {
      ggplot2::element_text(size = sizes$axis_text, angle = -40, hjust = 0, vjust = 1)
    }
  } else if (!is.null(x_angle)) {
    ggplot2::element_text(
      size = sizes$axis_text,
      angle = x_angle,
      hjust = if (is.null(x_hjust)) 1 else x_hjust,
      vjust = if (is.null(x_vjust)) 1 else x_vjust
    )
  } else {
    ggplot2::element_text(size = sizes$axis_text)
  }
  png_plot <- png_plot +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = sizes$title, lineheight = lineheight_multiplier),
      plot.subtitle = ggplot2::element_text(size = sizes$subtitle, lineheight = lineheight_multiplier),
      # Set x/y explicitly so plot-level axis.title.x/y cannot keep tiny sizes
      axis.title = ggplot2::element_text(size = sizes$axis_title, lineheight = lineheight_multiplier),
      axis.title.x = ggplot2::element_text(size = sizes$axis_title, lineheight = lineheight_multiplier),
      axis.title.y = ggplot2::element_text(size = sizes$axis_title, lineheight = lineheight_multiplier),
      axis.text = ggplot2::element_text(size = sizes$axis_text, lineheight = lineheight_multiplier),
      axis.text.x = axis_text_x,
      axis.text.y = ggplot2::element_text(size = sizes$axis_text, lineheight = lineheight_multiplier),
      legend.title = ggplot2::element_text(size = sizes$legend_title, lineheight = lineheight_multiplier),
      legend.text = ggplot2::element_text(size = sizes$legend_text, lineheight = lineheight_multiplier),
      strip.text = ggplot2::element_text(size = sizes$strip, lineheight = lineheight_multiplier),
      plot.caption = ggplot2::element_text(size = sizes$caption, lineheight = lineheight_multiplier)
    )

  for (layer_idx in seq_along(png_plot$layers)) {
    geom <- png_plot$layers[[layer_idx]]$geom
    if (inherits(geom, "GeomText") || inherits(geom, "GeomLabel") ||
        inherits(geom, "GeomTextNpc") || inherits(geom, "GeomTextRepel") ||
        inherits(geom, "GeomLabelRepel")) {
      layer_label <- as.character(c(
        png_plot$layers[[layer_idx]]$aes_params$label,
        png_plot$layers[[layer_idx]]$geom_params$label
      ))
      is_stats_layer <- any(grepl(
        "(?i)(\\bN\\s*=|Sessions\\s*=|\\bMean\\s*=|\\bsd\\s*=)",
        layer_label,
        perl = TRUE
      ))

      size <- png_plot$layers[[layer_idx]]$aes_params$size
      if (is.null(size)) size <- png_plot$layers[[layer_idx]]$geom_params$size
      base_size <- if (is_stats_layer) stats_text_layer_size else default_text_layer_size
      size_num <- if (length(size) == 1) suppressWarnings(as.numeric(size)) else NA_real_
      if (!is.na(size_num)) {
        scaled_size <- size_num * text_layer_multiplier
        png_plot$layers[[layer_idx]]$aes_params$size <- scaled_size
        png_plot$layers[[layer_idx]]$geom_params$size <- scaled_size
      } else if (is.null(size)) {
        # Size may be mapped (e.g. aes(size = abbrev_size) for equalized x-heights).
        ld <- png_plot$layers[[layer_idx]]$data
        scaled_mapped <- FALSE
        if (is.data.frame(ld) && "abbrev_size" %in% names(ld)) {
          png_plot$layers[[layer_idx]]$data$abbrev_size <-
            suppressWarnings(as.numeric(ld$abbrev_size)) * text_layer_multiplier
          scaled_mapped <- TRUE
        }
        if (!scaled_mapped) {
          scaled_size <- base_size * text_layer_multiplier
          png_plot$layers[[layer_idx]]$aes_params$size <- scaled_size
          png_plot$layers[[layer_idx]]$geom_params$size <- scaled_size
        }
      }

      lineheight <- png_plot$layers[[layer_idx]]$aes_params$lineheight
      if (is.null(lineheight)) lineheight <- png_plot$layers[[layer_idx]]$geom_params$lineheight
      lineheight_num <- if (length(lineheight) == 1) suppressWarnings(as.numeric(lineheight)) else NA_real_
      if (!is.na(lineheight_num)) {
        scaled_lineheight <- lineheight_num * lineheight_multiplier
        png_plot$layers[[layer_idx]]$aes_params$lineheight <- scaled_lineheight
        png_plot$layers[[layer_idx]]$geom_params$lineheight <- scaled_lineheight
      } else if (is.null(lineheight)) {
        png_plot$layers[[layer_idx]]$aes_params$lineheight <- 0.6
        png_plot$layers[[layer_idx]]$geom_params$lineheight <- 0.6
      }
    }
  }

  png_plot
}

# Prepare a plot the same way on-screen PNGs are prepared.
prepare_plots_display_plot <- function(plot,
                                       use_png_theme = TRUE,
                                       png_theme_profile = "plots",
                                       text_scale = 1,
                                       scale_title = FALSE,
                                       scale_subtitle = FALSE,
                                       scale_axis_title = TRUE,
                                       scale_axis_text = TRUE) {
  if (isTRUE(use_png_theme)) {
    plot <- apply_direct_png_theme(
      plot,
      profile = png_theme_profile,
      text_scale = text_scale,
      scale_title = scale_title,
      scale_subtitle = scale_subtitle,
      scale_axis_title = scale_axis_title,
      scale_axis_text = scale_axis_text
    )
  }
  plot
}

# Plots whose layout is in absolute units carry attr "plots_fit_to_content":
# function(plot, dpi) -> list(plot, width_in, height_in). Called after the PNG
# theme (final text sizes) and font registration, so measurements match the
# drawn figure. Returns NULL for ordinary plots.
fit_plot_to_content <- function(plot, dpi = 200) {
  fit <- attr(plot, "plots_fit_to_content", exact = TRUE)
  if (!is.function(fit)) {
    return(NULL)
  }
  tryCatch(fit(plot, dpi = dpi), error = function(e) {
    log_error("plots_fit_to_content failed: ", conditionMessage(e))
    NULL
  })
}

# Pixel geometry used by on-screen plot PNGs (and matching downloads).
plots_display_png_geometry <- function(width_in, height_in, disp_w = 700) {
  width_in <- as.numeric(width_in)[1]
  height_in <- as.numeric(height_in)[1]
  if (is.na(width_in) || width_in <= 0) width_in <- 6
  if (is.na(height_in) || height_in <= 0) height_in <- width_in
  disp_w <- as.numeric(disp_w)[1]
  if (is.na(disp_w) || disp_w <= 0) disp_w <- 700

  scale <- 2
  png_w <- round(disp_w * scale)
  png_h <- round((height_in / width_in) * png_w)
  list(
    width_in = width_in,
    height_in = height_in,
    disp_w = disp_w,
    scale = scale,
    png_w = png_w,
    png_h = png_h,
    dpi = png_w / width_in
  )
}

# Save a PNG with the exact same theme/device/dpi as the Plots-tab on-screen image.
ggsave_plots_display_png <- function(file,
                                     plot,
                                     width_in,
                                     height_in,
                                     disp_w = 700,
                                     use_png_theme = TRUE,
                                     png_theme_profile = "plots",
                                     text_scale = 1,
                                     scale_title = FALSE,
                                     scale_subtitle = FALSE,
                                     scale_axis_title = TRUE,
                                     scale_axis_text = TRUE,
                                     limitsize = FALSE) {
  geom <- plots_display_png_geometry(width_in, height_in, disp_w)
  plot <- prepare_plots_display_plot(
    plot,
    use_png_theme = use_png_theme,
    png_theme_profile = png_theme_profile,
    text_scale = text_scale,
    scale_title = scale_title,
    scale_subtitle = scale_subtitle,
    scale_axis_title = scale_axis_title,
    scale_axis_text = scale_axis_text
  )

  # PNG theme sizes (e.g. axis_title=28) are calibrated for showtext_auto(TRUE)
  # from emojifont. Turning showtext off makes those sizes ~50% larger on screen.
  # Keep showtext on for all Plots-tab PNGs; register ee_* via sysfonts::font_add.
  if (requireNamespace("showtext", quietly = TRUE)) {
    tryCatch(showtext::showtext_auto(TRUE), error = function(e) invisible(NULL))
  }

  needs_ee_fonts <- FALSE
  if (exists("plot_uses_crowding24_ee_fonts", mode = "function")) {
    needs_ee_fonts <- isTRUE(plot_uses_crowding24_ee_fonts(plot))
  }
  if (needs_ee_fonts && exists("ensure_crowding24_plot_fonts_registered", mode = "function")) {
    ee_fams <- if (exists("crowding24_ee_families_in_plot", mode = "function")) {
      crowding24_ee_families_in_plot(plot)
    } else {
      NULL
    }
    ensure_crowding24_plot_fonts_registered(families = ee_fams)
  }
  on.exit({
    if (needs_ee_fonts && exists("release_crowding24_plot_fonts", mode = "function")) {
      release_crowding24_plot_fonts()
    }
  }, add = TRUE)

  fitted <- fit_plot_to_content(plot, dpi = geom$dpi)
  if (!is.null(fitted)) {
    plot <- fitted$plot
    # Same dpi, so on-screen pixels per inch stay as for other Plots PNGs.
    geom <- plots_display_png_geometry(
      fitted$width_in,
      fitted$height_in,
      disp_w = geom$disp_w * fitted$width_in / geom$width_in
    )
  }

  tryCatch({
    ggplot2::ggsave(
      filename = file,
      plot = plot,
      width = geom$width_in,
      height = geom$height_in,
      units = "in",
      limitsize = limitsize,
      device = ragg::agg_png,
      dpi = geom$dpi
    )
  }, error = function(e) {
    log_error("Direct ragg render failed, falling back to svglite: ", conditionMessage(e))
    # svglite cannot use systemfonts ee_* aliases — fall back without custom faces.
    tmp_svg <- tempfile(fileext = ".svg")
    ggplot2::ggsave(
      filename = tmp_svg,
      plot = plot,
      width = geom$width_in,
      height = geom$height_in,
      units = "in",
      limitsize = limitsize,
      device = svglite::svglite
    )
    rsvg::rsvg_png(tmp_svg, file, width = geom$png_w, height = geom$png_h)
  })

  invisible(geom)
}

# Download helper: PNG matches on-screen render; SVG/PDF share the same theme
# and use a larger canvas (default 140%) so text/layout stay closer to PNG.
save_plots_display_download <- function(file,
                                        plot,
                                        file_type,
                                        width_in,
                                        height_in,
                                        disp_w = 700,
                                        use_png_theme = TRUE,
                                        png_theme_profile = "plots",
                                        text_scale = 1,
                                        scale_title = FALSE,
                                        scale_subtitle = FALSE,
                                        scale_axis_title = TRUE,
                                        scale_axis_text = TRUE,
                                        limitsize = FALSE,
                                        vector_size_scale = 1.4) {
  if (identical(file_type, "png")) {
    ggsave_plots_display_png(
      file = file,
      plot = plot,
      width_in = width_in,
      height_in = height_in,
      disp_w = disp_w,
      use_png_theme = use_png_theme,
      png_theme_profile = png_theme_profile,
      text_scale = text_scale,
      scale_title = scale_title,
      scale_subtitle = scale_subtitle,
      scale_axis_title = scale_axis_title,
      scale_axis_text = scale_axis_text,
      limitsize = limitsize
    )
    return(invisible(NULL))
  }

  vector_size_scale <- as.numeric(vector_size_scale)[1]
  if (is.na(vector_size_scale) || vector_size_scale <= 0) vector_size_scale <- 1

  plot <- prepare_plots_display_plot(
    plot,
    use_png_theme = use_png_theme,
    png_theme_profile = png_theme_profile,
    text_scale = text_scale,
    scale_title = scale_title,
    scale_subtitle = scale_subtitle,
    scale_axis_title = scale_axis_title,
    scale_axis_text = scale_axis_text
  )
  fitted <- fit_plot_to_content(plot)
  if (!is.null(fitted)) {
    # Content is laid out in absolute inches; an enlarged canvas would only
    # add white space around it.
    plot <- fitted$plot
    width_in <- fitted$width_in
    height_in <- fitted$height_in
    vector_size_scale <- 1
  }
  ggplot2::ggsave(
    file = file,
    plot = plot,
    width = width_in * vector_size_scale,
    height = height_in * vector_size_scale,
    unit = "in",
    limitsize = limitsize,
    device = if (identical(file_type, "svg")) svglite::svglite else file_type
  )
  invisible(NULL)
}

# On-screen PNG via ragg (with svglite/rsvg fallback). Returns renderImage list().
render_plots_display_png <- function(plot,
                                     width_in,
                                     height_in,
                                     disp_w = 700,
                                     disp_h = NULL,
                                     use_png_theme = TRUE,
                                     png_theme_profile = "plots",
                                     text_scale = 1,
                                     scale_title = FALSE,
                                     scale_subtitle = FALSE,
                                     scale_axis_title = TRUE,
                                     scale_axis_text = TRUE,
                                     limitsize = FALSE) {
  outfile <- tempfile(fileext = ".png")
  geom <- ggsave_plots_display_png(
    file = outfile,
    plot = plot,
    width_in = width_in,
    height_in = height_in,
    disp_w = disp_w,
    use_png_theme = use_png_theme,
    png_theme_profile = png_theme_profile,
    text_scale = text_scale,
    scale_title = scale_title,
    scale_subtitle = scale_subtitle,
    scale_axis_title = scale_axis_title,
    scale_axis_text = scale_axis_text,
    limitsize = limitsize
  )

  list(
    src = outfile,
    contenttype = "image/png",
    width = geom$disp_w,
    height = if (is.null(disp_h)) round(geom$png_h / geom$scale) else as.numeric(disp_h)[1]
  )
}

# Helper: return a PNG image response for renderImage using ragg
render_png_response <- function(plot, width, height, units = "in", dpi = 144, limitsize = FALSE) {
  outfile <- tempfile(fileext = ".png")
  ggplot2::ggsave(
    filename  = outfile,
    plot      = plot,
    device    = ragg::agg_png,
    width     = width,
    height    = height,
    units     = units,
    dpi       = dpi,
    limitsize = limitsize
  )
  list(src = outfile, contenttype = "image/png")
}

# Enhanced error logging function for debugging
log_detailed_error <- function(e, plot_id = "Unknown Plot") {
  cat("\n=== DETAILED ERROR INFORMATION ===\n")
  cat("Plot ID:", plot_id, "\n")
  cat("Error Message:", e$message, "\n")
  cat("Error Call:", deparse(e$call), "\n")
  if (!is.null(e$trace)) {
    cat("Stack Trace:\n")
    print(e$trace)
  }
  cat("Full Error Object:\n")
  print(e)
  cat("Session Info at time of error:\n")
  print(sessionInfo())
  cat("=== END ERROR INFORMATION ===\n\n")
}

# Word-wrapping helper that never splits words; wraps by max characters per line
wrap_words <- function(text, max_chars) {
  words <- strsplit(text, "\\s+")[[1]]
  lines <- character()
  current <- ""
  for (w in words) {
    if (nchar(current) == 0) {
      current <- w
    } else if (nchar(current) + 1 + nchar(w) <= max_chars) {
      current <- paste(current, w, sep = " ")
    } else {
      lines <- c(lines, current)
      current <- w
    }
  }
  lines <- c(lines, current)
  paste(lines, collapse = "\n")
}

# Enhanced error handler for plot rendering with detailed console logging
handle_plot_error <- function(e, plot_id, experiment_names = NULL, plot_subtitle = "") {
  # Enhanced error logging for debugging - print detailed error info to console
  # log_detailed_error(e, plot_id)
  
  # Create user-friendly error plot
  error_plot <- ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.6,
      label = paste("Error:", e$message),
      color = "red",
      size = 5,
      hjust = 0.5,
      vjust = 0.5
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.4,
      label = paste("Plot ID:", plot_id),
      size = 4,
      fontface = "italic"
    ) +
    plt_theme
  
  # Add title if experiment_names function is provided
  if (!is.null(experiment_names)) {
    tryCatch({
      title_text <- experiment_names
      error_plot <- error_plot + labs(title = title_text, subtitle = plot_subtitle)
    }, error = function(title_error) {
      error_plot <<- error_plot + labs(title = "Error getting experiment names", subtitle = plot_subtitle)
    })
  } else {
    error_plot <- error_plot + labs(subtitle = plot_subtitle)
  }
  
  # Save the error plot to a temp file
  outfile <- tempfile(fileext = '.svg')
  ggsave(
    file = outfile,
    plot = error_plot,
    device = svglite,
    width = 6,
    height = 6
  )
  
  return(list(
    src = outfile,
    contenttype = 'image/svg+xml',
    alt = paste0("Error in ", plot_id, ": ", e$message)
  ))
}

# Class tag for one column vector (used before dplyr::bind_rows across sessions).
column_bind_class <- function(vectors) {
  non_empty <- vectors[vapply(vectors, length, integer(1)) > 0]
  if (length(non_empty) == 0) {
    return("character")
  }

  tags <- unique(vapply(non_empty, function(x) {
    if (is.list(x) && !is.data.frame(x)) {
      return("character")
    }
    if (inherits(x, "POSIXt")) {
      return("POSIXt")
    }
    if (inherits(x, "Date")) {
      return("Date")
    }
    if (is.factor(x)) {
      return("character")
    }
    if (is.logical(x)) {
      return("logical")
    }
    if (is.integer(x)) {
      return("integer")
    }
    if (is.numeric(x)) {
      return("double")
    }
    if (is.character(x)) {
      return("character")
    }
    "character"
  }, character(1)))

  if (length(tags) == 1) {
    return(tags[[1]])
  }
  if (all(tags %in% c("integer", "double"))) {
    return("double")
  }
  "character"
}

coerce_column_to_bind_class <- function(x, target_class) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (length(x) == 0) {
    return(switch(
      target_class,
      character = character(0),
      logical = logical(0),
      integer = integer(0),
      double = double(0),
      Date = structure(numeric(0), class = "Date"),
      POSIXt = structure(numeric(0), class = c("POSIXct", "POSIXt")),
      character(0)
    ))
  }

  switch(
    target_class,
    character = as.character(x),
    logical = as.logical(x),
    integer = suppressWarnings(as.integer(x)),
    double = suppressWarnings(as.numeric(x)),
    Date = suppressWarnings(as.Date(x)),
    POSIXt = suppressWarnings(as.POSIXct(as.character(x), tz = "UTC")),
    as.character(x)
  )
}

harmonize_chunks_for_bind_rows <- function(chunks) {
  all_cols <- unique(unlist(lapply(chunks, names), use.names = FALSE))
  col_targets <- stats::setNames(
    lapply(all_cols, function(col) {
      vectors <- lapply(chunks, function(df) {
        if (col %in% names(df)) df[[col]] else logical(0)
      })
      column_bind_class(vectors)
    }),
    all_cols
  )

  lapply(chunks, function(df) {
    n <- nrow(df)
    for (col in all_cols) {
      target <- col_targets[[col]]
      if (col %in% names(df)) {
        df[[col]] <- coerce_column_to_bind_class(df[[col]], target)
      } else if (n == 0) {
        df[[col]] <- coerce_column_to_bind_class(logical(0), target)
      } else {
        df[[col]] <- coerce_column_to_bind_class(rep(NA, n), target)
      }
    }
    df[, all_cols, drop = FALSE]
  })
}

# Bind session chunks with compatible column types (character/logical mixes, etc.).
bind_rows_or_empty <- function(chunks) {
  chunks <- chunks[!vapply(chunks, is.null, logical(1))]
  chunks <- chunks[vapply(chunks, is.data.frame, logical(1))]
  if (length(chunks) == 0) {
    return(tibble::tibble())
  }
  dplyr::bind_rows(harmonize_chunks_for_bind_rows(chunks))
}

# Progressive plot unlock for renderImage/renderPlot.
#
# Do NOT use `req(ii <= unlocked_rv())` in image renderers: each counter
# increment invalidates every earlier slot (O(n^2) re-renders / looks like an
# infinite loop). Poll with isolate + invalidateLater instead. Optional
# `generation` (reactive or zero-arg function) re-triggers after a gate reset.
req_progressive_slot_unlocked <- function(ii, unlocked_rv, session, generation = NULL) {
  if (!is.null(generation)) {
    if (is.function(generation)) {
      invisible(generation())
    }
  }
  if (isolate(unlocked_rv()) < ii) {
    shiny::invalidateLater(50, session)
    shiny::req(FALSE)
  }
  invisible(NULL)
}
