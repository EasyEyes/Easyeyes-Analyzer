# plotting/participant_styles.R
#
# Maximally-separated visual styles for participant lines (color + optional mixed color).
# Same participant gets the same style across plots via participant_style_map().

# ── Color helpers (base R) ────────────────────────────────────────────────────

hex_to_lab <- function(hexes) {
  rgb_mat <- t(grDevices::col2rgb(hexes)) / 255
  grDevices::convertColor(rgb_mat, from = "sRGB", to = "Lab")
}

hex_to_lch <- function(hexes) {
  lab <- hex_to_lab(hexes)
  L <- lab[, 1]
  C <- sqrt(lab[, 2]^2 + lab[, 3]^2)
  H <- (atan2(lab[, 3], lab[, 2]) * 180 / pi) %% 360
  cbind(L = L, C = C, H = H)
}

# ── Step 1 — Initial color pool ──────────────────────────────────────────────

STYLE_INITIAL_POOL <- c(
  "#E41A1C", "#FF7F00", "#FFD700", "#4DAF4A",
  "#17BECF", "#377EB8", "#984EA3", "#F781BF",
  "#8B0000", "#8B4513", "#556B2F", "#006400",
  "#008B8B", "#191970", "#4B0082", "#800080",
  "#B2182B", "#D95F02", "#B8860B", "#2E8B57",
  "#20B2AA", "#4682B4", "#6A3D9A", "#C71585",
  "#FF6347", "#FFA07A", "#708090", "#6B8E23",
  "#32CD32", "#6495ED", "#BA55D3", "#FF69B4",
  "#A65628", "#DDA0DD", "#1B7837", "#ADFF2F",
  "#5F9EA0", "#4169E1", "#8A2BE2", "#FF1493",
  "#666666", "#999999",
  "#00CED1", "#1E90FF", "#FF4500", "#9932CC",
  "#00BFFF", "#CD5C5C", "#2F4F4F", "#D2691E"
)

build_distinct_palette <- function(n_target = 40,
                                   initial_pool = STYLE_INITIAL_POOL,
                                   extra_pool   = NULL) {
  pool <- unique(c(initial_pool, extra_pool))
  if (length(pool) <= n_target) return(pool)
  lab  <- hex_to_lab(pool)
  de   <- as.matrix(stats::dist(lab))
  rownames(de) <- colnames(de) <- pool
  lch   <- hex_to_lch(pool)
  first <- which.max(lch[, 2])
  selected       <- integer(n_target)
  selected[1]    <- first
  min_dist       <- de[first, ]
  for (k in 2:n_target) {
    min_dist[selected[seq_len(k - 1)]] <- -Inf
    best        <- which.max(min_dist)
    selected[k] <- best
    min_dist    <- pmin(min_dist, de[best, ])
  }
  pool[selected]
}

# ── Step 2 — Single line width (no thickness separation) ──────────────────────

STYLE_LWD_LEVELS  <- c(0.75)
STYLE_SIZE_LEVELS <- c(3)

# ── Step 3 — Complementary pairs ────────────────────────────────────────────

find_complementary_pairs <- function(palette,
                                     min_hue_sep = 140,
                                     min_delta_e = 55) {
  lch <- hex_to_lch(palette)
  lab <- hex_to_lab(palette)
  de  <- as.matrix(stats::dist(lab))
  n   <- length(palette)
  pairs <- list()
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      hue_diff <- abs(lch[i, 3] - lch[j, 3])
      if (hue_diff > 180) hue_diff <- 360 - hue_diff
      if (hue_diff >= min_hue_sep && de[i, j] >= min_delta_e) {
        pairs[[length(pairs) + 1]] <- c(i, j)
      }
    }
  }
  pairs
}

# ── Step 4 — Candidate pool ─────────────────────────────────────────────────

generate_style_candidates <- function(palette, lwd_levels, comp_pairs) {
  n_single <- length(palette) * length(lwd_levels)
  n_mixed  <- length(comp_pairs) * length(lwd_levels)
  n_total  <- n_single + n_mixed
  color1   <- character(n_total)
  color2   <- character(n_total)
  lwd      <- numeric(n_total)
  is_mixed <- logical(n_total)
  idx <- 0L
  for (ci in seq_along(palette)) {
    for (lv in lwd_levels) {
      idx           <- idx + 1L
      color1[idx]   <- palette[ci]
      color2[idx]   <- NA_character_
      lwd[idx]      <- lv
      is_mixed[idx] <- FALSE
    }
  }
  for (pair in comp_pairs) {
    for (lv in lwd_levels) {
      idx           <- idx + 1L
      color1[idx]   <- palette[pair[1]]
      color2[idx]   <- palette[pair[2]]
      lwd[idx]      <- lv
      is_mixed[idx] <- TRUE
    }
  }
  data.frame(color1 = color1, color2 = color2, lwd = lwd, is_mixed = is_mixed,
             stringsAsFactors = FALSE)
}

# ── Step 5 — Visual distance (color + mixed only) ────────────────────────────

style_visual_distance <- function(i, j, candidates, color_de, lwd_levels,
                                  w_color = 0.80,
                                  w_mixed = 0.20,
                                  w_thick = 0.00) {
  MAX_DE <- 130
  s1 <- candidates[i, ]
  s2 <- candidates[j, ]
  if (!s1$is_mixed && !s2$is_mixed) {
    color_dist <- color_de[s1$color1, s2$color1] / MAX_DE
  } else if (s1$is_mixed && !s2$is_mixed) {
    color_dist <- min(color_de[s1$color1, s2$color1],
                      color_de[s1$color2, s2$color1]) / MAX_DE
  } else if (!s1$is_mixed && s2$is_mixed) {
    color_dist <- min(color_de[s1$color1, s2$color1],
                      color_de[s1$color1, s2$color2]) / MAX_DE
  } else {
    color_dist <- min(color_de[s1$color1, s2$color1],
                      color_de[s1$color1, s2$color2],
                      color_de[s1$color2, s2$color1],
                      color_de[s1$color2, s2$color2]) / MAX_DE
  }
  mixed_bonus <- 0
  if (s1$is_mixed != s2$is_mixed)      mixed_bonus <- 1.0
  else if (s1$is_mixed && s2$is_mixed)  mixed_bonus <- 0.3
  lwd_step   <- abs(match(s1$lwd, lwd_levels) - match(s2$lwd, lwd_levels))
  thick_dist <- if (lwd_step == 0) 0 else if (lwd_step == 1) 0.35 else 1.0
  w_color * color_dist + w_mixed * mixed_bonus + w_thick * thick_dist
}

precompute_style_distances <- function(candidates, color_de, lwd_levels) {
  nc <- nrow(candidates)
  D  <- matrix(0, nc, nc)
  for (i in seq_len(nc - 1)) {
    for (j in (i + 1):nc) {
      D[i, j] <- D[j, i] <- style_visual_distance(
        i, j, candidates, color_de, lwd_levels
      )
    }
  }
  D
}

# ── Step 6 — Farthest-first selection ────────────────────────────────────────

select_farthest_styles <- function(style_D, n, candidates, lwd_levels) {
  nc <- nrow(style_D)
  if (nc <= n) return(seq_len(nc))
  mid_lwd   <- lwd_levels[ceiling(length(lwd_levels) / 2)]
  first_idx <- which(!candidates$is_mixed & candidates$lwd == mid_lwd)[1]
  if (is.na(first_idx)) first_idx <- 1L
  selected    <- integer(n)
  selected[1] <- first_idx
  min_dist    <- style_D[first_idx, ]
  for (k in 2:n) {
    min_dist[selected[seq_len(k - 1)]] <- -Inf
    best        <- which.max(min_dist)
    selected[k] <- best
    min_dist    <- pmin(min_dist, style_D[best, ])
  }
  selected
}

# ── Step 7 — Main entry ──────────────────────────────────────────────────────

participant_style_map <- function(participants, extra_colors = NULL) {
  participants <- as.character(participants)
  participants <- participants[!is.na(participants) &
                               participants != "" &
                               participants != "NA"]
  participants <- sort(unique(participants))
  n <- length(participants)
  empty <- list(
    style_table = data.frame(participant = character(),
                             color1 = character(), color2 = character(),
                             lwd = numeric(), is_mixed = logical(),
                             stringsAsFactors = FALSE),
    colors   = stats::setNames(character(), character()),
    lwd_map  = stats::setNames(numeric(),   character()),
    size_map = stats::setNames(numeric(),   character())
  )
  if (n == 0) return(empty)
  if (is.null(extra_colors) && exists("colorPalette", inherits = TRUE)) {
    extra_colors <- get("colorPalette", inherits = TRUE)
  }
  n_palette  <- max(n, 20)
  pool_size  <- length(unique(c(STYLE_INITIAL_POOL, extra_colors)))
  palette    <- build_distinct_palette(
    n_target     = min(n_palette, pool_size),
    extra_pool   = extra_colors
  )
  lwd_levels <- STYLE_LWD_LEVELS
  comp_pairs <- find_complementary_pairs(palette)
  candidates <- generate_style_candidates(palette, lwd_levels, comp_pairs)
  all_colors <- unique(c(candidates$color1, stats::na.omit(candidates$color2)))
  lab_mat    <- hex_to_lab(all_colors)
  color_de   <- as.matrix(stats::dist(lab_mat))
  rownames(color_de) <- colnames(color_de) <- all_colors
  style_D  <- precompute_style_distances(candidates, color_de, lwd_levels)
  sel_idx  <- select_farthest_styles(style_D, min(n, nrow(candidates)),
                                     candidates, lwd_levels)
  if (length(sel_idx) < n) sel_idx <- rep_len(sel_idx, n)
  selected <- candidates[sel_idx, ]
  lwd_to_size <- stats::setNames(STYLE_SIZE_LEVELS, STYLE_LWD_LEVELS)
  style_table <- data.frame(
    participant = participants,
    color1      = selected$color1,
    color2      = selected$color2,
    lwd         = selected$lwd,
    is_mixed    = selected$is_mixed,
    stringsAsFactors = FALSE
  )
  list(
    style_table = style_table,
    colors   = stats::setNames(style_table$color1, participants),
    lwd_map  = stats::setNames(style_table$lwd,    participants),
    size_map = stats::setNames(lwd_to_size[as.character(style_table$lwd)],
                               participants)
  )
}

participant_style_max_count <- function(extra_colors = NULL, max_palette = 80L) {
  if (is.null(extra_colors) && exists("colorPalette", inherits = TRUE)) {
    extra_colors <- get("colorPalette", inherits = TRUE)
  }
  pool      <- unique(c(STYLE_INITIAL_POOL, extra_colors))
  n_palette <- min(max_palette, length(pool))
  palette   <- build_distinct_palette(n_target = n_palette, extra_pool = extra_colors)
  comp_pairs <- find_complementary_pairs(palette)
  n_single  <- length(palette) * length(STYLE_LWD_LEVELS)
  n_mixed   <- length(comp_pairs) * length(STYLE_LWD_LEVELS)
  as.integer(n_single + n_mixed)
}

# ── Step 8 — Mixed-color rendering + legend key ───────────────────────────────

MIXED_LTY_PATTERN <- "44"

make_mixed_key_glyph <- function(style_table) {
  color2_lookup <- stats::setNames(style_table$color2, style_table$color1)
  mixed_lookup  <- stats::setNames(style_table$is_mixed, style_table$color1)
  function(data, params, size) {
    col1 <- data$colour
    lwd  <- (data$linewidth %||% 0.5) * ggplot2::.pt
    a    <- data$alpha %||% 1
    le   <- params$lineend %||% "butt"
    base_gp <- grid::gpar(
      col = grDevices::adjustcolor(col1, alpha.f = a),
      lwd = lwd, lty = 1, lineend = le
    )
    line1 <- grid::segmentsGrob(0.1, 0.5, 0.9, 0.5, gp = base_gp)
    is_mix <- isTRUE(mixed_lookup[[col1]])
    if (is_mix && !is.na(color2_lookup[[col1]])) {
      col2 <- color2_lookup[[col1]]
      dash_gp <- grid::gpar(
        col = grDevices::adjustcolor(col2, alpha.f = a),
        lwd = lwd, lty = MIXED_LTY_PATTERN, lineend = le
      )
      line2 <- grid::segmentsGrob(0.1, 0.5, 0.9, 0.5, gp = dash_gp)
      grid::grobTree(line1, line2)
    } else {
      line1
    }
  }
}

add_mixed_color_lines <- function(p, data, style_map,
                                  x_var = "x", y_var = "y",
                                  group_var = NULL,
                                  lty_pattern = MIXED_LTY_PATTERN,
                                  alpha = 0.8) {
  st    <- style_map$style_table
  mixed <- st[st$is_mixed, , drop = FALSE]
  if (nrow(mixed) == 0) return(p)
  for (r in seq_len(nrow(mixed))) {
    pid <- mixed$participant[r]
    d   <- data[data[["participant"]] == pid, , drop = FALSE]
    if (nrow(d) == 0) next
    mapping <- if (!is.null(group_var)) {
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]],
                   group = .data[[group_var]])
    } else {
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
    }
    p <- p +
      ggplot2::geom_path(
        data        = d,
        mapping     = mapping,
        color       = mixed$color2[r],
        linewidth   = mixed$lwd[r],
        linetype    = lty_pattern,
        alpha       = alpha,
        show.legend = FALSE,
        inherit.aes = FALSE
      )
  }
  p
}

apply_participant_scales <- function(style_map, drop = FALSE) {
  list(
    ggplot2::scale_color_manual(values = style_map$colors, drop = drop),
    ggplot2::scale_linewidth_manual(values = style_map$lwd_map, drop = drop)
  )
}

validate_styles <- function(style_map, min_separation = 0.10) {
  st <- style_map$style_table
  n  <- nrow(st)
  if (n <= 1) return(invisible(TRUE))
  all_colors <- unique(c(st$color1, stats::na.omit(st$color2)))
  lab_mat    <- hex_to_lab(all_colors)
  color_de   <- as.matrix(stats::dist(lab_mat))
  rownames(color_de) <- colnames(color_de) <- all_colors
  lwd_levels <- STYLE_LWD_LEVELS
  worst_d    <- Inf
  worst_pair <- c(NA_integer_, NA_integer_)
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      d <- style_visual_distance(i, j, st, color_de, lwd_levels)
      if (d < worst_d) {
        worst_d    <- d
        worst_pair <- c(i, j)
      }
    }
  }
  if (worst_d < min_separation) {
    warning(sprintf(
      "Style validation: min separation %.3f < threshold %.3f  (%s vs %s)",
      worst_d, min_separation,
      st$participant[worst_pair[1]], st$participant[worst_pair[2]]
    ))
  }
  invisible(TRUE)
}
