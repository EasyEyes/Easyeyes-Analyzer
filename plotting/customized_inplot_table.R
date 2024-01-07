library(gridExtra)
library(grid)
library(gtable)
geom_table_costumized <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           table.theme = NULL,
                           table.rownames = FALSE,
                           table.colnames = TRUE,
                           table.hjust = 0.5,
                           parse = FALSE,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = FALSE) {
  
  if (is.character(table.hjust)) {
    table.hjust <- switch(table.hjust,
                          left = 0,
                          middle = 0.5,
                          center = 0.5,
                          right = 1,
                          0.5)
  }
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTableNpcNew,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      table.theme = table.theme,
      table.rownames = table.rownames,
      table.colnames = table.colnames,
      table.hjust = table.hjust,
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTableNpcNew <-
  ggplot2::ggproto("GeomTableNpc", ggplot2::Geom,
                   required_aes = c("npcx", "npcy", "label","text", "title_text","subtitle","leftShift", "fs","shrinkPadding","titleFont"),
                   
                   default_aes = ggplot2::aes(
                     colour = NA,
                     fill = NA,
                     size = 3.2,
                     angle = 0,
                     hjust = "inward",
                     vjust = "inward",
                     alpha = 1,
                     family = "",
                     fontface = 1,
                     lineheight = 1.2
                   ),
                   
                   draw_panel =
                     function(data,
                              panel_params,
                              coord,
                              table.theme = NULL,
                              table.rownames = FALSE,
                              table.colnames = TRUE,
                              table.hjust = 0,
                              parse = FALSE,
                              na.rm = FALSE) {
                       
                       if (nrow(data) == 0) {
                         return(grid::nullGrob())
                       }
                       
                       if (!is.data.frame(data$label[[1]])) {
                         warning("Skipping as object mapped to 'label' is not a list of ",
                                 "\"tibble\" or \"data.frame\" objects.")
                         return(grid::nullGrob())
                       }
                       
                       data$npcx <- compute_npcx(data$npcx, margin.npc = 0)-data$leftShift
                       data$npcy <- compute_npcy(data$npcy, margin.npc = 0)
                       
                       if (is.character(data$vjust)) {
                         data$vjust <- compute_just(data$vjust, data$npcy)
                       }
                       if (is.character(data$hjust)) {
                         data$hjust <- compute_just(data$hjust, data$npcx)
                       }
                       
                       # replace NULL with default
                       if (is.null(table.theme)) {
                         table.theme <-
                           getOption("ggpmisc.ttheme.default", default = ttheme_gtdefault)
                       }
                       
                       tb.grobs <- grid::gList()
                       
                       for (row.idx in seq_len(nrow(data))) {
                         # if needed, construct the table theme
                         if (is.function(table.theme)) {
                           # text position in cell depends on hjust
                           table.x <- if(table.hjust == 0.5) 0.5 else table.hjust * 0.8 + 0.1
                           if (is.na(data$fill[row.idx])) {
                             core.params <-
                               list(fg_params = list(hjust = 0, x = 0.1))
                           } else {
                             core.params <-
                               list(fg_params = list(hjust = 0, x = 0.1),
                                    bg_params = list(fill = data$fill[row.idx]))
                           }
                           if (is.na(data$colour[row.idx])) {
                             # use theme's default base_colour
                             this.table.theme <-
                               table.theme(base_size = data$size[row.idx] * .pt,
                                           base_family = data$family[[row.idx]],
                                           parse = parse,
                                           rowhead = list(fg_params = list(hjust = 0, x = 0.1)),
                                           colhead = list(fg_params = list(hjust = 0,
                                                                           x = 0.1)),
                                           core = core.params)
                           } else {
                             # use colour from data$colour
                             this.table.theme <-
                               table.theme(base_size = data$size[row.idx] * .pt,
                                           base_colour = ggplot2::alpha(data$colour[row.idx],
                                                                        data$alpha[row.idx]),
                                           base_family = data$family[[row.idx]],
                                           parse = parse,
                                           rowhead = list(fg_params = list(hjust = 0, x = 0.1)),
                                           colhead = list(fg_params = list(hjust = 0,
                                                                           x = 0.1)),
                                           core = core.params)
                           }
                         } else if (is.list(table.theme)) {
                           this.table.theme <- table.theme
                         }
                         table.tb <- data[["label"]][[row.idx]]
                         gtb <-
                           gridExtra::tableGrob(
                             d = table.tb,
                             theme = this.table.theme,
                             rows = if (table.rownames) rownames(table.tb) else NULL,
                             cols = if (table.colnames) colnames(table.tb) else NULL
                           )
                         
                         ## my customized code
                         shrinkPadding <- data[["shrinkPadding"]][[1]]
                    
                         fs = data[["fs"]][[1]]
                         title_text <- gridtext::richtext_grob(data[["title_text"]], x=0.03, hjust=0, gp=gpar(fontsize=fs, fontface = data[["titleFont"]][[1]]))
                         row_to_add <- list()
                         if (data[["title_text"]][[1]] != "") {
                           row_to_add <- list(title_text)
                           gtb <- gtable_add_rows(gtb, 
                                                  heights = grobHeight(title_text) + unit(0.2,"line"),
                                                  pos = 0)
                           index = 2
                         } else {
                           index = 1
                         }
                         subtitles <- unlist(data[["subtitle"]][[1]])
                         for (i in 1: length(subtitles)) {
                           tmp <- gridtext::richtext_grob(subtitles[i], x=0.03, hjust=0, gp=gpar(fontsize=fs))
                           row_to_add[[index]] <- tmp
                           index = index + 1
                           gtb <- gtable_add_rows(gtb, 
                                                  heights = grobHeight(tmp) + unit(0.2,"line"))
                         }
                         
                         footnote <- textGrob(data[["text"]], x=0.03, hjust=0, gp=gpar(fontsize=fs))
                         
                         gtb <- gtable_add_rows(gtb, 
                                                heights = grobHeight(footnote)+ unit(0.2,"line"))
                         row_to_add[[index]] <- footnote
                         index = index + 1
                         
                         if (data[["title_text"]][[1]] != "") {
                           gtb <- gtable_add_grob(gtb, grobs = row_to_add,
                                                  t=c(1, (nrow(gtb) - length(row_to_add) + 2) : nrow(gtb)), 
                                                  l=rep(1,length(row_to_add)), 
                                                  r=ncol(gtb))
                         } else {
                           gtb <- gtable_add_grob(gtb, grobs = row_to_add,
                                                  t=(nrow(gtb) - length(row_to_add) + 1) : nrow(gtb), 
                                                  l=rep(1,length(row_to_add)), 
                                                  r=ncol(gtb))
                         }
                         
                         gtb$vp <-
                           grid::viewport(x = grid::unit(data$npcx[row.idx], "native"),
                                          y = grid::unit(data$npcy[row.idx], "native"),
                                          width = sum(gtb$widths),
                                          height = sum(gtb$heights),
                                          just = c(data$hjust[row.idx], data$vjust[row.idx]),
                                          angle = data$angle[row.idx],
                                          name = paste("geom_table.panel", data$PANEL[row.idx],
                                                       "row", row.idx, sep = "."))
                         
                         # give unique name to each table
                         gtb$name <- paste("table", row.idx, sep = ".")
                         
                         tb.grobs[[row.idx]] <- gtb
                       }
                       
                       grid::grobTree(children = tb.grobs)
                     },
                   
                   draw_key = function(...) {
                     grid::nullGrob()
                   }
  )
compute_just <- function(just, a, b = a, angle = 0) {
  #  As justification direction is relative to the text, not the plotting area
  #  we need to swap x and y if text direction is rotated so that hjust is
  #  applied along y and vjust along x.
  if (any(grepl("outward|inward", just))) {
    # ensure all angles are in -360...+360
    angle <- angle %% 360
    # ensure correct behaviour for angles in -360...+360
    angle <- ifelse(angle > 180, angle - 360, angle)
    angle <- ifelse(angle < -180, angle + 360, angle)
    rotated_forward <-
      grepl("outward|inward", just) & (angle > 45 & angle < 135)
    rotated_backwards <-
      grepl("outward|inward", just) & (angle < -45 & angle > -135)
    
    ab <- ifelse(rotated_forward | rotated_backwards, b, a)
    just_swap <- rotated_backwards | abs(angle) > 135
    inward <-
      (just == "inward" & !just_swap | just == "outward" & just_swap)
    just[inward] <- c("left", "middle", "right")[just_dir(ab[inward])]
    outward <-
      (just == "outward" & !just_swap) | (just == "inward" & just_swap)
    just[outward] <- c("right", "middle", "left")[just_dir(ab[outward])]
    
  }
  
  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001, split_at = 0.5) {
  out <- rep(2L, length(x))
  out[x < split_at - tol] <- 1L
  out[x > split_at + tol] <- 3L
  out
}
