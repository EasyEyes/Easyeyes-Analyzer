
callback_2 <- "function MergeGridCells() {
    var dimension_cells = new Array();
    var dimension_col = null;
    var columnCount = table.length;
for (dimension_col = 0; dimension_col <= columnCount; dimension_col++) {
  // first_instance holds the first instance of identical td
  var first_instance = null;
  var rowspan = 1;
  // iterate through rows
  table.find('tr').each(function () {
    
    // find the td of the correct column (determined by the dimension_col set above)
    var dimension_td = $(this).find('td:nth-child(' + dimension_col + ')');
    
    
    
    if (first_instance === null) {
      // must be the first row
      first_instance = dimension_td;
    } else if (dimension_td.text() === first_instance.text()) {
      // the current td is identical to the previous
      // remove the current td
      // dimension_td.remove();
      dimension_td.attr('hidden', true);
      ++rowspan;
      // increment the rowspan attribute of the first instance
      first_instance.attr('rowspan', rowspan);
    } else {
      // this cell is different from the last
      first_instance = dimension_td;
      rowspan = 1;
    }
  });
}
}
MergeGridCells();
"

rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"
)

library(ggplot2)

#### black and grey color scale ####

color_scale <- function(n) {
  scale_color_manual(
    values = grDevices::colorRampPalette(c("gray", "black"))(n),
    guide = guide_legend(title = "Grade")
  )
}


##### ggplot display theme #####
plt_theme_scatter <- theme(
  legend.position = "top", 
  legend.box = "vertical", 
  legend.key.size = unit(4.5, "mm"),
  legend.title = element_text(size=14,hjust = 0),
  legend.text = element_text(size=14),
  legend.box.margin = margin(0,0,0,0,"cm"),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_blank(), 
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 14),
  axis.line = element_line(colour = "black"),
  plot.title = element_text(size=18,
                            hjust = 0),
  plot.title.position = "plot",
  plot.subtitle = element_text(size=18),
  plot.caption = element_text(size=10, hjust = 0),
  plot.margin = margin(
    t = 0.1,
    r = 0.9,
    b = 0.1,
    l = 0.1,
    "inch"
  ),
  strip.text = element_text(size = 14))

colorPalette <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
  "#F781BF", "#A65628", "#17BECF", "#999999", "#B2182B",
  "#2166AC", "#1B7837", "#D6604D", "#4393C3", "#D95F02",
  "#8C510A", "#762A83", "#D9D9D9", "#8DD3C7", "#BEBADA",
  "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5",
  "#D9D9D9", "#BC80BD", "#CCEBC5", "#7FC97F", "#BEAED4",
  "#FDC086", "#386CB0", "#F0027F", "#BF5B17", "#666666",
  "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#6A3D9A",
  "#B15928", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
  "#FB9A99", "#E31A1C", "#FF7F00", "#CAB2D6", "#6A3D9A",
  "#FF7F7F", "#7FFF7F", "#7F7FFF", "#B03060", "#7FFFD4",
  "#6B8E23", "#8A2BE2", "#DC143C", "#4169E1", "#6495ED",
  "#228B22", "#FF6347", "#2E8B57", "#FFA07A", "#20B2AA",
  "#87CEEB", "#FF4500", "#800080", "#ADFF2F", "#7CFC00",
  "#FF69B4", "#B0E0E6", "#48D1CC", "#E6E6FA", "#40E0D0",
  "#F08080", "#EE82EE", "#8B0000", "#DEB887", "#D8BFD8",
  "#FA8072", "#FFE4E1", "#00CED1", "#4682B4", "#C71585",
  "#708090", "#FF1493", "#191970", "#00BFFF", "#7B68EE",
  "#87CEFA", "#A52A2A", "#556B2F", "#9932CC", "#8B4513",
  "#2F4F4F", "#8A2BE2", "#CD5C5C", "#DC143C", "#FF4500",
  "#1E90FF", "#DA70D6", "#8FBC8F", "#800000", "#483D8B",
  "#2E8B57", "#D2691E", "#5F9EA0", "#DDA0DD", "#B22222",
  "#FF00FF", "#4B0082", "#696969", "#FF1493", "#556B2F",
  "#B0C4DE", "#BA55D3", "#8B008B", "#FA8072", "#20B2AA"
)

scale_color <- function(list) {
  if (length(list) <= 120) {
    scale_color_manual(values = colorPalette)
  } else {
    extendedPalette <- rep(colorPalette, length.out = length(list))
    scale_color_manual(values = extendedPalette)
  }
}


plt_theme <- theme(legend.position = "top", 
                   legend.box = "vertical", 
                   legend.justification='left',
                   legend.key.size = unit(4.5, "mm"),
                   legend.title = element_text(size=14, hjust = 0),
                   legend.text = element_text(size=14, hjust = 0),
                   legend.box.margin = margin(0,0,0,0,"cm"),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.title = element_text(size = 14),
                   axis.text = element_text(size = 14),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=18,
                                             hjust = 0),
                   plot.title.position = "plot",
                   plot.subtitle = element_text(size=18,
                                                hjust = 0),
                   plot.caption = element_text(size=10, hjust = 0),
                   plot.margin = margin(
                     t = 0.1,
                     r = 0.1,
                     b = 0.1,
                     l = 0.1,
                     "inch"
                   ),
                   strip.text = element_text(size = 14))

plt_theme_ggiraph <- theme(legend.position = "top", 
                   legend.box = "vertical", 
                   legend.justification='left',
                   legend.key.size = unit(4.5, "mm"),
                   legend.title = element_text(size=10, hjust = 0),
                   legend.text = element_text(size=10, hjust = 0),
                   legend.box.margin = margin(0,0,0,0,"cm"),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.title = element_text(size = 10),
                   axis.text = element_text(size = 10),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=12,
                                             hjust = 0),
                   plot.title.position = "plot",
                   plot.subtitle = element_text(size=12,
                                                hjust = 0),
                   plot.caption = element_text(size=10, hjust = 0),
                   plot.margin = margin(
                     t = 0.1,
                     r = 0.1,
                     b = 0.1,
                     l = 0.1,
                     "inch"
                   ),
                   strip.text = element_text(size = 10))

hist_theme <- theme(legend.position = "top", 
                   legend.box = "vertical", 
                   legend.justification='left',
                   legend.key.size = unit(4.5, "mm"),
                   legend.title = element_text(size=14),
                   legend.text = element_text(size=14),
                   legend.box.margin = margin(0,0,0,0,"cm"),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.title = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   axis.line = element_line(colour = "black"),
                   axis.text.x = element_text(size  = 10, angle = -40, hjust=0, vjust=1),
                   axis.text.y = element_text(size  = 10),
                   plot.title = element_text(size=14,
                                             hjust = 0),
                   plot.title.position = "plot",
                   plot.subtitle = element_text(size=12,
                                                hjust = 0),
                   plot.caption = element_text(size=10, hjust = 0),
                   plot.margin = margin(
                     t = 0.1,
                     r = 0.5,
                     b = 0.1,
                     l = 0.1,
                     "inch"
                   ),
                   strip.text = element_text(size = 14))

stacked_theme <- theme(
  axis.text.x = element_text(),
  axis.ticks.x = element_line(),
  legend.position = "top",
  legend.key.size = unit(2, "mm"),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8),
  axis.text = element_text(size = 11),
  plot.title = element_text(size = 12, margin = margin(b = 2)),
  plot.caption = element_text(size=10, hjust = 0),
  plot.margin = margin(5, 5, 5, 5, "pt")
)





# instruction <- "All rows belonging to the same Pavlovia session (a CSV results file) have the same pastel shade.  Each row is a session event: ending, error, or warning. The right side of each column’s heading has buttons to sort up and down, and below is a text box for selection."
instruction <- "Each row is one Pavlovia session. All rows belonging to the same Prolific session have the same pastel shade. One Prolific session includes several Pavlovia sessions when the participant doesn’t finish and tries again."

scale_family <- function(...) {
  nfamily <- length(quartzFonts())
  namesfamily <- names(quartzFonts())
  discrete_scale("family", "family_d", function(n)namesfamily[(1:n)%%nfamily+1])
}

# Helper function to add experiment name as footnote to plots
add_experiment_footnote <- function(plot, experiment_name) {
  if (is.null(plot) || is.null(experiment_name) || experiment_name == "") {
    return(plot)
  }
  
  # Add experiment name as caption (footnote) to the plot
  plot <- plot + labs(caption = experiment_name)
  
  return(plot)
}

# eq1_text <- "\\(totalDBSPL = 10 \\text{log}_{10}(10 ^ {\\frac{backgroundDbSpl}{10}} + 10 ^ {\\frac{(gainDbSpl + inDb)}{10}}) \\\\  
# outDB = \\begin{cases} T + \\frac{(totalDBSPL - T)}{R} & \\text{if} \\ \\ totalDBSPL > T + \\frac{W}{2} \\\\
# totalDBSPL + \\frac{(1-R)}{R}\\frac{(totalDBSPL - (T-\\frac{W}{2}))^2}{2W} & \\text{if} \\ \\ T - \\frac{W}{2} < totalDBSPL \\leq T + \\frac{W}{2} \\\\ 
# totalDBSPL & \\text{if} \\ \\ totalDBSPL \\leq T - \\frac{W}{2} \\end{cases}\\)"


eq1_text <- "\\(compressed = \\begin{cases} T + Q(in - T) & \\text{if} \\ \\ in > T + \\frac{W}{2} \\\\
in + (1-Q)\\frac{(in - (T-\\frac{W}{2}))^2}{2W} & \\text{if} \\ \\ T - \\frac{W}{2} < in \\leq T + \\frac{W}{2} \\\\ 
in & \\text{if} \\ \\ in \\leq T - \\frac{W}{2} \\end{cases}\\\\out = compressed + gain\\)"

reference <- "http://eecs.qmul.ac.uk/~josh/documents/2012/GiannoulisMassbergReiss-dynamicrangecompression-JAES2012.pdf"