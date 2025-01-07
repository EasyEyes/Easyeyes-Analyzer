
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
##### ggplot download theme #####
downloadtheme <- theme(legend.position = "right", 
                       legend.box = "vertical", 
                       legend.justification = c(1,1),
                       legend.margin = margin(-0.4),
                       legend.key.size = unit(4.5, "mm"),
                       legend.title = element_text(size=20),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       axis.title = element_text(size = 20),
                       axis.text = element_text(size = 20),
                       axis.line = element_line(colour = "black"),
                       plot.title = element_text(size=20),
                       plot.subtitle = element_text(size=20),
                       strip.text = element_text(size = 20))
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
  legend.title = element_text(size=14),
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
  plot.margin = margin(
    t = 0.1,
    r = 0.9,
    b = 0.1,
    l = 0.1,
    "inch"
  ),
  strip.text = element_text(size = 14))

plt_theme <- theme(legend.position = "top", 
                   legend.box = "vertical", 
                   legend.justification='left',
                   legend.key.size = unit(4.5, "mm"),
                   legend.title = element_text(size=14),
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
                   plot.subtitle = element_text(size=18,
                                                hjust = 0),
                   plot.margin = margin(
                     t = 0.1,
                     r = 0.1,
                     b = 0.1,
                     l = 0.1,
                     "inch"
                   ),
                   strip.text = element_text(size = 14))

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
                   axis.title = element_text(size = 14),
                   axis.text = element_text(size = 14),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=14,
                                             hjust = 0),
                   plot.title.position = "plot",
                   plot.subtitle = element_text(size=14,
                                                hjust = 0),
                   plot.margin = margin(
                     t = 0.1,
                     r = 0.5,
                     b = 0.1,
                     l = 0.1,
                     "inch"
                   ),
                   strip.text = element_text(size = 14))

stacked_theme <- function() {
  scaling_factor <- 0.5  # Further reduce size for smaller aesthetics
  
  theme(
    legend.position = "top", 
    legend.box = "vertical", 
    legend.justification = 'left',
    legend.key.size = unit(4.5 * scaling_factor, "mm"),  # Reduced size
    legend.title = element_text(size = 14 * scaling_factor),  # Reduced size
    legend.text = element_text(size = 14 * scaling_factor),  # Reduced size
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.title = element_text(size = 14 * scaling_factor),  # Reduced size
    axis.text = element_text(size = 14 * scaling_factor),  # Reduced size
    axis.line = element_line(colour = "black", size=0.2), 
    plot.title = element_text(size = 18 * scaling_factor, hjust = 0),  # Reduced size
    plot.subtitle = element_text(size = 18 * scaling_factor, hjust = 0),  # Reduced size
    plot.margin = margin(
      t = 0.1,  # Reduced size
      r = 0.1,  # Reduced size
      b = 0.1,  # Reduced size
      l = 0.1,  # Reduced size
      "inch"
    ),
    strip.text = element_text(size = 14 * scaling_factor)  # Reduced size
  )
}





# instruction <- "All rows belonging to the same Pavlovia session (a CSV results file) have the same pastel shade.  Each row is a session event: ending, error, or warning. The right side of each column’s heading has buttons to sort up and down, and below is a text box for selection."
instruction <- "Each row is one Pavlovia session. All rows belonging to the same Prolific session have the same pastel shade. One Prolific session includes several Pavlovia sessions when the participant doesn’t finish and tries again."

scale_family <- function(...) {
  nfamily <- length(quartzFonts())
  namesfamily <- names(quartzFonts())
  discrete_scale("family", "family_d", function(n)namesfamily[(1:n)%%nfamily+1])
}

# eq1_text <- "\\(totalDBSPL = 10 \\text{log}_{10}(10 ^ {\\frac{backgroundDbSpl}{10}} + 10 ^ {\\frac{(gainDbSpl + inDb)}{10}}) \\\\  
# outDB = \\begin{cases} T + \\frac{(totalDBSPL - T)}{R} & \\text{if} \\ \\ totalDBSPL > T + \\frac{W}{2} \\\\
# totalDBSPL + \\frac{(1-R)}{R}\\frac{(totalDBSPL - (T-\\frac{W}{2}))^2}{2W} & \\text{if} \\ \\ T - \\frac{W}{2} < totalDBSPL \\leq T + \\frac{W}{2} \\\\ 
# totalDBSPL & \\text{if} \\ \\ totalDBSPL \\leq T - \\frac{W}{2} \\end{cases}\\)"


eq1_text <- "\\(compressed = \\begin{cases} T + Q(in - T) & \\text{if} \\ \\ in > T + \\frac{W}{2} \\\\
in + (1-Q)\\frac{(in - (T-\\frac{W}{2}))^2}{2W} & \\text{if} \\ \\ T - \\frac{W}{2} < in \\leq T + \\frac{W}{2} \\\\ 
in & \\text{if} \\ \\ in \\leq T - \\frac{W}{2} \\end{cases}\\\\out = compressed + gain\\)"

reference <- "http://eecs.qmul.ac.uk/~josh/documents/2012/GiannoulisMassbergReiss-dynamicrangecompression-JAES2012.pdf"