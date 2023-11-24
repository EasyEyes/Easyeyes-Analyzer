

#### dataTable callback function
data_table_call_back = "
    table.column(14).nodes().to$().css({cursor: 'pointer'});
    var format1 = function(d) {
      return '<p>' + d[14] + '</p>';
    };
    table.on('click', 'td.details-control1', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format1(row.data())).show();
      }
    });

    table.column(15).nodes().to$().css({cursor: 'pointer'});
    var format2 = function(d) {
      return '<p>' + d[15] + '</p>';
    };
    table.on('click', 'td.details-control2', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format2(row.data())).show();
      }
    });

    table.column(1).nodes().to$().css({cursor: 'pointer'});
    var format3 = function(d) {
      return '<p>' + d[1] + '</p> <p>' + d[2] + '</p>';
    };
    table.column(2).nodes().to$().css({cursor: 'pointer'});
    var format4 = function(d) {
      return '<p>' + d[1] + '</p> <p>' + d[2] + '</p>';
    };
    table.on('click', 'td.information-control1', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format3(row.data())).show();
      }
    });
    table.on('click', 'td.information-control2', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format4(row.data())).show();
      }
    });
    
    $('div.has-feedback input[type=\"search\"]').attr('placeholder', '');
    
    $('#search').keyup(function(){
      table.search($(this).val()).draw() ;
})
  "

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

library(ggplot2)
##### ggplot download theme #####
downloadtheme <- theme(legend.position = "right", 
                       legend.box = "vertical", 
                       legend.justification = c(1,1),
                       legend.margin = margin(-0.4),
                       legend.key.size = unit(4.5, "mm"),
                       legend.title = element_text(size=14),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14),
                       axis.line = element_line(colour = "black"),
                       plot.title = element_text(size=11),
                       plot.subtitle = element_text(size=14))


##### ggplot display theme #####

plt_theme <- theme(legend.position = "right", 
                   legend.box = "vertical", 
                   legend.justification = c(1,1),
                   legend.margin = margin(-0.4),
                   legend.key.size = unit(4.5, "mm"),
                   legend.title = element_text(size=14),
                   legend.text = element_text(size=14),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.title = element_text(size = 14),
                   axis.text = element_text(size = 14),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=11),
                   plot.subtitle = element_text(size=14))

instruction <- "All rows belonging to the same Pavlovia session (a CSV results file) have the same pastel shade.  Each row is a session event: ending, error, or warning. The right side of each column’s heading has buttons to sort up and down, and below is a text box for selection."


scale_family <- function(...) {
  nfamily <- length(quartzFonts())
  namesfamily <- names(quartzFonts())
  discrete_scale("family", "family_d", function(n)namesfamily[(1:n)%%nfamily+1])
}

eq1_text <- "\\(totalDBSPL = 10 \\text{log}_{10}(10 ^ {\\frac{backgroundDbSpl}{10}} + 10 ^ {\\frac{(gainDbSpl + inDb)}{10}}) \\\\  
outDB = \\begin{cases} T + \\frac{(totalDBSPL - T)}{R} & \\text{if} \\ \\ totalDBSPL > T + \\frac{W}{2} \\\\
totalDBSPL + \\frac{(1-R)}{R}\\frac{(totalDBSPL - (T-\\frac{W}{2}))^2}{2W} & \\text{if} \\ \\ T - \\frac{W}{2} < totalDBSPL \\leq T + \\frac{W}{2} \\\\ 
totalDBSPL & \\text{if} \\ \\ totalDBSPL \\leq T - \\frac{W}{2} \\end{cases}\\)"

reference <- "http://eecs.qmul.ac.uk/~josh/documents/2012/GiannoulisMassbergReiss-dynamicrangecompression-JAES2012.pdf"