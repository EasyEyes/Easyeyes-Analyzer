
#### dataTable callback function
data_table_call_back = "
    table.column(9).nodes().to$().css({cursor: 'pointer'});
    var format1 = function(d) {
      return '<p>' + d[9] + '</p>';
    };
    table.on('click', 'td.details-control1', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format1(row.data())).show();
      }
    });

    table.column(10).nodes().to$().css({cursor: 'pointer'});
    var format2 = function(d) {
      return '<p>' + d[10] + '</p>';
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

##### ggplot download theme #####
downloadtheme <- theme(legend.position = "right", 
                       legend.box = "vertical", 
                       legend.justification = c(1,1),
                       legend.margin = margin(-0.4),
                       legend.key.size = unit(4.5, "mm"),
                       legend.title = element_text(size=16),
                       legend.text = element_text(size=16),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.title = element_text(size = 16),
                       axis.text = element_text(size = 16),
                       axis.line = element_line(colour = "black"),
                       plot.title = element_text(size=16))

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
                   plot.title = element_text(size=14))

instruction <- "All rows belonging to the same Pavlovia session (a CSV results file) have the same pastel shade.  Each row is a session event: ending, error, or warning. The right side of each column’s heading has buttons to sort up and down, and below is a text box for selection."