library(dplyr)
library(DT)

# Each time update the summary table, the rmd report need to be updated accordingly.

data_table_call_back = "

  table.column(29).nodes().to$().css({cursor: 'pointer'});
    var format8 = function(d) {
      return '<p>' + d[29] + '</p>';
    };
    table.on('click', 'td.computer51Deg', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format8(row.data())).show();
      }
    });
    
    table.column(15).nodes().to$().css({cursor: 'pointer'});
    var format1 = function(d) {
      return '<p>' + d[15] + '</p>';
    };
    table.on('click', 'td.errorC-control', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format1(row.data())).show();
      }
    });

    table.column(16).nodes().to$().css({cursor: 'pointer'});
    var format2 = function(d) {
      return '<p>' + d[16] + '</p>';
    };
    table.on('click', 'td.warnC-control', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format2(row.data())).show();
      }
    });
    
    table.column(30).nodes().to$().css({cursor: 'pointer'});
    var format6 = function(d) {
      return '<p>' + d[30] + '</p>';
    };
    table.on('click', 'td.loudspeakerSurvey', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format6(row.data())).show();
      }
    });
    
    table.column(31).nodes().to$().css({cursor: 'pointer'});
    var format5 = function(d) {
      return '<p>' + d[31] + '</p>';
    };
    table.on('click', 'td.microphoneSurvey', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      console.log(td);
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format5(row.data())).show();
      }
    });

    table.column(1).nodes().to$().css({cursor: 'pointer'});
    var format3 = function(d) {
      return '<p>' + d[1] + '</p> <p>' + d[3] + '</p>';
    };
    table.column(3).nodes().to$().css({cursor: 'pointer'});
    var format4 = function(d) {
      return '<p>' + d[1] + '</p> <p>' + d[3] + '</p>';
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

get_lateness_and_duration <- function(all_files){
  t <- all_files %>% 
  
    select(participant, date, targetMeasuredLatenessSec, targetMeasuredDurationSec, targetDurationSec) %>% 
    mutate(targetDurationSec = as.numeric(targetDurationSec),
           targetMeasuredDurationSec = as.numeric(targetMeasuredDurationSec),
           targetMeasuredLatenessSec = as.numeric(targetMeasuredLatenessSec)) %>% 
    group_by(participant, date) %>% 
    summarize(targetMeasuredLatenessMeanSec = mean(targetMeasuredLatenessSec, na.rm = TRUE) * 1000,
              targetMeasuredLatenessSDSec = sd(targetMeasuredLatenessSec, na.rm = TRUE) * 1000,
              targetMeasuredDurationMeanSec = mean(targetMeasuredDurationSec - targetDurationSec, na.rm = TRUE) * 1000,
              targetMeasuredDurationSDSec = sd(targetMeasuredDurationSec - targetDurationSec, na.rm = TRUE) * 1000,
              .groups = "keep") %>% 
    mutate(tardyMs =
             paste0(round(targetMeasuredLatenessMeanSec), "±",
                    round(targetMeasuredLatenessSDSec)),
           excessMs =
             paste0(round(targetMeasuredDurationMeanSec), "±",
                    round(targetMeasuredDurationSDSec))) %>% 
    select(-targetMeasuredLatenessMeanSec,
           -targetMeasuredLatenessSDSec,
           -targetMeasuredDurationMeanSec,
           -targetMeasuredDurationSDSec)
  t <- t %>% 
    mutate(date = parse_date_time(date, orders = c('ymdHMS', 'mdyHMS'))) %>% 
    mutate(date = format(date, "%b %d, %Y, %H:%M:%S"))
  return(t)
}

generate_summary_table <- function(data_list){
  all_files <- tibble()
  for (i in 1 : length(data_list)) {
    t <- data_list[[i]] %>% select(ProlificParticipantID, participant, prolificSessionID, deviceType, 
                                   cores, browser, deviceSystemFamily, deviceLanguage,
                                   block, block_condition,conditionName, targetTask, targetKind, 
                                   thresholdParameter, resolution,error, warning, targetMeasuredLatenessSec,
                                   targetMeasuredDurationSec,date, targetDurationSec, rows, cols, kb, 
                                   ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,
                                   `Microphone survey`, QRConnect, questionAndAnswerResponse) %>% 
      distinct(ProlificParticipantID, participant, prolificSessionID, deviceType, 
               cores, browser, deviceSystemFamily, deviceLanguage,
               block, block_condition,conditionName, targetTask, targetKind, 
               thresholdParameter, resolution,error, warning, targetMeasuredLatenessSec,
               targetMeasuredDurationSec,date, targetDurationSec, rows, cols, kb, 
               ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`, 
               QRConnect, questionAndAnswerResponse) %>% 
      arrange(`Loudspeaker survey`)
    tmp <- t$`Loudspeaker survey`[1]
    t <- t %>% mutate(`Loudspeaker survey` = ifelse(is.na(tmp), '',tmp))
    t <- t %>% arrange(`_needsUnmet`)
    tmp <- t$`_needsUnmet`[1]
    t <- t %>% mutate(`_needsUnmet` = ifelse(is.na(tmp), '',tmp))
    t <- t %>% arrange(`Microphone survey`)
    tmp <- t$`Microphone survey`[1]
    t <- t %>% mutate(`Microphone survey` = ifelse(is.na(tmp), '',tmp))
    t <- t %>% arrange(`ComputerInfoFrom51Degrees`)
    tmp <- t$`ComputerInfoFrom51Degrees`[1]
    t <- t %>% mutate(`ComputerInfoFrom51Degrees` = ifelse(is.na(tmp), '',tmp))
    
    t <- t %>% arrange(desc(QRConnect))
    tmp <- t$QRConnect[1]
    t <- t %>% mutate(QRConnect = ifelse(is.na(tmp), '',tmp)) 
    t <- t %>% arrange((questionAndAnswerResponse))
    tmp <- t$questionAndAnswerResponse[1]
    t <- t %>% mutate(questionAndAnswerResponse = ifelse(is.na(tmp), '',tmp)) %>% 
      distinct(ProlificParticipantID, participant, prolificSessionID, deviceType, 
               cores, browser, deviceSystemFamily, deviceLanguage,
               block, block_condition,conditionName, targetTask, targetKind, 
               thresholdParameter, resolution,error, warning, targetMeasuredLatenessSec,
               targetMeasuredDurationSec,date, targetDurationSec, rows, cols, kb, 
               ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,
               `Microphone survey`, QRConnect, questionAndAnswerResponse)
    all_files <- rbind(all_files,t)
  }
  print('done all files')
  trial <- all_files %>% group_by(participant, block_condition) %>% count()
  lateness_duration <- get_lateness_and_duration(all_files)
  
  #### errors ####
  error <- all_files %>% 
    dplyr::filter(error != "" & error != "Incomplete") %>%
    mutate(warning = "") %>% 
    mutate(ok = paste(emoji("x")))
  print('done error')
  
  #### warnings ####
  warnings <- all_files %>% 
    dplyr::filter(warning != "") %>%
    mutate(error = "") %>% 
    mutate(ok = emoji("large_orange_diamond"))
  print('done warnings')
  
  #### incomplete files ####
  noerror_fails = tibble()
  for (i in 1 : length(data_list)) {

    if (tail(data_list[[i]]$experimentCompleteBool, 1) == 'FALSE' | 
        is.na(tail(data_list[[i]]$experimentCompleteBool, 1))) {
      if (!data_list[[i]]$participant[1] %in% error$participant) {
        t <- data_list[[i]] %>% 
          distinct(ProlificParticipantID, participant, prolificSessionID, deviceType,
                   cores, deviceSystemFamily, browser, resolution, rows, cols, kb, 
                   ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`,
                   QRConnect, questionAndAnswerResponse) %>% 
          mutate(error = "Incomplete") %>% 
          select(error,ProlificParticipantID, participant, prolificSessionID, deviceType, 
                 cores, deviceSystemFamily, browser, resolution, rows, cols, kb, 
                 ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`,
                 QRConnect, questionAndAnswerResponse) %>% 
          arrange(`Loudspeaker survey`)
        tmp <- t$`Loudspeaker survey`[1]
        t <- t %>% mutate(`Loudspeaker survey` = ifelse(is.na(tmp), '',tmp))
        t <- t %>% arrange(`_needsUnmet`)
        tmp <- t$`_needsUnmet`[1]
        t <- t %>% mutate(`_needsUnmet` = ifelse(is.na(tmp), '',tmp))
        t <- t %>% arrange(`Microphone survey`)
        tmp <- t$`Microphone survey`[1]
        t <- t %>% mutate(`Microphone survey` = ifelse(is.na(tmp), '',tmp))
        t <- t %>% arrange(desc(QRConnect))
        tmp <- t$QRConnect[1]
        t$QRConnect = ifelse(is.na(tmp), '',tmp)
        t <- t %>% arrange(`ComputerInfoFrom51Degrees`)
        tmp <- t$`ComputerInfoFrom51Degrees`[1]
        t <- t %>% mutate(`ComputerInfoFrom51Degrees` = ifelse(is.na(tmp), '',tmp))
        t <- t %>% arrange((questionAndAnswerResponse))
        tmp <- t$questionAndAnswerResponse[1]
        t <- t %>% mutate(questionAndAnswerResponse = ifelse(is.na(tmp), '',tmp))
        t <- t %>% 
          distinct(error, ProlificParticipantID, participant, prolificSessionID, deviceType, 
                   cores, deviceSystemFamily, browser, resolution, rows, cols, kb, 
                   ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`,
                   QRConnect, questionAndAnswerResponse)
        info <- data_list[[i]] %>% 
          distinct(block, block_condition, conditionName, 
                   targetTask, targetKind, thresholdParameter) %>% 
          dplyr::filter(block_condition != "", conditionName != "") 
        if (nrow(info) > 0) {
          info <- info %>% tail(1)
        } else {
          info <- tibble(block = NA,
                         block_condition = NA, 
                         conditionName = NA, 
                         targetTask = NA, 
                         targetKind = NA,
                         thresholdParameter = NA)
        }
        t <- cbind(t, info)
        t$ok <- emoji("construction")
        noerror_fails <- rbind(noerror_fails,t)
      }
    }
  }
  
  print('done noerror_fails')
  
  
  noerror_fails$warning = ""
  completes = tibble()
  for (i in 1 : length(data_list)) {
    if (!data_list[[i]]$participant[1] %in% error$participant 
        & !data_list[[i]]$participant[1] %in% noerror_fails$participant) {
      t <- data_list[[i]] %>% 
        distinct(ProlificParticipantID, participant, prolificSessionID, deviceType, error,
                 cores, deviceSystemFamily, browser, resolution, rows, cols, kb, 
                 ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`,
                 QRConnect, questionAndAnswerResponse) %>% 
        arrange(`Loudspeaker survey`)
      t$`Loudspeaker survey` = t$`Loudspeaker survey`[1]
      t <- t %>% arrange( `_needsUnmet`)
      t$`_needsUnmet` = t$`_needsUnmet`[1]
      t <- t %>% arrange(`Microphone survey`)
      t$`Microphone survey` = t$`Microphone survey`[1]
      t <- t %>% arrange(`ComputerInfoFrom51Degrees`)
      tmp <- t$`ComputerInfoFrom51Degrees`[1]
      t <- t %>% mutate(`ComputerInfoFrom51Degrees` = ifelse(is.na(tmp), '',tmp))
      t <- t %>% arrange(desc(QRConnect))
      tmp <- t$QRConnect[1]
      t <- t %>% mutate(QRConnect = ifelse(is.na(tmp), '',tmp))
      t <-t %>% arrange((questionAndAnswerResponse))
      tmp <- t$questionAndAnswerResponse[1]
      t <- t %>% mutate(questionAndAnswerResponse = ifelse(is.na(tmp), '',tmp))
      t <- t %>%
        distinct(ProlificParticipantID, participant, prolificSessionID, deviceType, error,
                 cores, deviceSystemFamily, browser, resolution, rows, cols, kb, 
                 ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`,
                 QRConnect, questionAndAnswerResponse)
      info <- data_list[[i]] %>% 
        distinct(block, block_condition, conditionName, 
                 targetTask, targetKind, thresholdParameter) %>% 
        dplyr::filter(block_condition != "" & conditionName != "") %>% 
        tail(1)
      if (nrow(t) == nrow(info)) {
        t <- cbind(t, info)
      } else {
        t <- t %>% mutate(block='', block_condition='', conditionName='', 
                          targetTask='', targetKind='', thresholdParameter='')
      }
      t$ok <- emoji("white_check_mark")
      completes <- rbind(completes,t)
    }
  }
  completes$warning <- ""
  
  summary_df <- rbind(noerror_fails,
                      completes,
                      error %>% 
                        select(error, warning, ProlificParticipantID, participant, prolificSessionID, deviceType, 
                               cores, deviceSystemFamily, browser, resolution,
                               block, block_condition, conditionName, targetTask, targetKind, 
                               thresholdParameter, ok, rows, cols, kb, ComputerInfoFrom51Degrees,
                               `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`,QRConnect,questionAndAnswerResponse),
                      warnings %>% 
                        select(error, warning, ProlificParticipantID, participant, prolificSessionID, deviceType, 
                               cores, deviceSystemFamily, browser, resolution,
                               block, block_condition, conditionName, targetTask, targetKind, 
                               thresholdParameter, ok, rows, cols, kb, ComputerInfoFrom51Degrees,
                               `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`,QRConnect, questionAndAnswerResponse)) %>% 
    mutate(ok = factor(ok, levels = c(paste(emoji("x")), 
                                      emoji("construction"),
                                      emoji("large_orange_diamond"),
                                      emoji("white_check_mark")
    ))) %>% 
    left_join(lateness_duration, by = "participant") %>% 
    left_join(trial, by = c("participant", 'block_condition')) %>% 
    rename("Prolific participant ID" = "ProlificParticipantID",
           "Pavlovia session ID" = "participant",
           "target kind" = "targetKind",
           "target task" = "targetTask",
           "threshold parameter" = "thresholdParameter",
           "condition name" ="conditionName",
           "device type" = "deviceType",
           "block condition" = "block_condition",
           "system" = "deviceSystemFamily",
           "trial" = "n",
           'KB' = 'kb',
           "unmetNeeds" = '_needsUnmet',
           "computer51Deg" ="ComputerInfoFrom51Degrees",
           "Loudspeaker" = "Loudspeaker survey",
           "Microphone" = "Microphone survey",
           'comment' = 'questionAndAnswerResponse') %>% 
    distinct(`Prolific participant ID`, `Pavlovia session ID`, prolificSessionID, `device type`, system,
             browser, resolution, QRConnect, date, ok, unmetNeeds, error, warning, computer51Deg, cores, tardyMs,
             excessMs, KB, rows, cols, `block condition`, trial, `condition name`,
             `target task`, `threshold parameter`, `target kind`, Loudspeaker, Microphone, comment)
  
  #### order block_condition by splitting and order block and condition order ####
  summary_df <- summary_df %>% 
    mutate(block = as.numeric(unlist(lapply(summary_df$`block condition`, 
                                            FUN = function(x){unlist(str_split(x, "[_]"))[1]}))),
           condition = as.numeric(unlist(lapply(summary_df$`block condition`, 
                                                FUN = function(x){unlist(str_split(x, "[_]"))[2]}))))
  block_condition_order <- summary_df %>%
    distinct(block, condition) %>%
    arrange(block, condition) %>%
    mutate(order = row_number())
  summary_df <- summary_df %>%
    left_join(block_condition_order, by = c("block", "condition")) %>%
    select(-block, -condition)
  summary_df$`threshold parameter` = as.character(summary_df$`threshold parameter`)
  
  return(summary_df)
}

render_summary_datatable <- function(dt, participants, prolific_id){
  datatable(dt,
            class = list(stripe = FALSE),
            selection = 'none',
            filter = "top",
            escape = FALSE,
            # extensions = 'FixedHeader',
            options = list(
              autoWidth = TRUE,
              paging = FALSE,
              scrollX = TRUE, 
              # fixedHeader = TRUE,
              dom= 'lrtip',
              language = list(
                info = 'Showing _TOTAL_ entries',
                infoFiltered =  "(filtered from _MAX_ entries)"
              ),
              columnDefs = list(
                list(visible = FALSE, targets = c(0, 41)),
                list(orderData = 36, targets = 23),
                list(
                  targets = c(15),
                  width = '200px',
                  className = 'errorC-control',
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data && data.length > 30 ?",
                    "data.substr(0, 30) + '...' : data;",
                    "}"
                  )
                ),
                list(
                  targets = c(16),
                  width = '200px',
                  className = 'warnC-control',
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data && data.length > 30 ?",
                    "data.substr(0, 30) + '...' : data;",
                    "}"
                  )
                ),
                list(
                  targets = c(29),
                  width = '200px',
                  className = 'computer51Deg',
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data && data.length > 30 ?",
                    "data.substr(0, 30) + '...' : data;",
                    "}"
                  )
                ),
                list(
                  targets = c(30),
                  width = '250px',
                  className = 'loudspeakerSurvey',
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data && data.length > 30 ?",
                    "data.substr(0, 30) + '...' : data;",
                    "}"
                  )
                ),
                list(
                  targets = c(31),
                  width = '250px',
                  className = 'microphoneSurvey',
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data && data.length > 30 ?",
                    "data.substr(0, 30) + '...' : data;",
                    "}"
                  )
                ),
                list(
                  targets = c(1),
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data && data.length > 6 ?",
                    "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                    "}"
                  ),
                  className = 'information-control1'
                ),
                list(
                  targets = c(3),
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data && data.length > 6 ?",
                    "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                    "}"
                  ),
                  className = 'information-control2'
                ),
                list(
                  width = '50px',
                  targets = c(7, 8, 20, 24),
                  className = 'dt-center'
                ),
                list(
                  width = '20px',
                  targets = c(10,14,15,16,24,28,32,33),
                  className = 'dt-center'
                ),
                list(width = '100px', targets = c(13)),
                list(width = '600px', targets = c(35))
              )
            ),
            callback = JS(data_table_call_back)) %>% 
    formatStyle(names(dt),color = 'black', lineHeight="15px") %>% 
    formatStyle(names(dt)[-1],
                'Pavlovia session ID',
                backgroundColor = styleEqual(participants, random_rgb(length(participants)))) %>% 
    formatStyle(names(dt)[1],
                'Prolific participant ID',
                backgroundColor = styleEqual(prolific_id, random_rgb(length(prolific_id))))
}








