library(dplyr)
library(DT)
source('./other/utility.R')
# Each time update the summary table, the rmd report need to be updated accordingly.

data_table_call_back = "
  table.column(31).nodes().to$().css({cursor: 'pointer'});
    var format8 = function(d) {
      return '<p>' + d[31] + '</p>';
    };
    table.on('click', 'td.computer51Deg', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format8(row.data())).show();
      }
    });

    table.column(16).nodes().to$().css({cursor: 'pointer'});
    var format1 = function(d) {
      return '<p>' + d[16] + '</p>';
    };
    table.on('click', 'td.errorC-control', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format1(row.data())).show();
      }
    });

    table.column(17).nodes().to$().css({cursor: 'pointer'});
    var format2 = function(d) {
      return '<p>' + d[17] + '</p>';
    };
    table.on('click', 'td.warnC-control', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format2(row.data())).show();
      }
    });

    table.column(32).nodes().to$().css({cursor: 'pointer'});
    var format6 = function(d) {
      return '<p>' + d[32] + '</p>';
    };
    table.on('click', 'td.loudspeakerSurvey', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format6(row.data())).show();
      }
    });

    table.column(33).nodes().to$().css({cursor: 'pointer'});
    var format5 = function(d) {
      return '<p>' + d[33] + '</p>';
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

    table.column(2).nodes().to$().css({cursor: 'pointer'});
    table.column(3).nodes().to$().css({cursor: 'pointer'});
    table.column(4).nodes().to$().css({cursor: 'pointer'});

    var format3 = function(d) {
    return '<p>' + d[2] + '</p> <p>' + d[3]+  '</p> <p>' + d[4] + '</p>';
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
        row.child(format3(row.data())).show();
      }
    });
    table.on('click', 'td.information-control3', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format3(row.data())).show();
      }
    });

    $('div.has-feedback input[type=\"search\"]').attr('placeholder', '');

    $('#search').keyup(function(){
      table.search($(this).val()).draw() ;
})
  "

get_lateness_and_duration <- function(all_files) {
  print('inside get_lateness_and_duration')
  t <- all_files %>%
    select(
      participant,
      date,
      targetMeasuredLatenessSec,
      targetMeasuredDurationSec,
      targetDurationSec
    ) %>%
    mutate(
      targetDurationSec = as.numeric(targetDurationSec),
      targetMeasuredLatenessSec = as.numeric(targetMeasuredLatenessSec)
    )
  
  if (is.character(t$targetMeasuredDurationSec)) {
    t <- t %>% separate_rows(targetMeasuredDurationSec, sep = ',')
  }
  
  t$targetMeasuredDurationSec <-
    as.numeric(t$targetMeasuredDurationSec)
  
  t <- t %>%
    group_by(participant, date) %>%
    summarize(
      targetMeasuredLatenessMeanSec = mean(targetMeasuredLatenessSec, na.rm = TRUE) * 1000,
      targetMeasuredLatenessSDSec = sd(targetMeasuredLatenessSec, na.rm = TRUE) * 1000,
      targetMeasuredDurationMeanSec = mean(targetMeasuredDurationSec - targetDurationSec, na.rm = TRUE) * 1000,
      targetMeasuredDurationSDSec = sd(targetMeasuredDurationSec - targetDurationSec, na.rm = TRUE) * 1000,
      .groups = "keep"
    ) %>%
    mutate(
      tardyMs = round(targetMeasuredLatenessMeanSec),
      excessMs = round(targetMeasuredDurationMeanSec)
    ) %>%
    select(
      -targetMeasuredLatenessMeanSec,-targetMeasuredLatenessSDSec,-targetMeasuredDurationMeanSec,-targetMeasuredDurationSDSec
    )
  t <- t %>%
    mutate(date = str_remove(date, " UTC[+-]\\d+")) %>%
    mutate(date = parse_date_time(date, orders = c('ymdHMS', 'mdyHMS'))) %>%
    mutate(date = format(date, "%b %d, %Y, %H:%M:%S"))
  
  return(t)
}

generate_summary_table <- function(data_list, stairs) {
  all_files <- tibble()
  params <- foreach(i = 1:length(data_list), .combine = 'rbind') %do% {
    t <- data_list[[i]] %>%
      filter(!is.na(staircaseName)) %>%
      select(
        participant,
        `heapTotalAfterDrawing (MB)`,
        `heapLimitAfterDrawing (MB)`,
        deviceMemoryGB,
        mustTrackSec
      )
  }
  
  NQuestTrials <- stairs %>%
    arrange(participant, staircaseName) %>%
    group_by(participant, staircaseName) %>%
    summarize(
      goodTrials = sum(trialGivenToQuest, na.rm = T),
      badTrials = sum(!trialGivenToQuest, na.rm = T)
    ) %>%
    ungroup() %>%
    group_by(participant) %>%
    summarize(goodTrials = format(round(mean(goodTrials), 2), nsmall = 2),
              badTrials = format(round(mean(badTrials), 2), nsmall = 2))
  
  params <- params %>%
    group_by(participant,
             deviceMemoryGB) %>%
    summarize(
      mustTrackSec = format(round(mean(
        mustTrackSec, na.rm = T
      ), 2), nsmall = 2),
      `heapLimitAfterDrawing (MB)` = format(round(
        mean(`heapLimitAfterDrawing (MB)`, na.rm = T), 2
      ), nsmall = 2),
      heapTotalAvgMB = format(round(
        mean(`heapTotalAfterDrawing (MB)`, na.rm = T), 2
      ), nsmall = 2)
    ) %>%
    left_join(NQuestTrials, by = 'participant') %>%
    rename("Pavlovia session ID" = "participant")
  
  webGL <-
    get_webGL(data_list) %>% rename("Pavlovia session ID" = "participant")
  
  for (i in 1:length(data_list)) {
    t <- data_list[[i]] %>%
      distinct(
        ProlificParticipantID,
        participant,
        prolificSessionID,
        block,
        block_condition,
        conditionName,
        targetTask,
        targetKind,
        thresholdParameter,
        deviceType,
        cores,
        deviceSystemFamily,
        browser,
        resolution,
        rows,
        cols,
        kb,
        targetMeasuredLatenessSec,
        targetMeasuredDurationSec,
        date,
        targetDurationSec,
        ComputerInfoFrom51Degrees,
        `_needsUnmet`,
        `Loudspeaker survey`,
        `Microphone survey`,
        QRConnect,
        questionAndAnswerResponse,
        error,
        warning
      ) %>%
      arrange(`Loudspeaker survey`)
    loudspeakerSurvey <-
      t[t$`Loudspeaker survey` != "", ]$`Loudspeaker survey`
    micSurvey <- t[t$`Microphone survey` != "", ]$`Microphone survey`
    needsUnmet <- t[t$`_needsUnmet` != "", ]$`_needsUnmet`
    QRConnect <- t[t$QRConnect != "", ]$`QRConnect`
    questionAndAnswerResponse <-
      t[t$questionAndAnswerResponse != "", ]$`questionAndAnswerResponse`
    ComputerInfoFrom51Degrees <-
      t[t$ComputerInfoFrom51Degrees != "", ]$`ComputerInfoFrom51Degrees`
    t <- t %>%
      mutate(
        `Loudspeaker survey` = ifelse(length(loudspeakerSurvey) == 0, '', loudspeakerSurvey),
        `_needsUnmet` = ifelse(length(needsUnmet) == 0, '', needsUnmet),
        `Microphone survey` = ifelse(length(micSurvey) == 0, '', micSurvey),
        QRConnect = ifelse(length(QRConnect) == 0, '', needsUnmet),
        `ComputerInfoFrom51Degrees` = ifelse(
          length(ComputerInfoFrom51Degrees) == 0,
          '',
          ComputerInfoFrom51Degrees
        ),
        questionAndAnswerResponse = ifelse(
          length(questionAndAnswerResponse) == 0,
          '',
          questionAndAnswerResponse
        )
      ) %>%
      distinct(
        ProlificParticipantID,
        participant,
        prolificSessionID,
        block,
        block_condition,
        conditionName,
        targetTask,
        targetKind,
        thresholdParameter,
        deviceType,
        cores,
        deviceSystemFamily,
        browser,
        resolution,
        rows,
        cols,
        kb,
        targetMeasuredLatenessSec,
        targetMeasuredDurationSec,
        date,
        targetDurationSec,
        ComputerInfoFrom51Degrees,
        `_needsUnmet`,
        `Loudspeaker survey`,
        `Microphone survey`,
        QRConnect,
        questionAndAnswerResponse,
        error,
        warning
      )
    all_files <- rbind(all_files, t)
  }
  
  logFont <- foreach(i = 1:length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% distinct(participant, `_logFontBool`) %>%
      filter(`_logFontBool` == TRUE) %>%
      rename('Pavlovia session ID' = 'participant')
  }
  
  print('done all files')
  trial <- all_files %>% group_by(participant, block_condition) %>% count()
  lateness_duration <- get_lateness_and_duration(all_files)
  
  #### errors ####
  error <- all_files %>%
    dplyr::filter(error != "" & error != "Incomplete") %>%
    group_by(participant) %>%
    summarise(error = paste(error, collapse = "<br>"),
              .groups = "drop")
  # mutate(warning = "") %>%
  # mutate(ok = paste(emoji("x")))
  print('done error')
  
  #### warnings ####
  warnings <- all_files %>%
    dplyr::filter(warning != "") %>%
    group_by(participant) %>%
    summarise(warning = paste(warning, collapse = "<br>"),
              .groups = "drop")
  # mutate(error = "") %>%
  # mutate(ok = emoji("large_orange_diamond"))
  print('done warnings')
  
  #### incomplete files ####
  incomplete = tibble()
  for (i in 1:length(data_list)) {
    if (nrow(data_list[[i]] >=  1)) {
      experimentCompleteBool = 
        any(data_list[[i]]$experimentCompleteBool[sapply(data_list[[i]]$experimentCompleteBool, length) > 0])
      if (!experimentCompleteBool) {
        t <- data_list[[i]] %>%
          distinct(
            ProlificParticipantID,
            participant,
            prolificSessionID,
            deviceType,
            cores,
            deviceSystemFamily,
            browser,
            resolution,
            rows,
            cols,
            kb,
            ComputerInfoFrom51Degrees,
            `_needsUnmet`,
            `Loudspeaker survey`,
            `Microphone survey`,
            QRConnect,
            questionAndAnswerResponse
          ) %>%
          arrange(`Loudspeaker survey`)
        loudspeakerSurvey <-
          t[t$`Loudspeaker survey` != "", ]$`Loudspeaker survey`
        micSurvey <-
          t[t$`Microphone survey` != "", ]$`Microphone survey`
        needsUnmet <- t[t$`_needsUnmet` != "", ]$`_needsUnmet`
        QRConnect <- t[t$QRConnect != "", ]$`QRConnect`
        questionAndAnswerResponse <-
          t[t$questionAndAnswerResponse != "", ]$`questionAndAnswerResponse`
        ComputerInfoFrom51Degrees <-
          t[t$ComputerInfoFrom51Degrees != "", ]$`ComputerInfoFrom51Degrees`
        t <- t %>%
          mutate(
            `Loudspeaker survey` = ifelse(length(loudspeakerSurvey) == 0, '', loudspeakerSurvey),
            `_needsUnmet` = ifelse(length(needsUnmet) == 0, '', needsUnmet),
            `Microphone survey` = ifelse(length(micSurvey) == 0, '', micSurvey),
            QRConnect = ifelse(length(QRConnect) == 0, '', needsUnmet),
            `ComputerInfoFrom51Degrees` = ifelse(
              length(ComputerInfoFrom51Degrees) == 0,
              '',
              ComputerInfoFrom51Degrees
            ),
            questionAndAnswerResponse = ifelse(
              length(questionAndAnswerResponse) == 0,
              '',
              questionAndAnswerResponse
            )
          ) %>%
          distinct(
            ProlificParticipantID,
            participant,
            prolificSessionID,
            deviceType,
            cores,
            deviceSystemFamily,
            browser,
            resolution,
            rows,
            cols,
            kb,
            ComputerInfoFrom51Degrees,
            `_needsUnmet`,
            `Loudspeaker survey`,
            `Microphone survey`,
            QRConnect,
            questionAndAnswerResponse
          )
        info <- data_list[[i]] %>%
          distinct(
            block,
            block_condition,
            conditionName,
            targetTask,
            targetKind,
            thresholdParameter
          ) %>%
          dplyr::filter(block_condition != "")
        if (nrow(info) > 0) {
          info <- info %>% tail(1)
        } else {
          info <- tibble(
            block = 0,
            block_condition = NA,
            conditionName = NA,
            targetTask = NA,
            targetKind = NA,
            thresholdParameter = NA
          )
        }
        t <- cbind(t, info)
        if (t$participant[1] %in% error$participant) {
          t$ok <- emoji("x")
        } else {
          t$ok <- emoji("construction")
        }
        
        incomplete <- rbind(incomplete, t)
      }
    }
   
  }
  incomplete_participant <- c()
  if (nrow(incomplete) > 0) {
    incomplete_participant <- incomplete_participant$participant
  }
  
  print('done noerror_fails')
  # noerror_fails$warning = ""
  sessions = incomplete
  for (i in 1:length(data_list)) {
    if (nrow(data_list[[i]] >=  1)) {
      experimentCompleteBool = 
        any(data_list[[i]]$experimentCompleteBool[sapply(data_list[[i]]$experimentCompleteBool, length) > 0])
      if (experimentCompleteBool) {
        t <- data_list[[i]] %>%
          distinct(
            ProlificParticipantID,
            participant,
            prolificSessionID,
            deviceType,
            cores,
            deviceSystemFamily,
            browser,
            resolution,
            rows,
            cols,
            kb,
            ComputerInfoFrom51Degrees,
            `_needsUnmet`,
            `Loudspeaker survey`,
            `Microphone survey`,
            QRConnect,
            questionAndAnswerResponse
          )
        
        loudspeakerSurvey <-
          t[t$`Loudspeaker survey` != "", ]$`Loudspeaker survey`
        micSurvey <-
          t[t$`Microphone survey` != "", ]$`Microphone survey`
        needsUnmet <- t[t$`_needsUnmet` != "", ]$`_needsUnmet`
        QRConnect <- t[t$QRConnect != "", ]$`QRConnect`
        questionAndAnswerResponse <-
          t[t$questionAndAnswerResponse != "", ]$`questionAndAnswerResponse`
        ComputerInfoFrom51Degrees <-
          t[t$ComputerInfoFrom51Degrees != "", ]$`ComputerInfoFrom51Degrees`
        
        t <- t %>%
          mutate(
            `Loudspeaker survey` = ifelse(length(loudspeakerSurvey) == 0, '', loudspeakerSurvey),
            `_needsUnmet` = ifelse(length(needsUnmet) == 0, '', needsUnmet),
            `Microphone survey` = ifelse(length(micSurvey) == 0, '', micSurvey),
            QRConnect = ifelse(length(QRConnect) == 0, '', needsUnmet),
            `ComputerInfoFrom51Degrees` = ifelse(
              length(ComputerInfoFrom51Degrees) == 0,
              '',
              ComputerInfoFrom51Degrees
            ),
            questionAndAnswerResponse = ifelse(
              length(questionAndAnswerResponse) == 0,
              '',
              questionAndAnswerResponse
            )
          ) %>%
          distinct(
            ProlificParticipantID,
            participant,
            prolificSessionID,
            deviceType,
            cores,
            deviceSystemFamily,
            browser,
            resolution,
            rows,
            cols,
            kb,
            ComputerInfoFrom51Degrees,
            `_needsUnmet`,
            `Loudspeaker survey`,
            `Microphone survey`,
            QRConnect,
            questionAndAnswerResponse
          )
        info <- data_list[[i]] %>%
          distinct(
            block,
            block_condition,
            conditionName,
            targetTask,
            targetKind,
            thresholdParameter
          ) %>%
          dplyr::filter(block_condition != "")
        if (nrow(info) > 0) {
          info <- info %>% tail(1)
        } else {
          info <- tibble(
            block = 0,
            block_condition = NA,
            conditionName = NA,
            targetTask = NA,
            targetKind = NA,
            thresholdParameter = NA
          )
        }
        t <- cbind(t, info)
        t$ok <- emoji("white_check_mark")
        sessions <- rbind(sessions, t)
      }
    }
  }
  
  # completes$warning <- ""
  # completes$error <- ""
  print('done completes')
  
  summary_df <- sessions %>%
    left_join(error, by = 'participant') %>%
    left_join(warnings, by = 'participant') %>% 
    mutate(ok = factor(ok, levels = c(
      emoji("x"),
      emoji("construction"),
      emoji("white_check_mark")
    ))) %>%
    left_join(lateness_duration, by = "participant") %>%
    left_join(trial, by = c("participant", 'block_condition')) %>%
    rename(
      "Prolific participant ID" = "ProlificParticipantID",
      "Pavlovia session ID" = "participant",
      "target kind" = "targetKind",
      "target task" = "targetTask",
      "threshold parameter" = "thresholdParameter",
      "condition name" = "conditionName",
      "device type" = "deviceType",
      "block condition" = "block_condition",
      "system" = "deviceSystemFamily",
      "trial" = "n",
      'KB' = 'kb',
      "unmetNeeds" = '_needsUnmet',
      "computer51Deg" = "ComputerInfoFrom51Degrees",
      "Loudspeaker" = "Loudspeaker survey",
      "Microphone" = "Microphone survey",
      'comment' = 'questionAndAnswerResponse'
    ) %>%
    distinct(
      `Prolific participant ID`,
      `Pavlovia session ID`,
      prolificSessionID,
      `device type`,
      system,
      browser,
      resolution,
      QRConnect,
      date,
      ok,
      unmetNeeds,
      error,
      warning,
      computer51Deg,
      cores,
      tardyMs,
      excessMs,
      KB,
      rows,
      cols,
      block,
      `block condition`,
      trial,
      `condition name`,
      `target task`,
      `threshold parameter`,
      `target kind`,
      Loudspeaker,
      Microphone,
      comment
    )
  
  #### order block_condition by splitting and order block and condition order ####
  summary_df <- summary_df %>%
    mutate(block_new = as.numeric(unlist(
      lapply(
        summary_df$`block condition`,
        FUN = function(x) {
          unlist(str_split(x, "[_]"))[1]
        }
      )
    )),
    condition = as.numeric(unlist(
      lapply(
        summary_df$`block condition`,
        FUN = function(x) {
          unlist(str_split(x, "[_]"))[2]
        }
      )
    ))) %>%
    group_by(`Pavlovia session ID`, `block condition`, block) %>%
    mutate(block = max(block, block_new, na.rm = T)) %>%
    ungroup()
  block_condition_order <- summary_df %>%
    distinct(block, condition) %>%
    arrange(block, condition) %>%
    mutate(order = row_number())
  summary_df <- summary_df %>%
    left_join(block_condition_order, by = c("block", "condition")) %>%
    select(-`block condition`) %>%
    mutate(`threshold parameter` = as.character(`threshold parameter`)) %>%
    left_join(logFont, by = 'Pavlovia session ID') %>%
    left_join(webGL, by = 'Pavlovia session ID') %>%
    left_join(params, by = 'Pavlovia session ID')
  print('done summary_df')
  return(summary_df)
}

# render_summary_datatable <- function(dt, participants, prolific_id){
#   datatable(dt,
#             class = list(stripe = FALSE, 'compact'),
#             selection = 'none',
#             filter = "top",
#             escape = FALSE,
#             # extensions = 'FixedHeader',
#             options = list(
#               autoWidth = TRUE,
#               paging = FALSE,
#               scrollX = TRUE,
#               fixedHeader = TRUE,
#               dom= 'lrtip',
#               language = list(
#                 info = 'Showing _TOTAL_ entries',
#                 infoFiltered =  "(filtered from _MAX_ entries)"
#               ),
#               columnDefs = list(
#                 list(visible = FALSE, targets = c(0, 53)),
#                 list(
#                   targets = c(16),
#                   width = '100px',
#                   className = 'errorC-control',
#                   render = JS(
#                     "function(data, type, row, meta) {",
#                     "return type === 'display' && data && data.length > 30 ?",
#                     "data.substr(0, 30) + '...' : data;",
#                     "}"
#                   )
#                 ),
#                 list(
#                   targets = c(17),
#                   width = '100px',
#                   className = 'warnC-control',
#                   render = JS(
#                     "function(data, type, row, meta) {",
#                     "return type === 'display' && data && data.length > 30 ?",
#                     "data.substr(0, 30) + '...' : data;",
#                     "}"
#                   )
#                 ),
#                 list(
#                   targets = c(31),
#                   width = '50px',
#                   className = 'computer51Deg',
#                   render = JS(
#                     "function(data, type, row, meta) {",
#                     "return type === 'display' && data && data.length > 30 ?",
#                     "data.substr(0, 30) + '...' : data;",
#                     "}"
#                   )
#                 ),
#                 list(
#                   targets = c(32),
#                   width = '50px',
#                   className = 'loudspeakerSurvey',
#                   render = JS(
#                     "function(data, type, row, meta) {",
#                     "return type === 'display' && data && data.length > 30 ?",
#                     "data.substr(0, 30) + '...' : data;",
#                     "}"
#                   )
#                 ),
#                 list(
#                   targets = c(33),
#                   width = '50px',
#                   className = 'microphoneSurvey',
#                   render = JS(
#                     "function(data, type, row, meta) {",
#                     "return type === 'display' && data && data.length > 30 ?",
#                     "data.substr(0, 30) + '...' : data;",
#                     "}"
#                   )
#                 ),
#                 list(
#                   targets = c(2),
#                   render = JS(
#                     "function(data, type, row, meta) {",
#                     "return type === 'display' && data && data.length > 6 ?",
#                     "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
#                     "}"
#                   ),
#                   className = 'information-control1'
#                 ),
#                 list(
#                   targets = c(3),
#                   render = JS(
#                     "function(data, type, row, meta) {",
#                     "return type === 'display' && data && data.length > 6 ?",
#                     "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
#                     "}"
#                   ),
#                   className = 'information-control2'
#                 ),
#                 list(
#                   targets = c(4),
#                   render = JS(
#                     "function(data, type, row, meta) {",
#                     "return type === 'display' && data && data.length > 6 ?",
#                     "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
#                     "}"
#                   ),
#                   className = 'information-control3'
#                 ),
#                 list(
#                   width = '50px',
#                   targets = c(8),
#                   className = 'dt-center'
#                 ),
#                 list(
#                   width = '20px',
#                   padding = '0px',
#                   targets = c(9:15, 18:29, 34:52),
#                   className = 'dt-center'
#                 )
#               )
#             ),
#             callback = JS(data_table_call_back)) %>%
#     formatStyle(names(dt),color = 'black', lineHeight="15px") %>%
#     formatStyle(names(dt)[-1],
#                 'Pavlovia session ID',
#                 backgroundColor = styleEqual(participants, random_rgb(length(participants)))) %>%
#     formatStyle(names(dt)[1],
#                 'Prolific participant ID',
#                 backgroundColor = styleEqual(prolific_id, random_rgb(length(prolific_id))))
# }
render_summary_datatable <- function(dt, participants, prolific_id) {
  datatable(
    dt,
    class = list(stripe = FALSE, 'compact'),
    selection = 'none',
    extensions = 'FixedHeader',
    filter = "top",
    escape = FALSE,
    options = list(
      autoWidth = TRUE,
      paging = FALSE,
      scrollX = TRUE,
      fixedHeader = TRUE,
      dom = 'lrtip',
      language = list(info = 'Showing _TOTAL_ entries',
                      infoFiltered =  "(filtered from _MAX_ entries)"),
      columnDefs = list(
        list(visible = FALSE, targets = c(0, 53)),
        list(
          targets = c(16),
          width = '100px',
          className = 'errorC-control',
          render = JS(
            "function(data, type, row, meta) {",
            "  return type === 'display' && data && data.length > 30 ?",
            "    data.substr(0, 30) + '...' : data;",
            "}"
          )
        ),
        list(
          targets = c(17),
          width = '100px',
          className = 'warnC-control',
          render = JS(
            "function(data, type, row, meta) {",
            "  return type === 'display' && data && data.length > 30 ?",
            "    data.substr(0, 30) + '...' : data;",
            "}"
          )
        ),
        list(
          targets = c(31),
          width = '50px',
          className = 'computer51Deg',
          render = JS(
            "function(data, type, row, meta) {",
            "  return type === 'display' && data && data.length > 30 ?",
            "    data.substr(0, 30) + '...' : data;",
            "}"
          )
        ),
        list(
          targets = c(32),
          width = '50px',
          className = 'loudspeakerSurvey',
          render = JS(
            "function(data, type, row, meta) {",
            "  return type === 'display' && data && data.length > 30 ?",
            "    data.substr(0, 30) + '...' : data;",
            "}"
          )
        ),
        list(
          targets = c(33),
          width = '50px',
          className = 'microphoneSurvey',
          render = JS(
            "function(data, type, row, meta) {",
            "  return type === 'display' && data && data.length > 30 ?",
            "    data.substr(0, 30) + '...' : data;",
            "}"
          )
        ),
        list(
          targets = c(2),
          render = JS(
            "function(data, type, row, meta) {",
            "  return type === 'display' && data && data.length > 6 ?",
            "    '<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
            "}"
          ),
          className = 'information-control1'
        ),
        list(
          targets = c(3),
          render = JS(
            "function(data, type, row, meta) {",
            "  return type === 'display' && data && data.length > 6 ?",
            "    '<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
            "}"
          ),
          className = 'information-control2'
        ),
        list(
          targets = c(4),
          render = JS(
            "function(data, type, row, meta) {",
            "  return type === 'display' && data && data.length > 6 ?",
            "    '<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
            "}"
          ),
          className = 'information-control3'
        ),
        list(
          width = '50px',
          targets = c(8),
          className = 'dt-center'
        ),
        list(
          width = '20px',
          padding = '0px',
          # Update the range so it only covers valid column indices (9:15, 18:29, and 34:43)
          targets = c(9:15, 18:29, 34:43),
          className = 'dt-center'
        )
      )
    ),
    callback = JS(data_table_call_back)
  ) %>%
    formatStyle(names(dt), color = 'black', lineHeight = "15px") %>%
    formatStyle(names(dt)[-1],
                'Pavlovia session ID',
                backgroundColor = styleEqual(participants, random_rgb(length(participants)))) %>%
    formatStyle(names(dt)[1],
                'Prolific participant ID',
                backgroundColor = styleEqual(prolific_id, random_rgb(length(prolific_id))))
}
