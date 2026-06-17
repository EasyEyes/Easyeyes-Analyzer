library(dplyr)
library(DT)
source('./other/utility.R')
# Each time update the summary table, the rmd report need to be updated accordingly.

data_table_call_back = "
    // error column call back
    table.column(18).nodes().to$().css({cursor: 'pointer'});
    var format1 = function(d) {
      return '<p>' + d[18] + '</p>';
    };
    table.on('click', 'td.errorC-control', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format1(row.data())).show();
      }
    });
     // warning column call back
    table.column(19).nodes().to$().css({cursor: 'pointer'});
    var format2 = function(d) {
      return '<p>' + d[19] + '</p>';
    };
    table.on('click', 'td.warnC-control', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format2(row.data())).show();
      }
    });
    
    // computer51Deg column call back
     table.column(34).nodes().to$().css({cursor: 'pointer'});
    var format8 = function(d) {
      return '<p>' + d[34] + '</p>';
    };
    table.on('click', 'td.computer51Deg', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format8(row.data())).show();
      }
    });

    table.column(35).nodes().to$().css({cursor: 'pointer'});
    var format6 = function(d) {
      return '<p>' + d[35] + '</p>';
    };
    table.on('click', 'td.loudspeakerSurvey', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(format6(row.data())).show();
      }
    });

    table.column(36).nodes().to$().css({cursor: 'pointer'});
    var format5 = function(d) {
      return '<p>' + d[36] + '</p>';
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
    
    table.column(40).nodes().to$().css({cursor: 'pointer'});
    var formatComment = function(d) {
      return '<p>' + d[40] + '</p>';
    };
    table.on('click', 'td.comment', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      console.log(td);
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        row.child(formatComment(row.data())).show();
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
    ) %>% 
    filter(!is.na(date))
  
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
      .groups = "drop"
    ) %>%
    mutate(
      tardyMs = round(targetMeasuredLatenessMeanSec),
      excessMs = round(targetMeasuredDurationMeanSec)
    ) %>%
    select(
      -targetMeasuredLatenessMeanSec,-targetMeasuredLatenessSDSec,-targetMeasuredDurationMeanSec,-targetMeasuredDurationSDSec
    )
  
  return(t)
}

first_nonempty_string <- function(values) {
  values <- values[!is.na(values) & values != ""]
  if (length(values) > 0) {
    return(values[[1]])
  }
  ""
}

normalize_block_condition_col <- function(df) {
  if (!"block_condition" %in% names(df)) {
    df$block_condition <- ""
  }
  df$block_condition <- as.character(df$block_condition)
  df
}

empty_font_params <- function() {
  tibble(
    participant = character(),
    date = character(),
    block_condition = character(),
    fontSizePx = double(),
    viewingDistanceCm = double(),
    fontRenderMaxPx = double(),
    fontMaxPx = double()
  )
}

ensure_font_params_joinable <- function(df) {
  required <- c("participant", "date", "block_condition")
  if (all(required %in% names(df))) {
    return(df)
  }
  empty_font_params()
}

generate_summary_table <- function(data_list, stairs, pretest, prolific) {
  fontParams <- bind_rows_or_empty(lapply(data_list, function(df) {
    if (nrow(df) < 1) {
      return(NULL)
    }
    df <- normalize_block_condition_col(df)
    t <- df %>%
      mutate(
        block_condition = as.character(ifelse(
          is.na(block_condition) | block_condition == "",
          as.character(staircaseName),
          block_condition
        ))
      ) %>%
      filter(!is.na(fontSizePx) | !is.na(viewingDistanceCm)) %>%
      select(participant, date, block_condition, fontSizePx, viewingDistanceCm, fontRenderMaxPx, fontMaxPx)
    if (nrow(t) < 1) {
      return(NULL)
    }
    t %>%
      group_by(participant, date, block_condition) %>%
      summarize(
        fontSizePx = round(mean(as.numeric(fontSizePx), na.rm = TRUE), 1),
        viewingDistanceCm = round(mean(as.numeric(viewingDistanceCm), na.rm = TRUE), 1),
        fontRenderMaxPx = mean(as.numeric(fontRenderMaxPx), na.rm = TRUE),
        fontMaxPx = mean(as.numeric(fontMaxPx), na.rm = TRUE),
        .groups = "drop"
      )
  })) %>%
    ensure_font_params_joinable()

  params <- bind_rows_or_empty(lapply(data_list, function(df) {
    df %>%
      filter(!is.na(staircaseName) & staircaseName != "") %>%
      select(
        participant,
        date,
        `heapTotalAfterDrawing (MB)`,
        `heapLimitAfterDrawing (MB)`,
        deviceMemoryGB,
        mustTrackSec
      )
  }))
  
  NQuestTrials <- stairs %>%
    arrange(participant, date, staircaseName) %>%
    group_by(participant, date, staircaseName) %>%
    summarize(
      goodTrials = sum(trialGivenToQuest, na.rm = T),
      badTrials = sum(!trialGivenToQuest, na.rm = T),
      .groups="drop"
    ) %>%
    group_by(participant, date) %>%
    summarize(goodTrials = format(round(mean(goodTrials), 2), nsmall = 2),
              badTrials = format(round(mean(badTrials), 2), nsmall = 2),
              .groups="drop") %>% 
    mutate(date = as.character(date))

  params <- params %>%
    mutate(
      mustTrackSec = suppressWarnings(as.numeric(mustTrackSec)),
      `heapLimitAfterDrawing (MB)` = suppressWarnings(as.numeric(`heapLimitAfterDrawing (MB)`)),
      `heapTotalAfterDrawing (MB)` = suppressWarnings(as.numeric(`heapTotalAfterDrawing (MB)`))
    ) %>%
    group_by(participant,
             deviceMemoryGB, 
             date) %>%
    summarize(
      mustTrackSec = format(round(mean(mustTrackSec, na.rm = TRUE), 2), nsmall = 2),
      `heapLimitAfterDrawing (MB)` = format(round(
        mean(`heapLimitAfterDrawing (MB)`, na.rm = TRUE), 2
      ), nsmall = 2),
      heapTotalAvgMB = format(round(
        mean(`heapTotalAfterDrawing (MB)`, na.rm = TRUE), 2
      ), nsmall = 2),
      .groups="drop"
    ) %>%
    mutate(date = as.character(date)) %>% 
    left_join(NQuestTrials, by = c('participant', 'date'), relationship = 'many-to-many') %>%
    rename("Pavlovia session ID" = "participant")
    
  
  webGL <- get_webGL(data_list) %>% rename("Pavlovia session ID" = "participant")
  
  comments_data <- bind_rows_or_empty(lapply(data_list, function(df) {
    df %>%
      filter(
        !is.na(questionAndAnswerNickname),
        questionAndAnswerNickname == "COMMENT"
      ) %>%
      distinct(
        participant, date, questionAndAnswerNickname,
        questionAndAnswerResponse, questionAndAnswerQuestion
      ) %>%
      mutate(date = as.character(date)) %>%
      rename(comment = questionAndAnswerResponse)
  }))

  all_files <- bind_rows_or_empty(lapply(data_list, function(df) {
    if (nrow(df) == 0) {
      return(NULL)
    }
    df %>%
      distinct(
        participant,
        date,
        block,
        block_condition,
        conditionName,
        targetMeasuredLatenessSec,
        targetMeasuredDurationSec,
        targetDurationSec,
        error,
        warning
      ) %>%
      group_by(participant, block_condition) %>%
      mutate(trial = n()) %>%
      ungroup() %>%
      distinct(
        participant,
        date,
        block,
        block_condition,
        conditionName,
        trial,
        targetMeasuredLatenessSec,
        targetMeasuredDurationSec,
        targetDurationSec,
        error,
        warning
      )
  }))

  logFont <- bind_rows_or_empty(lapply(data_list, function(df) {
    df %>%
      distinct(participant, `_logFontBool`) %>%
      filter(`_logFontBool` == TRUE) %>%
      rename(`Pavlovia session ID` = participant)
  }))
  
  trial <- all_files %>%
    select(participant, date, block_condition, trial) %>%
    filter(block_condition != "") %>%
    mutate(block_condition = as.character(block_condition))

  if (!all(c("participant", "date", "block_condition") %in% names(trial))) {
    trial <- tibble(
      participant = character(),
      date = character(),
      block_condition = character(),
      trial = integer()
    )
  }
  
  lateness_duration <- get_lateness_and_duration(all_files)
  
  #### errors ####
  error <- all_files %>%
    dplyr::filter(error != "" & error != "Incomplete") %>%
    group_by(participant,date) %>%
    summarize(error = paste(error, collapse = "<br>"),
              .groups = "drop")
  
  #### warnings ####
  warnings <- all_files %>%
    dplyr::filter(warning != "") %>%
    group_by(participant,date) %>%
    summarize(warning = paste(warning, collapse = "<br>"),
              .groups = "drop")
  
  #### Get device and block condition columns ####
  sessions <- bind_rows_or_empty(lapply(data_list, function(df) {
    if (nrow(df) < 1) {
      return(NULL)
    }

    experimentCompleteBool <- {
      val <- df$experimentCompleteBool[[1]]
      if (is.na(val) || length(val) == 0) {
        FALSE
      } else {
        val
      }
    }

    t <- df %>%
      arrange(`Loudspeaker survey`) %>%
      mutate(
        `Loudspeaker survey` = first_nonempty_string(`Loudspeaker survey`),
        `_needsUnmet` = first_nonempty_string(`_needsUnmet`),
        `Microphone survey` = first_nonempty_string(`Microphone survey`),
        QRConnect = first_nonempty_string(QRConnect),
        ComputerInfoFrom51Degrees = first_nonempty_string(ComputerInfoFrom51Degrees)
      ) %>%
      distinct(
        ProlificParticipantID,
        participant,
        ProlificSessionID,
        date,
        deviceType,
        deviceMemoryGB,
        cores,
        deviceSystemFamily,
        browser,
        resolution,
        screenWidthCm,
        cameraIsTopCenter,
        rows,
        cols,
        kb,
        ComputerInfoFrom51Degrees,
        `_needsUnmet`,
        `Loudspeaker survey`,
        `Microphone survey`,
        QRConnect,
        snapshotsLink
      )

    info <- df %>%
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
        block_condition = NA_character_,
        conditionName = NA_character_,
        targetTask = NA_character_,
        targetKind = NA_character_,
        thresholdParameter = NA_character_
      )
    }

    t <- cbind(t, info)

    t$ok <- if (t$participant[[1]] %in% error$participant) {
      emoji("x")
    } else if (!experimentCompleteBool) {
      emoji("construction")
    } else {
      emoji("white_check_mark")
    }

    t
  }))
  
  summary_df <- sessions %>%
    distinct() %>% 
    left_join(error, by = c('participant','date')) %>%
    left_join(warnings, by = c('participant','date')) %>% 
    mutate(ok = factor(ok, levels = c(
      emoji("x"),
      emoji("construction"),
      emoji("white_check_mark")
    ))) %>%
    left_join(lateness_duration, by = c("participant", "date")) %>%
    left_join(trial, by = c("participant", 'date','block_condition')) %>%
    left_join(fontParams, by = c("participant", 'date', 'block_condition')) %>% 
    left_join(comments_data, by = c("participant", "date")) %>% 
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
      'KB' = 'kb',
      "unmetNeeds" = '_needsUnmet',
      "computer51Deg" = "ComputerInfoFrom51Degrees",
      "Loudspeaker" = "Loudspeaker survey",
      "Microphone" = "Microphone survey"
    ) %>%
    distinct(
      `Prolific participant ID`,
      `Pavlovia session ID`,
      ProlificSessionID,
      `device type`,
      deviceMemoryGB,
      system,
      browser,
      resolution,
      screenWidthCm,
      cameraIsTopCenter,
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
      fontRenderMaxPx,
      fontSizePx,
      viewingDistanceCm,
      fontMaxPx,
      Loudspeaker,
      Microphone,
      comment,
      snapshotsLink
    )
  
  #### order block_condition by splitting and order block and condition order ####
  block_condition_parts <- stringr::str_split_fixed(summary_df$`block condition`, "_", 2)
  summary_df <- summary_df %>%
    mutate(
      block_new = suppressWarnings(as.numeric(block_condition_parts[, 1])),
      condition = suppressWarnings(as.numeric(block_condition_parts[, 2]))
    ) %>%
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
    left_join(logFont, by = c('Pavlovia session ID')) %>%
    left_join(webGL, by = c('Pavlovia session ID', 'date'), relationship = 'many-to-many') %>%
    # Preserve deviceMemoryGB from sessions before params join overwrites it
    mutate(deviceMemoryGB_preserved = deviceMemoryGB) %>%
    left_join(params, by = c('Pavlovia session ID', 'date')) %>%
    # Use preserved deviceMemoryGB from sessions since params filtering removes the data
    mutate(deviceMemoryGB = deviceMemoryGB_preserved) %>%
    select(-deviceMemoryGB_preserved) %>%
    rename("GB" = "deviceMemoryGB") %>% 
    mutate(date = parse_date_time(str_remove(date, " UTC[+-]\\d+"),
                                  orders = c('ymdHMS', 'mdyHMS'))) %>%
    mutate(date = format(date, "%b %d, %Y, %H:%M:%S"))
  
  summary_df <- summary_df %>%
    mutate(snapshotsLink = ifelse(
      !is.na(snapshotsLink) & snapshotsLink != "",
      paste0('<a href="', snapshotsLink, '" target="_blank">snapshots</a>'),
      ""
    ))

  final_summary_table <- combineProlific(prolific, summary_df, pretest)

  return(final_summary_table)
}


empty_summary_datatable <- function() {
  datatable(
    data.frame(),
    options = list(dom = "t", searching = FALSE, paging = FALSE, info = FALSE)
  )
}

render_summary_datatable <- function(dt, participants, prolific_id) {
  dt$resolution_width <- as.integer(sub("^\\s*([0-9]+).*", "\\1", dt$resolution))
  
  # compute one‐based indices for DataTables
  res_col   <- which(names(dt) == "resolution")
  width_col <- which(names(dt) == "resolution_width")
  
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
        list(visible = FALSE, targets = c(0, width_col)),  # Hide first column and resolution_width column
        list(
          targets   = res_col,
          orderData = width_col
        ),
        list(
          targets = c(18),
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
          targets = c(19),
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
          targets = c(34),
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
          targets = c(35),
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
          targets = c(36),
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
          targets = c(40),
          width = '50px',
          className = 'comment',
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
