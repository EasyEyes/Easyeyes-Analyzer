library(dplyr)
library(DT)
source("R/utils/utility.R")
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
  required <- c(
    "participant", "date",
    "targetMeasuredLatenessSec", "targetMeasuredDurationSec", "targetDurationSec"
  )
  if (is.null(all_files) || nrow(all_files) == 0 || !all(required %in% names(all_files))) {
    return(empty_lateness_duration())
  }

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
  
  if (nrow(t) == 0) {
    return(empty_lateness_duration())
  }

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

empty_comments_data <- function() {
  tibble(
    participant = character(),
    date = character(),
    comment = character()
  )
}

ensure_comments_joinable <- function(df) {
  if (all(c("participant", "date") %in% names(df))) {
    if (!"comment" %in% names(df)) {
      df$comment <- NA_character_
    }
    return(df)
  }
  empty_comments_data()
}

empty_all_files <- function() {
  tibble(
    participant = character(),
    date = character(),
    block = numeric(),
    block_condition = character(),
    conditionName = character(),
    trial = integer(),
    targetMeasuredLatenessSec = numeric(),
    targetMeasuredDurationSec = character(),
    targetDurationSec = numeric(),
    error = character(),
    warning = character()
  )
}

ensure_all_files_schema <- function(df) {
  required <- c(
    "participant", "date", "block_condition", "trial",
    "targetMeasuredLatenessSec", "targetMeasuredDurationSec", "targetDurationSec",
    "error", "warning"
  )
  if (all(required %in% names(df))) {
    return(df)
  }
  empty_all_files()
}

empty_params <- function() {
  tibble(
    participant = character(),
    date = character(),
    `heapTotalAfterDrawing (MB)` = numeric(),
    `heapLimitAfterDrawing (MB)` = numeric(),
    deviceMemoryGB = numeric(),
    mustTrackSec = numeric()
  )
}

ensure_params_schema <- function(df) {
  required <- c(
    "participant", "date", "deviceMemoryGB", "mustTrackSec",
    "heapTotalAfterDrawing (MB)", "heapLimitAfterDrawing (MB)"
  )
  if (all(required %in% names(df))) {
    return(df)
  }
  empty_params()
}

empty_log_font <- function() {
  tibble(
    `Pavlovia session ID` = character(),
    `_logFontBool` = logical()
  )
}

ensure_log_font_joinable <- function(df) {
  if ("Pavlovia session ID" %in% names(df)) {
    return(df)
  }
  empty_log_font()
}

empty_error_warnings <- function() {
  tibble(
    participant = character(),
    date = character(),
    error = character(),
    warning = character()
  )
}

empty_lateness_duration <- function() {
  tibble(
    participant = character(),
    date = character(),
    tardyMs = numeric(),
    excessMs = numeric()
  )
}

sanitize_embedded_json_text <- function(json_txt) {
  json_txt <- trimws(as.character(json_txt))
  if (nchar(json_txt) >= 2 && substr(json_txt, 1, 1) == '"' && substr(json_txt, nchar(json_txt), nchar(json_txt)) == '"') {
    json_txt <- substr(json_txt, 2, nchar(json_txt) - 1)
  }
  json_txt <- gsub('""', '"', json_txt, fixed = TRUE)
  json_txt <- gsub("\\\\n", " ", json_txt)
  json_txt <- gsub("\\\\t", " ", json_txt)
  json_txt <- gsub("\n", " ", json_txt)
  json_txt <- gsub("\t", " ", json_txt)
  json_txt
}

extract_webgl_row_from_df <- function(df) {
  if (!"WebGL_Report" %in% names(df)) {
    return(NULL)
  }
  json_candidates <- unique(df$WebGL_Report[!is.na(df$WebGL_Report)])
  if (length(json_candidates) == 0) {
    return(NULL)
  }
  json_txt <- as.character(json_candidates[1])
  json_txt <- trimws(json_txt)
  if (nchar(json_txt) >= 2 && substr(json_txt, 1, 1) == '"' && substr(json_txt, nchar(json_txt), nchar(json_txt)) == '"') {
    json_txt <- substr(json_txt, 2, nchar(json_txt) - 1)
  }
  if (grepl('""', json_txt, fixed = TRUE)) {
    json_txt <- gsub('""', '"', json_txt, fixed = TRUE)
  }
  t <- tryCatch(jsonlite::fromJSON(json_txt), error = function(e) NULL)
  if (is.null(t)) {
    return(NULL)
  }
  if ("maxTextureSize" %in% names(t)) {
    data.frame(
      participant = df$participant[1],
      date = df$date[1],
      WebGLVersion = t$WebGL_Version,
      maxTextureSize = t$maxTextureSize,
      maxViewportSize = max(unlist(t$maxViewportSize)),
      WebGLUnmaskedRenderer = t$Unmasked_Renderer,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      participant = df$participant[1],
      date = df$date[1],
      WebGLVersion = ifelse("WebGL_Version" %in% names(t), t$WebGL_Version, ""),
      maxTextureSize = ifelse("Max_Texture_Size" %in% names(t), t$Max_Texture_Size, ""),
      maxViewportSize = ifelse("Max_Viewport_Dims" %in% names(t), max(unlist(t$Max_Viewport_Dims)), ""),
      WebGLUnmaskedRenderer = ifelse("Unmasked_Renderer" %in% names(t), max(unlist(t$Unmasked_Renderer)), ""),
      stringsAsFactors = FALSE
    )
  }
}

finalize_webgl_chunks <- function(webgl_chunks) {
  webGL <- bind_rows_or_empty(webgl_chunks)
  if (is.null(webGL) || nrow(webGL) == 0) {
    return(tibble(
      participant = "",
      WebGLVersion = NA,
      maxTextureSize = NA,
      maxViewportSize = NA,
      WebGLUnmaskedRenderer = NA,
      date = NA
    ))
  }
  tibble(
    participant = webGL$participant,
    date = webGL$date,
    WebGLVersion = webGL$WebGLVersion,
    maxTextureSize = as.numeric(webGL$maxTextureSize),
    maxViewportSize = as.numeric(webGL$maxViewportSize),
    WebGLUnmaskedRenderer = webGL$WebGLUnmaskedRenderer
  )
}

# One walk over data_list collecting every Sessions/Distance input chunk.
collect_summary_table_inputs <- function(data_list) {
  font_chunks <- list()
  params_chunks <- list()
  comment_chunks <- list()
  all_files_chunks <- list()
  logfont_chunks <- list()
  session_chunks <- list()
  webgl_chunks <- list()
  distance_geometry_chunks <- list()
  distance_qa_chunks <- list()
  object_name_json_chunks <- list()
  object_suggestion_json_chunks <- list()

  if (length(data_list) == 0) {
    return(list(
      font = font_chunks,
      params = params_chunks,
      comments = comment_chunks,
      all_files = all_files_chunks,
      logFont = logfont_chunks,
      sessions = session_chunks,
      webGL = webgl_chunks,
      distance_geometry = distance_geometry_chunks,
      distance_qa = distance_qa_chunks,
      object_name_json = object_name_json_chunks,
      object_suggestion_json = object_suggestion_json_chunks
    ))
  }

  for (i in seq_along(data_list)) {
    df <- data_list[[i]]
    if (is.null(df) || nrow(df) < 1) {
      next
    }

    # --- fontParams ---
    df_bc <- normalize_block_condition_col(df)
    font_t <- df_bc %>%
      mutate(
        block_condition = as.character(ifelse(
          is.na(block_condition) | block_condition == "",
          as.character(staircaseName),
          block_condition
        ))
      ) %>%
      filter(!is.na(fontSizePx) | !is.na(viewingDistanceCm)) %>%
      select(participant, date, block_condition, fontSizePx, viewingDistanceCm, fontRenderMaxPx, fontMaxPx)
    if (nrow(font_t) > 0) {
      font_chunks[[length(font_chunks) + 1]] <- font_t %>%
        group_by(participant, date, block_condition) %>%
        summarize(
          fontSizePx = round(mean(as.numeric(fontSizePx), na.rm = TRUE), 1),
          viewingDistanceCm = round(mean(as.numeric(viewingDistanceCm), na.rm = TRUE), 1),
          fontRenderMaxPx = mean(as.numeric(fontRenderMaxPx), na.rm = TRUE),
          fontMaxPx = mean(as.numeric(fontMaxPx), na.rm = TRUE),
          .groups = "drop"
        )
    }

    # --- params ---
    params_chunks[[length(params_chunks) + 1]] <- df %>%
      filter(!is.na(staircaseName) & staircaseName != "") %>%
      select(
        participant,
        date,
        `heapTotalAfterDrawing (MB)`,
        `heapLimitAfterDrawing (MB)`,
        deviceMemoryGB,
        mustTrackSec
      )

    # --- COMMENT (Sessions) + OBJCT (Distance) ---
    if ("questionAndAnswerNickname" %in% names(df)) {
      qa <- df %>%
        filter(!is.na(questionAndAnswerNickname)) %>%
        distinct(
          participant, date, questionAndAnswerNickname,
          questionAndAnswerResponse, questionAndAnswerQuestion
        )
      comment_rows <- qa %>%
        filter(questionAndAnswerNickname == "COMMENT")
      if (nrow(comment_rows) > 0) {
        comment_chunks[[length(comment_chunks) + 1]] <- comment_rows %>%
          mutate(date = as.character(date)) %>%
          rename(comment = questionAndAnswerResponse)
      }
      objct_rows <- qa %>%
        filter(questionAndAnswerNickname == "OBJCT") %>%
        distinct(participant, questionAndAnswerNickname, questionAndAnswerResponse, questionAndAnswerQuestion)
      if (nrow(objct_rows) > 0) {
        distance_qa_chunks[[length(distance_qa_chunks) + 1]] <- objct_rows
      }
    }

    # --- all_files ---
    all_files_chunks[[length(all_files_chunks) + 1]] <- df %>%
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

    # --- logFont ---
    if ("_logFontBool" %in% names(df)) {
      logfont_chunks[[length(logfont_chunks) + 1]] <- df %>%
        distinct(participant, `_logFontBool`) %>%
        filter(`_logFontBool` == TRUE) %>%
        rename(`Pavlovia session ID` = participant)
    }

    # --- sessions row (ok assigned later once errors are known) ---
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
    t$experimentCompleteBool <- experimentCompleteBool
    session_chunks[[length(session_chunks) + 1]] <- t

    # --- WebGL ---
    webgl_row <- extract_webgl_row_from_df(df)
    if (!is.null(webgl_row)) {
      webgl_chunks[[length(webgl_chunks) + 1]] <- webgl_row
    }

    # --- Distance geometry ---
    has_objectName <- "objectName" %in% names(df)
    geom <- df %>%
      select(participant, pxPerCm,
             rulerLength, rulerUnit,
             calibrateTrackDistance, distanceObjectCm,
             any_of("objectName")) %>%
      distinct() %>%
      filter(!is.na(participant)) %>%
      mutate(
        rulerLength = as.numeric(rulerLength),
        rulerUnit = as.character(rulerUnit),
        calibrateTrackDistance = as.character(calibrateTrackDistance),
        distanceObjectCm = as.numeric(distanceObjectCm)
      )
    if (!has_objectName) {
      geom <- geom %>% mutate(objectName = NA_character_)
    } else {
      geom <- geom %>% mutate(objectName = as.character(objectName))
    }
    distance_geometry_chunks[[length(distance_geometry_chunks) + 1]] <- geom %>%
      select(participant, pxPerCm, rulerLength, rulerUnit, calibrateTrackDistance, distanceObjectCm, objectName)

    participant_id <- first(na.omit(df$participant))
    if (!is.na(participant_id) && participant_id != "") {
      # objectName from distanceCalibrationTJSON.COMMON
      if ("distanceCalibrationTJSON" %in% names(df)) {
        tryCatch({
          raw_json <- df$distanceCalibrationTJSON
          raw_json <- raw_json[!is.na(raw_json) & raw_json != ""]
          if (length(raw_json) > 0) {
            parsed_json <- jsonlite::fromJSON(
              sanitize_embedded_json_text(raw_json[1]),
              simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE
            )
            if (!is.null(parsed_json$COMMON) && !is.null(parsed_json$COMMON$objectName)) {
              obj_name <- parsed_json$COMMON$objectName
              if (length(obj_name) > 0 && !is.na(obj_name[1]) && obj_name[1] != "") {
                object_name_json_chunks[[length(object_name_json_chunks) + 1]] <-
                  tibble(participant = participant_id, ObjectFromJSON = obj_name[1])
              }
            }
          }
        }, error = function(e) NULL)
      }

      json_col <- if ("distanceCalibrationJSON" %in% names(df)) {
        "distanceCalibrationJSON"
      } else if ("distanceCalibrationTJSON" %in% names(df)) {
        "distanceCalibrationTJSON"
      } else {
        NULL
      }
      if (!is.null(json_col)) {
        tryCatch({
          raw_json <- df[[json_col]]
          raw_json <- raw_json[!is.na(raw_json) & raw_json != ""]
          if (length(raw_json) > 0) {
            parsed_json <- jsonlite::fromJSON(
              sanitize_embedded_json_text(raw_json[1]),
              simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE
            )
            if (!is.null(parsed_json$objectSuggestion)) {
              obj_suggestion <- parsed_json$objectSuggestion
              non_empty_suggestions <- obj_suggestion[!is.na(obj_suggestion) & obj_suggestion != ""]
              object_suggestion_json_chunks[[length(object_suggestion_json_chunks) + 1]] <-
                tibble(
                  participant = participant_id,
                  objectSuggestion = if (length(non_empty_suggestions) > 0) non_empty_suggestions[1] else ""
                )
            }
          }
        }, error = function(e) NULL)
      }
    }
  }

  list(
    font = font_chunks,
    params = params_chunks,
    comments = comment_chunks,
    all_files = all_files_chunks,
    logFont = logfont_chunks,
    sessions = session_chunks,
    webGL = webgl_chunks,
    distance_geometry = distance_geometry_chunks,
    distance_qa = distance_qa_chunks,
    object_name_json = object_name_json_chunks,
    object_suggestion_json = object_suggestion_json_chunks
  )
}

empty_distance_cols <- function() {
  tibble(
    participant = character(),
    rulerCm = numeric(),
    pxPerCm = numeric(),
    objectLengthCm = numeric(),
    Object = character(),
    objectSuggestion = character()
  )
}

empty_objects_data <- function() {
  tibble(participant = character(), Object = character())
}

empty_object_suggestion <- function() {
  tibble(participant = character(), objectSuggestion = character())
}

# bind_rows_or_empty() returns a columnless tibble() when chunks are empty —
# that is not NULL, so joins on participant still fail unless we restore schema.
ensure_join_cols <- function(df, required_cols, empty_factory) {
  if (is.null(df) || !is.data.frame(df) || !all(required_cols %in% names(df))) {
    return(empty_factory())
  }
  df
}

assemble_distance_cols_from_chunks <- function(chunks) {
  geometry <- bind_rows_or_empty(chunks$distance_geometry)
  geometry <- ensure_join_cols(geometry, "participant", empty_distance_cols)
  if (nrow(geometry) == 0) {
    return(empty_distance_cols())
  }

  geometry <- geometry %>%
    distinct() %>%
    mutate(
      rulerCm = case_when(
        !is.na(rulerLength) & rulerUnit == "cm" ~ rulerLength,
        !is.na(rulerLength) & rulerUnit == "inches" ~ rulerLength * 2.54,
        .default = NA_real_
      )
    ) %>%
    group_by(participant) %>%
    summarize(
      rulerCm = first(rulerCm[!is.na(rulerCm)]),
      calibrateTrackDistance = first(calibrateTrackDistance[!is.na(calibrateTrackDistance)]),
      distanceObjectCm = first(distanceObjectCm[!is.na(distanceObjectCm)]),
      pxPerCm = first(pxPerCm[!is.na(pxPerCm)]),
      objectName = first(objectName[!is.na(objectName) & objectName != ""]),
      .groups = "drop"
    )

  participant_qa <- bind_rows_or_empty(chunks$distance_qa)
  if (is.null(participant_qa) || nrow(participant_qa) == 0 ||
      !all(c("participant", "questionAndAnswerNickname", "questionAndAnswerResponse") %in% names(participant_qa))) {
    objects_data <- empty_objects_data()
  } else {
    objects_data <- participant_qa %>%
      filter(questionAndAnswerNickname == "OBJCT") %>%
      distinct(participant, questionAndAnswerResponse) %>%
      rename(Object = questionAndAnswerResponse)
    objects_data <- ensure_join_cols(objects_data, c("participant", "Object"), empty_objects_data)
  }

  object_name_from_json <- bind_rows_or_empty(chunks$object_name_json)
  if (!is.null(object_name_from_json) &&
      nrow(object_name_from_json) > 0 &&
      all(c("participant", "ObjectFromJSON") %in% names(object_name_from_json))) {
    objects_data <- objects_data %>%
      full_join(object_name_from_json, by = "participant") %>%
      mutate(Object = ifelse(is.na(Object) | Object == "", ObjectFromJSON, Object)) %>%
      select(participant, Object)
  }

  object_suggestion_from_json <- ensure_join_cols(
    bind_rows_or_empty(chunks$object_suggestion_json),
    c("participant", "objectSuggestion"),
    empty_object_suggestion
  )

  geometry %>%
    left_join(objects_data, by = "participant") %>%
    left_join(object_suggestion_from_json, by = "participant") %>%
    mutate(
      objectLengthCm = ifelse(
        !is.na(distanceObjectCm) & is.finite(distanceObjectCm),
        round(distanceObjectCm, 1),
        NA_real_
      ),
      rulerCm = case_when(
        !is.na(rulerCm) ~ round(as.numeric(rulerCm), 0),
        .default = NA_real_
      ),
      pxPerCm = ifelse(!is.na(pxPerCm), round(as.numeric(pxPerCm), 1), NA_real_),
      Object = ifelse(
        is.na(Object) | Object == "",
        ifelse(!is.na(objectName) & objectName != "", objectName, NA_character_),
        Object
      )
    ) %>%
    select(participant, rulerCm, pxPerCm, objectLengthCm, Object, objectSuggestion)
}

generate_summary_table <- function(data_list, stairs, pretest, prolific) {
  chunks <- collect_summary_table_inputs(data_list)

  fontParams <- bind_rows_or_empty(chunks$font) %>%
    ensure_font_params_joinable()

  params <- bind_rows_or_empty(chunks$params) %>%
    ensure_params_schema()

  NQuestTrials <- stairs %>%
    arrange(participant, date, staircaseName) %>%
    group_by(participant, date, staircaseName) %>%
    summarize(
      goodTrials = sum(trialGivenToQuest, na.rm = T),
      badTrials = sum(!trialGivenToQuest, na.rm = T),
      .groups = "drop"
    ) %>%
    group_by(participant, date) %>%
    summarize(
      goodTrials = format(round(mean(goodTrials), 2), nsmall = 2),
      badTrials = format(round(mean(badTrials), 2), nsmall = 2),
      .groups = "drop"
    ) %>%
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
      .groups = "drop"
    ) %>%
    mutate(date = as.character(date)) %>%
    left_join(NQuestTrials, by = c("participant", "date"), relationship = "many-to-many") %>%
    rename("Pavlovia session ID" = "participant")

  webGL <- finalize_webgl_chunks(chunks$webGL) %>%
    rename("Pavlovia session ID" = "participant")

  comments_data <- bind_rows_or_empty(chunks$comments) %>%
    ensure_comments_joinable()

  all_files <- bind_rows_or_empty(chunks$all_files) %>%
    ensure_all_files_schema()

  logFont <- bind_rows_or_empty(chunks$logFont) %>%
    ensure_log_font_joinable()

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

  error <- all_files %>%
    dplyr::filter(error != "" & error != "Incomplete") %>%
    group_by(participant, date) %>%
    summarize(error = paste(error, collapse = "<br>"),
              .groups = "drop")
  if (!all(c("participant", "date") %in% names(error))) {
    error <- empty_error_warnings() %>% select(participant, date, error)
  }

  warnings <- all_files %>%
    dplyr::filter(warning != "") %>%
    group_by(participant, date) %>%
    summarize(warning = paste(warning, collapse = "<br>"),
              .groups = "drop")
  if (!all(c("participant", "date") %in% names(warnings))) {
    warnings <- empty_error_warnings() %>% select(participant, date, warning)
  }

  sessions <- bind_rows_or_empty(chunks$sessions)
  if (!is.null(sessions) && nrow(sessions) > 0) {
    error_participants <- if (nrow(error) > 0) unique(error$participant) else character()
    sessions <- sessions %>%
      mutate(
        ok = dplyr::case_when(
          participant %in% error_participants ~ emoji("x"),
          !experimentCompleteBool ~ emoji("construction"),
          TRUE ~ emoji("white_check_mark")
        )
      ) %>%
      select(-experimentCompleteBool)
  }

  summary_df <- sessions %>%
    distinct() %>%
    left_join(error, by = c("participant", "date")) %>%
    left_join(warnings, by = c("participant", "date")) %>%
    mutate(ok = factor(ok, levels = c(
      emoji("x"),
      emoji("construction"),
      emoji("white_check_mark")
    ))) %>%
    left_join(lateness_duration, by = c("participant", "date")) %>%
    left_join(trial, by = c("participant", "date", "block_condition")) %>%
    left_join(fontParams, by = c("participant", "date", "block_condition")) %>%
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
      "KB" = "kb",
      "unmetNeeds" = "_needsUnmet",
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
    left_join(logFont, by = c("Pavlovia session ID")) %>%
    left_join(webGL, by = c("Pavlovia session ID", "date"), relationship = "many-to-many") %>%
    mutate(deviceMemoryGB_preserved = deviceMemoryGB) %>%
    left_join(params, by = c("Pavlovia session ID", "date")) %>%
    mutate(deviceMemoryGB = deviceMemoryGB_preserved) %>%
    select(-deviceMemoryGB_preserved) %>%
    rename("GB" = "deviceMemoryGB") %>%
    mutate(date = parse_date_time(str_remove(date, " UTC[+-]\\d+"),
                                  orders = c("ymdHMS", "mdyHMS"))) %>%
    mutate(date = format(date, "%b %d, %Y, %H:%M:%S"))

  summary_df <- summary_df %>%
    mutate(snapshotsLink = ifelse(
      !is.na(snapshotsLink) & snapshotsLink != "",
      paste0('<a href="', snapshotsLink, '" target="_blank">snapshots</a>'),
      ""
    ))

  final_summary_table <- combineProlific(prolific, summary_df, pretest)
  distance_cols <- assemble_distance_cols_from_chunks(chunks)
  final_summary_table <- enrich_summary_with_distance_fields(
    final_summary_table,
    distance_cols = distance_cols
  )

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
