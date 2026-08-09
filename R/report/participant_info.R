# Distance / Sessions shared participant fields.
# Distance-only geometry/object fields are joined onto summary_table;
# Distance and Stats derive a participant-level view via participant_info_from_summary().
library(dplyr)

build_distance_participant_fields <- function(data_list) {
  participant_info_list <- list()

  for (i in 1:length(data_list)) {
    has_objectName <- "objectName" %in% names(data_list[[i]])

    temp_data <- data_list[[i]] %>%
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
      temp_data <- temp_data %>% mutate(objectName = NA_character_)
    } else {
      temp_data <- temp_data %>% mutate(objectName = as.character(objectName))
    }

    temp_data <- temp_data %>%
      select(participant, pxPerCm,
             rulerLength, rulerUnit,
             calibrateTrackDistance, distanceObjectCm, objectName)

    participant_info_list[[i]] <- temp_data
  }

  geometry <- do.call(rbind, participant_info_list) %>%
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

  participant_qa_list <- list()

  for (i in 1:length(data_list)) {
    temp_qa <- data_list[[i]] %>%
      filter(!is.na(questionAndAnswerNickname),
             questionAndAnswerNickname %in% c("COMMENT", "OBJCT")) %>%
      distinct(participant, questionAndAnswerNickname, questionAndAnswerResponse, questionAndAnswerQuestion)

    if (nrow(temp_qa) > 0) {
      participant_qa_list[[i]] <- temp_qa
    }
  }

  if (length(participant_qa_list) > 0) {
    participant_qa <- do.call(rbind, participant_qa_list) %>%
      distinct()
  } else {
    participant_qa <- tibble(
      participant = character(),
      questionAndAnswerNickname = character(),
      questionAndAnswerResponse = character(),
      questionAndAnswerQuestion = character()
    )
  }

  # OBJCT only here — Sessions already has lowercase `comment` from COMMENT.
  objects_data <- participant_qa %>%
    filter(questionAndAnswerNickname == "OBJCT") %>%
    distinct(participant, questionAndAnswerResponse) %>%
    rename(Object = questionAndAnswerResponse)

  object_name_from_json <- tibble(participant = character(), ObjectFromJSON = character())
  for (i in 1:length(data_list)) {
    if ("distanceCalibrationTJSON" %in% names(data_list[[i]])) {
      tryCatch({
        participant_id <- first(na.omit(data_list[[i]]$participant))
        if (is.na(participant_id) || participant_id == "") next

        raw_json <- data_list[[i]]$distanceCalibrationTJSON
        raw_json <- raw_json[!is.na(raw_json) & raw_json != ""]
        if (length(raw_json) == 0) next

        json_txt <- raw_json[1]
        json_txt <- trimws(as.character(json_txt))
        if (nchar(json_txt) >= 2 && substr(json_txt, 1, 1) == '"' && substr(json_txt, nchar(json_txt), nchar(json_txt)) == '"') {
          json_txt <- substr(json_txt, 2, nchar(json_txt) - 1)
        }
        json_txt <- gsub('""', '"', json_txt, fixed = TRUE)
        json_txt <- gsub("\\\\n", " ", json_txt)
        json_txt <- gsub("\\\\t", " ", json_txt)
        json_txt <- gsub("\n", " ", json_txt)
        json_txt <- gsub("\t", " ", json_txt)

        parsed_json <- jsonlite::fromJSON(json_txt, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)

        if (!is.null(parsed_json$COMMON) && !is.null(parsed_json$COMMON$objectName)) {
          obj_name <- parsed_json$COMMON$objectName
          if (length(obj_name) > 0 && !is.na(obj_name[1]) && obj_name[1] != "") {
            object_name_from_json <- rbind(
              object_name_from_json,
              tibble(participant = participant_id, ObjectFromJSON = obj_name[1])
            )
          }
        }
      }, error = function(e) {
        # Skip if JSON parsing fails
      })
    }
  }

  if (nrow(object_name_from_json) > 0) {
    objects_data <- objects_data %>%
      full_join(object_name_from_json, by = "participant") %>%
      mutate(Object = ifelse(is.na(Object) | Object == "", ObjectFromJSON, Object)) %>%
      select(participant, Object)
  }

  object_suggestion_from_json <- tibble(participant = character(), objectSuggestion = character())
  for (i in 1:length(data_list)) {
    json_col <- if ("distanceCalibrationJSON" %in% names(data_list[[i]])) {
      "distanceCalibrationJSON"
    } else if ("distanceCalibrationTJSON" %in% names(data_list[[i]])) {
      "distanceCalibrationTJSON"
    } else {
      NULL
    }

    if (!is.null(json_col)) {
      tryCatch({
        participant_id <- first(na.omit(data_list[[i]]$participant))
        if (is.na(participant_id) || participant_id == "") next

        raw_json <- data_list[[i]][[json_col]]
        raw_json <- raw_json[!is.na(raw_json) & raw_json != ""]
        if (length(raw_json) == 0) next

        json_txt <- raw_json[1]
        json_txt <- trimws(as.character(json_txt))
        if (nchar(json_txt) >= 2 && substr(json_txt, 1, 1) == '"' && substr(json_txt, nchar(json_txt), nchar(json_txt)) == '"') {
          json_txt <- substr(json_txt, 2, nchar(json_txt) - 1)
        }
        json_txt <- gsub('""', '"', json_txt, fixed = TRUE)
        json_txt <- gsub("\\\\n", " ", json_txt)
        json_txt <- gsub("\\\\t", " ", json_txt)
        json_txt <- gsub("\n", " ", json_txt)
        json_txt <- gsub("\t", " ", json_txt)

        parsed_json <- jsonlite::fromJSON(json_txt, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)

        if (!is.null(parsed_json$objectSuggestion)) {
          obj_suggestion <- parsed_json$objectSuggestion
          non_empty_suggestions <- obj_suggestion[!is.na(obj_suggestion) & obj_suggestion != ""]
          if (length(non_empty_suggestions) > 0) {
            object_suggestion_from_json <- rbind(
              object_suggestion_from_json,
              tibble(participant = participant_id, objectSuggestion = non_empty_suggestions[1])
            )
          } else {
            object_suggestion_from_json <- rbind(
              object_suggestion_from_json,
              tibble(participant = participant_id, objectSuggestion = "")
            )
          }
        }
      }, error = function(e) {
        # Skip if JSON parsing fails
      })
    }
  }

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

enrich_summary_with_distance_fields <- function(summary_df, data_list = NULL, distance_cols = NULL) {
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(summary_df)
  }
  if (is.null(distance_cols)) {
    if (is.null(data_list) || length(data_list) == 0) {
      distance_cols <- tibble(
        participant = character(),
        rulerCm = numeric(),
        pxPerCm = numeric(),
        objectLengthCm = numeric(),
        Object = character(),
        objectSuggestion = character()
      )
    } else {
      distance_cols <- build_distance_participant_fields(data_list)
    }
  }
  summary_df <- summary_df %>%
    select(-any_of(c("rulerCm", "pxPerCm", "objectLengthCm", "Object", "objectSuggestion")))
  if (is.null(distance_cols) || !is.data.frame(distance_cols) ||
      nrow(distance_cols) == 0 || !"participant" %in% names(distance_cols)) {
    return(
      summary_df %>%
        mutate(
          rulerCm = NA_real_,
          pxPerCm = NA_real_,
          objectLengthCm = NA_real_,
          Object = NA_character_,
          objectSuggestion = NA_character_
        )
    )
  }
  # Participant-level Distance fields repeated on each Sessions (block) row.
  summary_df %>%
    left_join(distance_cols, by = c("Pavlovia session ID" = "participant"))
}

# Participant-level Distance/Stats view derived from enriched Sessions summary.
# Pass exclude_participant_ids from short_ruler_participant_ids() — do not recompute minRulerCm here.
participant_info_from_summary <- function(sessions_summary, exclude_participant_ids = NULL) {
  empty <- tibble(
    ok = character(),
    PavloviaParticipantID = character(),
    `Prolific Participant ID` = character(),
    `device type` = character(),
    system = character(),
    browser = character(),
    `Prolific min` = character(),
    screenWidthCm = numeric(),
    screenResolutionXY = character(),
    rulerCm = numeric(),
    pxPerCm = numeric(),
    objectLengthCm = numeric(),
    Object = character(),
    objectSuggestion = character(),
    Comment = character()
  )
  if (is.null(sessions_summary) || nrow(sessions_summary) == 0) {
    return(empty)
  }

  needed <- c(
    "Pavlovia session ID", "device type", "Prolific min", "system", "browser", "ok",
    "screenWidthCm", "resolution"
  )
  missing <- setdiff(needed, names(sessions_summary))
  if (length(missing) > 0) {
    stop("sessions_summary missing columns: ", paste(missing, collapse = ", "))
  }

  st <- sessions_summary
  if (!"Prolific participant ID" %in% names(st)) st$`Prolific participant ID` <- NA_character_
  if (!"rulerCm" %in% names(st)) st$rulerCm <- NA_real_
  if (!"pxPerCm" %in% names(st)) st$pxPerCm <- NA_real_
  if (!"objectLengthCm" %in% names(st)) st$objectLengthCm <- NA_real_
  if (!"Object" %in% names(st)) st$Object <- NA_character_
  if (!"objectSuggestion" %in% names(st)) st$objectSuggestion <- NA_character_
  if (!"comment" %in% names(st)) st$comment <- NA_character_

  out <- st %>%
    transmute(
      ok = ok,
      PavloviaParticipantID = `Pavlovia session ID`,
      `Prolific Participant ID` = `Prolific participant ID`,
      `device type` = `device type`,
      system = system,
      browser = browser,
      `Prolific min` = `Prolific min`,
      screenWidthCm = ifelse(
        !is.na(screenWidthCm) & is.finite(as.numeric(screenWidthCm)),
        round(as.numeric(screenWidthCm), 1),
        NA_real_
      ),
      screenResolutionXY = resolution,
      rulerCm = rulerCm,
      pxPerCm = pxPerCm,
      objectLengthCm = objectLengthCm,
      Object = Object,
      objectSuggestion = objectSuggestion,
      Comment = comment
    ) %>%
    distinct(PavloviaParticipantID, .keep_all = TRUE) %>%
    mutate(
      ok_priority = case_when(
        ok == "✅" ~ 1,
        ok == "🚧" ~ 2,
        ok == "❌" ~ 3,
        is.na(ok) ~ 4,
        .default = 5
      )
    ) %>%
    arrange(ok_priority, PavloviaParticipantID) %>%
    select(-ok_priority)

  if (!is.null(exclude_participant_ids) && length(exclude_participant_ids) > 0) {
    out <- out %>%
      filter(!PavloviaParticipantID %in% exclude_participant_ids)
  }
  out
}

# Single source of short-ruler exclusions (finite rulerCm below minRulerCm).
# NA ruler lengths are kept (not treated as short).
short_ruler_participant_ids <- function(sessions_summary, minRulerCm) {
  if (is.null(sessions_summary) || nrow(sessions_summary) == 0) {
    return(character())
  }
  if (is.null(minRulerCm) || length(minRulerCm) == 0 || is.na(minRulerCm[1])) {
    return(character())
  }
  if (!"rulerCm" %in% names(sessions_summary) || !"Pavlovia session ID" %in% names(sessions_summary)) {
    return(character())
  }
  sessions_summary %>%
    filter(!is.na(as.numeric(rulerCm)), as.numeric(rulerCm) < as.numeric(minRulerCm)[1]) %>%
    distinct(`Pavlovia session ID`) %>%
    pull(`Pavlovia session ID`)
}
