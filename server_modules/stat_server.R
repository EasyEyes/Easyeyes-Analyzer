#### Stats server module ####
statTabServer <- function(id,
                          df_list,
                          experiment_names,
                          mergedParticipantDistanceTable,
                          uploaded_file) {
  moduleServer(id, function(input, output, session) {

    output$filename <- renderText({
      file_info <- uploaded_file()
      if (!is.null(file_info) && length(file_info$datapath) > 0) {
        basename(file_info$name[1])
      }
    })

    output$thresholdSummary <- renderTable({
      req(df_list())
      df_list()$threshold
    })

    output$thresholdAll <- renderTable({
      req(df_list())
      df_list()$threshold_each
    })

    output$QA <- DT::renderDataTable({
      req(df_list())
      df_list()$QA
    })

    output$ratings <- renderTable({
      req(df_list())
      df_list()$ratings
    })

    output$participantInfo <- renderTable({
      req(df_list())
      df_list()$participant_info
    }, width = "100%", spacing = "xs")

    output$thresholdOne <- downloadHandler(
      filename = function() {
        ifelse(
          experiment_names() == "",
          "Summary-of-each-condition.xlsx",
          paste0(get_short_experiment_name(experiment_names()), "-Summary-of-each-condition.xlsx")
        )
      },
      content = function(filename) {
        req(df_list())
        openxlsx::write.xlsx(df_list()$threshold, file = filename)
      }
    )

    output$thresholdTwo <- downloadHandler(
      filename = function() {
        ifelse(
          experiment_names() == "",
          "Thresholds(brief).xlsx",
          paste0(get_short_experiment_name(experiment_names()), "-Thresholds(brief).xlsx")
        )
      },
      content = function(filename) {
        req(df_list())
        openxlsx::write.xlsx(df_list()$threshold_each, file = filename)
      }
    )

    output$thresholdThree <- downloadHandler(
      filename = function() {
        ifelse(
          experiment_names() == "",
          "IndividualResults.xlsx",
          paste0(get_short_experiment_name(experiment_names()), "-IndividualResults.xlsx")
        )
      },
      content = function(filename) {
        req(df_list())
        data <- df_list()$all_summary
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "IndividualResults")
        openxlsx::writeData(wb, "IndividualResults", data)
        decimal_style <- openxlsx::createStyle(numFmt = "0.00")
        rating_cols <- which(names(data) %in% c("Comfort", "Beauty", "Familiarity"))
        for (col_idx in rating_cols) {
          openxlsx::addStyle(
            wb, "IndividualResults", decimal_style,
            rows = 2:(nrow(data) + 1), cols = col_idx, gridExpand = TRUE
          )
        }
        openxlsx::saveWorkbook(wb, file = filename, overwrite = TRUE)
      }
    )

    output$participantInfoExcel <- downloadHandler(
      filename = function() {
        ifelse(
          experiment_names() == "",
          "ParticipantDistanceInfo.xlsx",
          paste0(get_short_experiment_name(experiment_names()), "-ParticipantDistanceInfo.xlsx")
        )
      },
      content = function(filename) {
        openxlsx::write.xlsx(mergedParticipantDistanceTable(), file = filename)
      }
    )
  })
}
