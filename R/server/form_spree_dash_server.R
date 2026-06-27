#### FormSpree dashboard server module ####
formSpreeTabServer <- function(id, app_profiler = NULL) {
  moduleServer(id, function(input, output, session) {
    formSpreeTable <-
      reactive({
        app_profile_time(app_profiler, "FormSpree dashboard table", {
          monitorFormSpree(input$listFontParameters)
        })
      })

    output$formSpreeDashboard <- renderDataTable({
      app_profile_time(app_profiler, "FormSpree dashboard render", {
      datatable(
        formSpreeTable(),
        extensions = 'FixedHeader',
        class = list(stripe = FALSE),
        selection = 'none',
        filter = "top",
        options = list(
          autoWidth = TRUE,
          fixedHeader = TRUE,
          pageLength = 150,
          paging = TRUE,
          # dom = 'lt',
          columnDefs = list(list(
            visible = FALSE, targets = c(0, ncol(formSpreeTable()))
          ))
        )
      ) %>%
        {
          tbl <- .
          if ("hl" %in% names(formSpreeTable())) {
            tbl <- tbl %>% formatStyle(
              names(formSpreeTable()),
              "hl",
              backgroundColor = styleEqual(c(TRUE, FALSE), c("yellow", "white"))
            )
          }
          tbl
        }
      })
    })
  })
}
