# modules/condition_checkbox.R

conditionCheckboxUI <- function(id) {
  ns <- NS(id)  # namespace function
  fixedRow(
    div(
      style = "background-color: #e6f2ff; padding: 10px; border-radius: 10px; margin: 10px 0;",
      div(
        style = "display: flex; align-items: center; margin-left:12px;",
        checkboxGroupInput(
          inputId = ns("conditionName"),  # namespace the ID
          label = "conditionName",
          inline = TRUE,
          choices = NULL,
          selected = NULL
        )
      )
    )
  )
}


conditionCheckboxServer <- function(id, updateChoices) {
  moduleServer(id, function(input, output, session) {
    observe({
      updateCheckboxGroupInput(session, "conditionName",
                               choices = updateChoices(),
                               selected = updateChoices(),
                               inline = TRUE)
    })
    return(reactive(input$conditionName))
  })
}

