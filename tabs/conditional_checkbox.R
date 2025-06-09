# modules/condition_checkbox.R

conditionCheckboxUI <- function(id) {
  ns <- NS(id)  # namespace function
  fixedRow(
    div(
      style = "background-color: #e6f2ff; padding: 10px; border-radius: 10px; margin: 10px 0;",
      div(
        style = "display: flex; align-items: center; margin-left:12px;",
        checkboxGroupInput(
          inputId = "conditionName",  
          label = "conditionName",
          inline = TRUE,
          choices = NULL,
          selected = NULL
        )
      )
    )
  )
}


conditionCheckboxServer <- function(id, updateChoices, sharedValue) {
  moduleServer(id, function(input, output, session) {
    
    # When the choices change (e.g., new df_list), update the input
    observe({
      choices <- updateChoices()
      updateCheckboxGroupInput(session, "conditionName",
                               choices = choices,
                               selected = sharedValue(),
                               inline = TRUE)
    })
    
    # When the user changes this particular input, update sharedValue
    observeEvent(input$conditionName, {
      isolate({
        current_shared <- sharedValue()
        if (!identical(sort(input$conditionName), sort(current_shared))) {
          sharedValue(input$conditionName)
        }
      })
    })
    
    # When sharedValue changes from *other* modules, update this input
    observeEvent(sharedValue(), {
      isolate({
        current_input <- input$conditionName
        if (!identical(sort(current_input), sort(sharedValue()))) {
          updateCheckboxGroupInput(session, "conditionName",
                                   selected = sharedValue())
        }
      })
    })
  })
}
