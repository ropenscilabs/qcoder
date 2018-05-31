#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This application will read raw data into data frames for use in qcoder.
# The focus is on two things: A folder of text "documents" with documents broadly
# defined and a csv file of codes.

# todo: deal with append/replace

library(shiny)
library(qcoder)

# Define UI for application that creates the data frames for QCoder
if (interactive()) {

  ui <- fluidPage(
    textInput("path_to_code_csv", label = h4("Enter full path to your csv file of tags
                                             including the .csv extension"),
              value = "" ),
    textInput("path_to_code_data_frame", label = h4("Enter full path of where to store
                                                    the code data frame, not including
                                                    the file extension."),
              value = "" ),
    uiOutput("codeButton"),
    verbatimTextOutput("new_code_data"),
    textInput("folder_of_data", label = h4("Enter full path to your folder containing documents"),
              value = "" ),

    textInput("text_data_path", label = h4("Enter full path to the data frame that will store the documents
                                           not including the file extension."),
              value = "" ),
    uiOutput("documentButton"),
    verbatimTextOutput("this_document_data")
  )
}
  server <- function(input, output, session) {

    code_data <- reactive({
      if (is.na(input$path_to_code_csv)  || is.na(input$path_to_code_data_frame)){
        return(FALSE)}
      y <- qcoder::read_code_data(input$path_to_code_csv, input$path_to_code_data_frame)
      y
    })
    new_code_data <- observeEvent(input$create_code_data,( isolate(code_data()) ))
    output$code_data_this <- renderPrint(new_code_data())
    output$codeButton <- renderUI({
     # if (input$text_doc_path == ""){
      #  return()}
      actionButton("create_code_data", "Create code data")
    })

    read_document_data <- reactive({
      if (is.na(input$folder_of_data)  | is.na(input$text_data_path)){
        return()}
      x <-
        qcoder::read_raw_data(input$folder_of_data, input$text_data_path)
     x
    })
  #  document_data <- observeEvent(input$document_data, {
   #                               isolate(read_document_data())})
    output$this_document_data <- renderPrint(
      observeEvent(input$document_data, {
            isolate(read_document_data())
      })
            )
    output$documentButton <- renderUI({
      #if (input$text_doc_path == ""){
      #  return()}
      actionButton("document_data", "Create document data")
    })

}

  # Run the application
  shinyApp(ui = ui, server = server)
