#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if (interactive()) {
  library(shiny)
  library(dplyr)
  library(magrittr)
  library(shinyAce)
  library(rlang)
  # hard coded for now

  text_df <- readRDS("~/Code/rcode/qcoder/data/qcoder_example_markedup.rds")
  code_df <- readRDS("~/Code/rcode/qcoder/data/example_codes.rds")

  # Define UI for application that draws a histogram
  ui <- fluidPage(

    mainPanel(
        tabsetPanel(
           # Application title
           tabPanel("Add codes to text data",

           # Edit box and document selector
            selectInput('this_doc_id',
                        'Document ID',
                        choices =c(" ", unique(text_df$doc_id)),
                        selected = ' '
                        ),
            actionButton("do_update_document", "Save - useless for now"),

           uiOutput("mydocA"),

          verbatimTextOutput("this_doc"
                                )

       ), # close editor panel
       tabPanel("Codes",
                tableOutput('code_table')

      ), # close codes tab panel
      tabPanel("Add Code",
               actionButton("submit", "Submit")

      ), # close add code panel
      tabPanel("Coded data",
               tableOutput('coded')

      ) # close coded tab panel
    ) # close tab set
  ) # close main panel
)}
  # Define server logic
  server <- function(input, output) {


    doc <- reactive ({
      this_doc <- text_df %>%
        filter(doc_id == as.numeric(input$this_doc_id)) %>%
        select(document_text)
       # Sanitize this
       return(as.character(this_doc[1, "document_text"]))
      })

    output$this_doc <-isolate ({renderText(doc())})

    output$code_table <- renderTable({
      code_df
      })
    output$coded <- renderTable({
      parse_qcodes(text_df)
    })


    output$this_doc_id_r <- reactive ({as.numeric(input$this_doc_id)})
    do_update_document <- reactive({
      text_df["this_doc_id", "document_text"] <- input$edited_doc
      saveRDS(text_df, file = "~/Code/rcode/qcoder/data/qcoder_example.rds")

    })

    output$update_document <- reactive ({do_update_document()})

    output$mydocA <- renderUI({
      aceEditor(
        "edited_doc",
        value = doc(),
        mode = "markdown",
        height = "500"

      )
    })
  }


# Run the application
shinyApp(ui = ui, server = server)

