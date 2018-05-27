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

  text_path <- "~/Code/rcode/qcoder/data/qcoder_example_markedup.rds"
  text_df <- readRDS(text_path)
  code_path <- "~/Code/rcode/qcoder/data/example_codes.rds"
  code_df <- readRDS(code_path)
  #make sure that integrity of IDs is ensured or throw an error
 # ids <- unique(text_df$doc_id)
#  names(ids) <- as.character(text_df$doc_path)
 # choices <- ids
  # Define UI for application that draws a histogram
  ui <- fluidPage(

    mainPanel(
        tabsetPanel(
           # Application title
           tabPanel("Add codes to text data",

           # Edit box and document selector
           #make sure these are unique
            selectInput('this_doc_path',
                        'Document',
                        choices =c(" ", unique(text_df$doc_path)),
                        selected = ' '
                        ),
            actionButton("submit", "Save - useless for now"),

           uiOutput("mydocA"),

          verbatimTextOutput("this_doc" )

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
  )
  }
  # Define server logic
  server <- function(input, output, session) {

    doc <- reactive ({
      this_doc <- text_df %>%
        filter(doc_path == as.character(input$this_doc_path)) %>%
        select(document_text)
       # Sanitize this
       return(as.character(this_doc[1, "document_text"]))
      })

    output$this_doc <-isolate ({renderText(doc())})

    output$code_table <- renderTable({
        code_df
      })
    output$coded <- renderTable({
      qcoder::parse_qcodes(text_df)
    })


    output$this_doc_path_r <- reactive ({input$this_doc_path})

    new_text <- reactive({
      input$edited_doc
    })

    do_update_document <- function(updated){
      row_num <- which(text_df[,"doc_path"] == input$this_doc_path)
      text_df[row_num, 2] <- updated
      # make sure this save happens
      saveRDS(text_df, file = text_path)
      invisible(TRUE)
    }

    update_document <-observeEvent(input$submit,
          {
             do_update_document(new_text())
          }
    )

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

