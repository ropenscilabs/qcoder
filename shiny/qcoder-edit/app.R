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
  # hard coded for now

  text_df <- readRDS("~/Code/rcode/qcoder/data/qcoder_example.rds")
  code_df <- readRDS("~/Code/rcode/qcoder/data/example_codes.rds")




  # Define UI for application that draws a histogram
  ui <- fluidPage(

    mainPanel(
        tabsetPanel(
           # Application title
           tabPanel("Add codes to text data",

           # Edit box and document selector

          # ui <- fluidPage(

            selectInput('this_doc_id',
                        'Document ID',
                        choices =c(" ", unique(text_df$doc_id)),
                        selected = ' '
                        ),
          #actionButton("update", "Load text"),
            #fix the bug in shineAce
            aceEditor(
              "edited_doc",
              value = text_df[1, "document_text"],
              #value = text_df["this_doc_id_r", "document_text"],
              mode = "markdown"

              ),

            verbatimTextOutput("this_doc"
                                )

       ),
       tabPanel("Codes",
                tableOutput('code_table')

      ), # close codes tab panel
      tabPanel("Add Code",
               actionButton("submit", "Submit")
               )
      ) # close add code panel
    ) # close tab set
  ) # close main panel
}
  # Define server logic
  server <- function(input, output) {


    doc <- reactive ({
      this_doc <- text_df %>%
        filter(doc_id == as.numeric(input$this_doc_id)) %>%
        select(document_text)

       return(as.character(this_doc[1, "document_text"]))
      })

    output$this_doc <-isolate ({renderText(doc())})

    output$code_table <- renderTable({
      code_df
      })

    output$this_doc_id_r <- reactive ({as.numeric(input$this_doc_id)})
  }


# Run the application
shinyApp(ui = ui, server = server)

