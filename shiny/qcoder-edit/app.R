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
  text_df <- readRDS("~/Code/rcode/qcoder/data/qcoder_example.rds")
  code_df <- readRDS("~/Code/rcode/qcoder/data/example_codes.rds")
  #example

  #fields <- c("doc_id", "doc_text")

  # Define UI for application that draws a histogram
  ui <- fluidPage(

    mainPanel(
        tabsetPanel(
       # Application title
           tabPanel("Adding codes to text data",

           # Edit box and document selector

           ui <- fluidPage(

            selectInput('this_doc_id', 'Document ID', choices =c(" ", unique(text_df$doc_id))),

            textAreaInput("document", "Document",
                           'this_doc',
                           width = "500px",
                           height= "800px"
                           ),
             verbatimTextOutput('this_doc'
                                ),
             actionButton("submit", "Submit")


         )),
       tabPanel("Codes",
                tableOutput('code_table')

      ),
      tabPanel("Add Code",

               actionButton("submit", "Submit"))

    )
)
  )
}
  # Define server logic
  server <- function(input, output) {

    output$this_doc <- reactive ({
      this_doc<- text_df %>% filter(doc_id =='this_doc_id') %>% select("document_text")})

    output$code_table <- renderTable({
      code_df
      })


  }


# Run the application
shinyApp(ui = ui, server = server)

