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
  library(shinyFiles)
  library(shinythemes)
  # hard coded for now

  # Define UI for application
  ui <- fluidPage(
    theme = shinytheme("flatly"),

    mainPanel(
      tags$h2("Qcoder"),
      tags$p("Select your project folder"),
      verbatimTextOutput("project_directory"),
      shinyDirButton('select_project', label="Select Folder", title="Select your project folder",
                    buttonType = "default", class = NULL),
      tags$br(),
      tags$br(),
      # Start tabset
      navlistPanel(
           # Tab title
           tabPanel("Add codes to text data",
          #  conditionalPanel(condition = "input$project_directory == TRUE",
             # Edit box and document selector
             # make sure these are unique
               uiOutput( 'choices' ),
               uiOutput('saveButton'),
               uiOutput('mydocA'),
               verbatimTextOutput("this_doc" )
           # )
       ), # close editor tab panel
       tabPanel("Codes",
                tableOutput('code_table')

      ), # close codes tab panel
     # tabPanel("Add Code",
      #         actionButton("submitNewCode", "Submit")

      #), # close add code panel
      tabPanel("Coded data",
               tableOutput('coded')

      ), # close coded tab panel
      tabPanel("Units",
               tableOutput('units_table')

      ), # close units panel
     tabPanel("Summary",
              verbatimTextOutput('code_freq')

     )
    ) # close tab set
  ) # close main panel

  )
}



  # Define server logic
  server <- function(input, output, session) {


    # Select the project directory
    user_folder <- c('Select Folder' = Sys.getenv("HOME"))
    shinyDirChoose(input, 'select_project',  roots = user_folder)



    observeEvent(input$select_project,{
      output$project_directory <- renderPrint({parseDirPath(user_folder, input$select_project)})
      project_path <- parseDirPath(user_folder, input$select_project)
      docs_df_path <- paste0(project_path,  "/data_frames/qcoder_documents_", basename(project_path), ".rds")
      codes_df_path <- paste0(project_path,  "/data_frames/qcoder_codes_", basename(project_path), ".rds")
      units_df_path <- paste0(project_path,  "/data_frames/qcoder_units_", basename(project_path), ".rds")

      my_choices <- reactive({
        text_df <- readRDS(file = docs_df_path)
        options <- text_df["doc_path"]
        options <- c(" ", options)
        options
      })

      output$choices <- renderUI({
            selectInput('this_doc_path', 'Document', my_choices())
         })


    output$saveButton <- renderUI({
      actionButton("submit", "Save changes")
    })

    # Functions related to rendering an individual text document in an editor and
    # verbatim
    # Consider making a backup each time you load this.
    doc <- reactive ({

      if (length(input$this_doc_path) != 1) {return()}
      # move to utils
      text_df <- readRDS(docs_df_path)
      this_doc <- text_df %>%
        filter(doc_path == as.character(input$this_doc_path)) %>%
        select(document_text)
       # Sanitize this
       return(as.character(this_doc[1, "document_text"]))
      })

      # Create the text editor
      output$mydocA <- renderUI({
        #if (length(input$project_directory) == 0 ) {return()}
        aceEditor(
          "edited_doc",
          value = doc(),
          mode = "markdown",
          height = "500",
          wordWrap = TRUE,
          autoCompleteList = list(qc = c("(QCODE)", "(/QCODE)", "{#}"))

        )
      })

      output$this_doc <-{renderText(doc())}

      # Get the code data for display
      output$code_table <- renderTable({
          if (codes_df_path == "") {return()}
          code_df <- readRDS(codes_df_path)
          code_df
        })

      # Get the units data for display
      output$units_table <- renderTable({
        if (units_df_path == "") {return()}
        units_df <- readRDS(units_df_path)
        units_df
      })

        # Get the parsed values with codes.
        output$coded <- renderTable({
          if (docs_df_path == "" ) {return()}
          text_df <- readRDS(docs_df_path)
          code_df <- readRDS(codes_df_path)
          parsed <- qcoder::parse_qcodes(text_df, save_path = codes_df_path, code_data_frame = code_df)

          parsed
        })

        output$code_freq <- renderPrint({
          text_df <- readRDS(docs_df_path)
          code_df <- readRDS(codes_df_path)
          parsed <- qcoder::parse_qcodes(text_df, save_path = codes_df_path, code_data_frame = code_df)
          parsed %>% dplyr::group_by(as.factor(qcode)) %>% dplyr::summarise(n = n()) %>% knitr::kable()
        })
    }) #close observer

    # Functions related to updating the text.
    new_text <- reactive({
      input$edited_doc
    })

    update_document <-observeEvent(input$submit,
           {
             project_path <- parseDirPath(user_folder, input$select_project)
             docs_df_path <- paste0(project_path,  "/data_frames/qcoder_documents_",
                                    basename(project_path), ".rds")
             qcoder::do_update_document(new_text(), docs_df_path = docs_df_path,
                                        this_doc_path = input$this_doc_path)
           }
    )
  }

# Run the application
shinyApp(ui = ui, server = server)

