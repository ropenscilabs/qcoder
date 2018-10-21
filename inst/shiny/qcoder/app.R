#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. Alternitively in the console run qcode().
# This assumes that you have set up a standard QCoder project.
#
if (interactive()) {
  library(qcoder)
  library(shiny)
  library(dplyr)
  library(magrittr)
  library(shinyAce)
  library(rlang)
  library(shinyFiles)
  library(shinythemes)
  library(DT)

  library(shinyjs)
  library(here)
  # hard coded for now
  editor_name <- "aceeditor"

  # Define UI for application
  ui <- fluidPage(
    theme = shinytheme("flatly"),

    mainPanel(
      tags$h2("Qcoder"),
      tags$p("Select your project folder"),
      verbatimTextOutput("project_directory"),
      shinyDirButton('select_project', label="Select Folder", title="Select your project folder",
                     buttonType = "default", class = NULL),
      actionButton("update", "Reload project for data updating",
                           icon = icon("refresh")),
      tags$br(),
      tags$br(),
      # Start tabset
      navlistPanel(
           # Tab title
           tabPanel("Add codes to text data",
          #  conditionalPanel(condition = "input$project_directory == TRUE",
             # Edit box and document selector
             # make sure these are unique

          tabsetPanel(id = "subTabPanel1",
            tabPanel("Edit",
               uiOutput( 'choices' ),
               uiOutput('saveButton'),
               uiOutput('mydocA')),
            tabPanel("Existing file",
               htmlOutput("this_doc" )
               ),
            tabPanel("Unit to Document Links" ,
                     uiOutput('checkbox_save_links'),
                     uiOutput('checkbox_links')
                    )

            ) # close document sub-tabset
       ), # close editor tab panel
       tabPanel("Codes",
                dataTableOutput('code_table')

      ), # close codes tab panel
     # tabPanel("Add Code",
      #         actionButton("submitNewCode", "Submit")

      #), # close add code panel
      tabPanel("Coded data",
               dataTableOutput('coded')

      ), # close coded tab panel
      tabPanel("Units",
               dataTableOutput('units_table')

      ), # close units panel
     tabPanel("Summary",
              dataTableOutput('code_freq')

     ),
     tabPanel("Add data",
             tags$h2("Add new document"),
             shinyFilesButton('file', label="Select File", title="Select your new files from
                            the project folder", multiple= TRUE,
                            buttonType = "default", class = NULL),
             tags$h2("Add new unit"),
             textInput("new_unit",  "Unit name"),
             uiOutput('add_new_unit')

     ) # close add data tab
    ) # close tab set
  ) # close main panel

  )
}


  # Define server logic
  server <- function(input, output, session) {


    # Select the project directory
    user_folder <- c('Select Volume' = Sys.getenv("HOME"))
    if (user_folder != ""){
         shinyDirChoose(input, 'select_project',  roots = user_folder)
    }
    observeEvent(c(input$select_project, input$file, input$update),{
      req(input$select_project)
      if (input$select_project[1] == ""){return()}
      output$project_directory <- renderPrint({parseDirPath(user_folder,
                                                      input$select_project)
                                              })

      if (as.character(input$select_project[1]) == "1" |
          input$select_project[1] == "" ) {return()}
        project_path <<- parseDirPath(user_folder, input$select_project)
        docs_df_path <<- paste0(project_path,
                                "/data_frames/qcoder_documents_",
                                basename(project_path), ".rds")
        codes_df_path <<- paste0(project_path,
                                 "/data_frames/qcoder_codes_",
                                 basename(project_path), ".rds")
        units_df_path <<- paste0(project_path,
                                 "/data_frames/qcoder_units_",
                                 basename(project_path), ".rds")
        units_docs_path <<- paste0(project_path,
                                   "/data_frames/qcoder_unit_document_map_",
                                   basename(project_path), ".rds")


      my_choices <- reactive({
        req(input$select_project)
        if (input$select_project[1] == ""){return()}
        text_df <- readRDS(file = docs_df_path)
        options <- text_df["doc_path"]
        options <- c(" ", options)
        options
      })

      output$choices <- renderUI({
          req(input$select_project)
          if (input$select_project[1] == ""){return()}

          if (docs_df_path == "") {return()}
            selectInput('this_doc_path', 'Document', my_choices())
         })

    output$saveButton <- renderUI({
      actionButton("submit", "Save changes")
    })

    # Functions related to rendering an individual text document in an editor and
    # verbatim
    # Consider making a backup each time you load this.
    doc <- reactive ({
      if (is.null(input$this_doc_path)) {return()}
      if (docs_df_path == "") {return()}
      # move to utils
      text_df <- readRDS(docs_df_path)
      if (length(text_df) == 0){return()}

      this_doc <- text_df %>%
        filter(doc_path == as.character(input$this_doc_path)) %>%
        select(document_text)
       # Sanitize this
       return(as.character(this_doc[1, "document_text"]))
      })

    comps <- list()
    if (codes_df_path == "" | is.null(codes_df_path)) {return()}
    code_df <- readRDS(codes_df_path)
    comps[["codes"]] <- code_df["code"]
    comps[["tags"]] <- c("QCODE",  "{#")

    codes <- reactive({
      if (codes_df_path == "") {return()}
      code_df <- readRDS(codes_df_path)
      return(code_df["code"])

    })

      # Create the text editor
       output$mydocA <- renderUI({list(useShinyjs(),
         selectInput(inputId = "select_codes", label = "Select Codes to Add",
                     choices = codes(), selected=codes()[1], multiple = TRUE),
         actionButton("replace", "Add selected code"),

           aceEditor(
             editor_name,
             value = doc(),
             mode = "markdown",
             height = "500",
             wordWrap = TRUE,
             autoComplete = "live",
             autoCompleters = "static",
             selectionId = "selected",
             cursorId = "cursorpos",
             autoCompleteList = comps

           )
         )
       })

      output$this_doc <-{renderText(qcoder::txt2html(doc()))}

      # Get the code data for display
      output$code_table <- DT::renderDataTable({
          if (codes_df_path == "") {return()}
          code_df <- readRDS(codes_df_path)
          DT::datatable(code_df,options = list(paging = FALSE))
        })

      # Get the units data for display
      p("Units are units of analysis which might be individuals, organizations,
        events, locations or any other entity relevant to the project.")
      output$units_table <- DT::renderDataTable({
        if (units_df_path == "") {return()}
        units_df <- readRDS(units_df_path)
        DT::datatable(units_df,options = list(paging = FALSE))
      })

        # Get the parsed values with codes.
        output$coded <- DT::renderDataTable({
          if (docs_df_path == "" | codes_df_path == "" ) {return()}
          text_df <- readRDS(docs_df_path)
          code_df <- readRDS(codes_df_path)
          parsed <- qcoder::parse_qcodes(text_df, save_path = codes_df_path, code_data_frame = code_df)

          DT::datatable(parsed,options = list(paging = FALSE))
        })

      output$code_freq <- DT::renderDataTable({
          if (docs_df_path == "" | codes_df_path == "" ) {return()}
          text_df <- readRDS(docs_df_path)
          code_df <- readRDS(codes_df_path)
          parsed <- qcoder::parse_qcodes(text_df)
          parsed %>% dplyr::group_by(as.factor(qcode)) %>%
              dplyr::summarise(n = n()) %>%
              rename('code'='as.factor(qcode)') %>%
              DT::datatable(options = list(paging = FALSE))
        })
    }) #close observer


    # Functions related to updating the text.
    new_text <- reactive({
      input$aceeditor
    })

    update_editor <- observeEvent(input$replace, {

      text_old <- new_text()
      codes <- input$select_codes
      selected <- input$selected
      if (length(selected) == 0) {return(message("No text selected"))}

      updated_selection <- qcoder:::add_codes_to_selection(selection = selected, codes = codes)

      updated_text <- qcoder:::replace_selection(text_old, selected, updated_selection)

      updateAceEditor(session=session, editorId=editor_name, value=updated_text)
      # put js code to move cursor here
      jump_to <- input$cursorpos
      # print(jump_to)
      row_num <- jump_to$row + 1
      col_num <- jump_to$column + (nchar(updated_selection) - nchar(selected))

      # print(jump_to)

      js_statement <- paste0("editor__",editor_name,".focus(); editor__",editor_name,".gotoLine(", row_num, ",", col_num, ");")
      # print(js_statement)

      shinyjs::runjs(js_statement)
    })


    update_document <-observeEvent(input$submit,
           {
             qcoder::do_update_document(new_text(), docs_df_path = docs_df_path,
                                        this_doc_path = input$this_doc_path)
             codes <- get_codes(new_text())
             if (length(codes) > 0){
               x <- readRDS(codes_df_path)
               qcoder::add_discovered_code(codes, x, codes_df_path)
             }
           }
    )

    # Adding a new document
    observeEvent(c(input$select_project,input$update),{
       doc_folder <- c(paste0(input$select_project, "/documents"))
       shinyFileChoose(input, 'file', roots = c("documents" = doc_folder))
    })

    observeEvent(input$file, {
      doc_folder <- c(paste0(project_path, "/documents/"))
      files <- parseFilePaths(doc_folder, input$file)
      qcoder::add_new_documents(files, doc_folder, docs_df_path)
    })

    # Set up for associating units and documents
    observeEvent(c(input$select_project,input$update), {
      output$checkbox_links <- renderUI({
        units_df <- as.data.frame(readRDS(units_df_path))
        units_docs_df <- as.data.frame(readRDS(units_docs_path))

        checknames <- units_df$name
        checkvalues <- units_df$unit_id
        this_selected_df <- units_docs_df %>% filter(doc_path == input$this_doc_path)
        this_selected <- as.character(this_selected_df$unit_id)

        checkboxGroupInput(inputId =  "unit_doc_links", label = "Units connected to this document:",
                                 choiceNames = checknames,
                                 choiceValues = checkvalues,
                                 selected = this_selected
        )
      })

      output$checkbox_save_links <- renderUI({
        actionButton("save_links", "Save links")
      })
    })

    observeEvent(input$save_links, {
      checks <- input$unit_doc_links
      qcoder::update_links(checked = checks, docs_df_path = docs_df_path,
                           this_doc_path = input$this_doc_path,
                           units_docs_path = units_docs_path)
    })

    output$add_new_unit <- renderUI({
      actionButton("add_new_unit", "Add unit")
    })

    observeEvent(   input$add_new_unit, {
      units_df <- readRDS(units_df_path)
      qcoder::add_unit(units_df, input$new_unit, units_df_path)
    })
  } # close server

# Run the application
shinyApp(ui = ui, server = server)

