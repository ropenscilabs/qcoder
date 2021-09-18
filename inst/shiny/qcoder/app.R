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

      tags$br(),
      tags$br(),
      # Start tabset
      navlistPanel(
      # Nav list panel id
      id = "navlist",
           # Tab title
           tabPanel("Project",
                    tags$p("Select your project folder"),
                    verbatimTextOutput("project_directory"),
                    shinyDirButton('select_project', label="Select Folder",
                                   title="Select your project folder",
                                   buttonType = "default", class = NULL),
                    tags$br(),
                    tags$br(),
                    actionButton("update", "Reload project for data updating",
                                 icon = icon("refresh"))

                    ), # Close tab panel
           # Tab title
           tabPanel("Add codes to text data",
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
                    ),
            tabPanel("Document Table",
                     tags$p("The first 250 characters of the documents are
                            shown."),
                     dataTableOutput('docs_table')
                     )
            ) # close document sub-tabset
       ), # close editor tab panel
       tabPanel("Codes",
                tags$p("You must open the application in browser to download data."),
                dataTableOutput('code_table')
      ), # close codes tab panel
      tabPanel("Coded data",
               dataTableOutput('coded')
      ), # close coded tab panel
      tabPanel("Units",
               dataTableOutput('units_table')

      ), # close units panel
     tabPanel("Summary",
              textOutput('units_summary'),
              textOutput('docs_summary'),
              textOutput('codes_summary'),
              tags$h5('Codes frequency'),
              dataTableOutput('code_freq')
     ),
     tabPanel("Export project",
              actionButton("zipfile", label = "Zip Project",
                           buttonType = "default, class = NULL"),
              tags$p("This exports the entire project so it can be qcoded
                     in another location."),
              tags$p("Zip is located in the same folder as the project.")
     ),
     tabPanel("Add data",

             tags$h2("Add new document"),
             shinyFilesButton('file', label="Select File",
                            title="Select your new files from
                            the project folder", multiple= TRUE,
                            buttonType = "default", class = NULL),

             tags$h2("Add new unit"),
             textInput("new_unit",  "Unit name"),
             uiOutput('add_new_unit'),
             tags$h2("Add new code"),
             textInput("new_code", "Code"),
             textInput("new_code_desc", "Description"),
             uiOutput("add_new_code")


     ) # close add data tab
    ) # close tab set
  ) # close main panel

  )
}


  # Define server logic
  server <- function(input, output, session) {
    #Project selected Conditionals
    #Show only if the project is selected
    #If no project selected, hide tabs
    conditionalPanel(
      condition = "is.null(input$select_project)",
      hideTab("navlist", "Add codes to text data"),
      hideTab("navlist", "Codes"),
      hideTab("navlist", "Coded data"),
      hideTab("navlist", "Units"),
      hideTab("navlist", "Summary"),
      hideTab("navlist", "Add data"),
      hideTab("navlist", "Export files")
    )
    # Select the project directory if not using working directory.
    if (!exists("user_folder")){
      if (Sys.getenv("HOME") != "") {
          # Linux/Mac
          user_folder <- c('Select Volume' = Sys.getenv("HOME"))
      } else
      if (Sys.getenv("R_USER") != "") {
        # Used for Windows
        user_folder <- c('Select Volume' = Sys.getenv("R_USER"))
      } else {
        # Fallback, which should never happen.
        user_folder <- getwd()
      }
    }

    if (user_folder != "" ){
         shinyDirChoose(input, 'select_project', roots = user_folder
                          )
    }

    observeEvent(c(input$select_project, input$file, input$update),{
      req(input$select_project)
      if (input$select_project[1] == "" ){
             return()
      }
      output$project_directory <- renderPrint({parseDirPath(user_folder,
                                                      input$select_project)
                                              })
      if (as.character(input$select_project[1]) == "1" |
          input$select_project[1] == "" ) {
               return()
           }
        project_path <<- parseDirPath(user_folder, input$select_project)

        if (length(project_path) == 0 ){
            return()
          }
        if (project_path == "" ){
            return()
          }

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

      project.status <- reactiveValues(saved=TRUE)

      #Show tabs once project is selected
      conditionalPanel(
        condition = "!is.null(input$select_project)",
        #print(project_path == logical(0)),
        showTab("navlist", "Add codes to text data"),
        showTab("navlist", "Codes"),
        showTab("navlist", "Coded data"),
        showTab("navlist", "Units"),
        showTab("navlist", "Summary"),
        showTab("navlist", "Add data"),
        showTab("navlist", "Export files")
      )

      my_choices <- reactive({
        req(input$select_project)
        if (input$select_project[1] == ""){return()}
        text_df <- readRDS(file = docs_df_path)
        options <- text_df["doc_path"]
        options <- c(" ", options)
        options
      })

      output$choices <- renderUI({
          qcoder::validate_project(project_path)
          req(input$select_project)
          if (input$select_project[1] == ""){return()}
          qcoder::validate_project_files(project_path)
          if (docs_df_path == "") {return()}
            selectizeInput('this_doc_path', 'Document', my_choices())
         })

      output$saveButton <- renderUI({
          if (project.status$saved) {
              saving.alert <- "check-circle"
          } else {
              saving.alert <- "exclamation-triangle"
          }
          actionButton("submit", "Save changes",icon= icon(saving.alert))
      })

      observeEvent(input$submit,{
          project.status$saved=TRUE
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
    qcoder::validate_project(project_path)
    qcoder::validate_project_files(project_path)

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
                     choices = codes(), selected = codes()[1], multiple = TRUE),
         actionButton("replace", "Add selected code"),

           aceEditor(
             editor_name,
             outputId = "ace",
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

      observeEvent(input$replace,{
          project.status$saved=FALSE
          })

      output$this_doc <-{renderText(qcoder::txt2html(doc()))}

      output$docs_table <- DT::renderDataTable({
        if (docs_df_path == "") {return()}
        docs_df <- readRDS(docs_df_path)
        docs_df$document_text <- substr(docs_df$document_text,1,250)
        DT::datatable(docs_df,options = list(paging = FALSE, dom = "Bfrtip",
                                              buttons = list(list(extend='copy'),
                                                             list(extend='csv', filename = "QCoder_Units"),
                                                             list(extend='excel', filename = "QCoder_Units"),
                                                             list(extend='pdf', filename = "QCoder_Units"),
                                                             list(extend="print"))))

      })

      # Get the code data for display
      output$code_table <- DT::renderDataTable(server = FALSE, {
          if (codes_df_path == "") {return()}
          code_df <- readRDS(codes_df_path)
          DT::datatable(code_df,
                        extensions = 'Buttons',
                        options = list(paging = TRUE, dom = 'Bfrtip',
                                  buttons = list(list(extend='copy'),
                                                 list(extend='csv', filename = "QCoder_Codes"),
                                                 list(extend='excel', filename = "QCoder_Codes"),
                                                 list(extend='pdf', filename = "QCoder_Codes"),
                                                 list(extend="print"))))
        })

      # Get the units data for display
      p("Units are units of analysis which might be individuals, organizations,
        events, locations or any other entity relevant to the project.")
      output$units_table <- DT::renderDataTable({
        if (units_df_path == "") {return()}
        units_df <- readRDS(units_df_path)
        DT::datatable(units_df,options = list(paging = FALSE, dom = "Bfrtip",
                                              buttons = list(list(extend='copy'),
                                                             list(extend='csv', filename = "QCoder_Units"),
                                                             list(extend='excel', filename = "QCoder_Units"),
                                                             list(extend='pdf', filename = "QCoder_Units"),
                                                             list(extend="print"))))

      })

        # Get the parsed values with codes.
        output$coded <- DT::renderDataTable(server = FALSE, {
          if (docs_df_path == "" | codes_df_path == "" ) {return()}
          text_df <- readRDS(docs_df_path)
          code_df <- readRDS(codes_df_path)
          parsed <- qcoder::parse_qcodes(text_df, save_path = codes_df_path, code_data_frame = code_df)

          DT::datatable(parsed,options = list(paging = TRUE, dom = "Bfrtip",
                                              buttons = list(list(extend='copy'),
                                                             list(extend='csv', filename = "QCoder_CD"),
                                                             list(extend='excel', filename = "QCoder_CD"),
                                                             list(extend='pdf', filename = "QCoder_CD"),
                                                             list(extend="print"))))

        })

      output$code_freq <- DT::renderDataTable({
          if (docs_df_path == "" | codes_df_path == "" ) {return()}
          text_df <- readRDS(docs_df_path)
          code_df <- readRDS(codes_df_path)
          parsed <- qcoder::parse_qcodes(text_df)
          if (!is.null(parsed$qcode)){
               code_freq <-parsed %>% dplyr::group_by(as.factor(qcode)) %>%
                 dplyr::summarise(n = n()) %>%
                 rename('code'='as.factor(qcode)')
             DT::datatable(code_freq,options = list(paging = TRUE, dom = "Bfrtip",
                                        buttons = list(list(extend='copy'),
                                        list(extend='csv', filename = "QCoder_Summary"),
                                        list(extend='excel', filename = "QCoder_Summary"),
                                        list(extend='pdf', filename = "QCoder_Summary"),
                                        list(extend="print"))))

          }
        })

      output$units_summary <- renderText({
        units_df <- readRDS(units_df_path)
        paste("Number of units: ", nrow(units_df))
      })

      output$docs_summary <- renderText({
        text_df <- readRDS(docs_df_path)
        paste("Number of documents: ", nrow(text_df))
      })

      output$codes_summary <- renderText({
        code_df <- readRDS(codes_df_path)
        paste("Number of codes: ", nrow(code_df))
      })

    }) #close observer


    # Functions related to updating the text.
    new_text <- reactive({
      input$ace
    })

    update_editor <- observeEvent(input$replace, {
      validate(need(input$select_codes, "Codes must be selected"),
               need(input$ace_selected, "Did you select some text?"))

      text_old <- new_text()
      codes <- input$select_codes
      selected <- input$ace_selected

      if (length(selected) == 0) {return(message("No text selected"))}

      updated_selection <- qcoder:::add_codes_to_selection(selection = selected, codes = codes)
      updated_text <- qcoder:::replace_selection(text_old, selected, updated_selection)

      updateAceEditor(session=session, "ace", value = updated_text)
      # put js code to move cursor here
      jump_to <- input$cursorpos
      # print(jump_to)
      row_num <- jump_to$row + 1
      col_num <- jump_to$column + (nchar(updated_selection) - nchar(selected))

      js_statement <- paste0("editor__","ace",".focus(); editor__","ace",".gotoLine(", row_num, ",", col_num, ");")

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
       if (!exists("project_path")){return()}
       doc_folder <- c(paste0(project_path, "/documents"))
       shinyFileChoose(input, 'file', roots = c("documents" = doc_folder))
    })

    observeEvent(input$file, {
      if (!exists("project_path")){return()}
      req(!is.integer(input$file))
      doc_folder <- c(paste0(project_path, "/documents/"))
      # Based on shinyFiles::parseFilePaths()
      files <- sapply(input$file$files, function(x) paste0(unlist(x), collapse = "/"))
      #remove leading separator if present
      files <- ifelse(substr(files, 1, 1) == "/", sub("^.", "", files), files)

      qcoder::add_new_documents(files, docs_df_path, doc_folder)
      showNotification(paste("Document added"), duration = 0)
    })

    # Set up for associating units and documents
    observeEvent(c(input$select_project,input$update), {
      output$checkbox_links <- renderUI({
        units_df <- as.data.frame(readRDS(units_df_path))
        units_docs_df <- as.data.frame(readRDS(units_docs_path))

        checknames <- units_df$name
        checkvalues <- units_df$unit_id
        if (nrow(units_docs_df) > 0) {
            this_selected_df <- units_docs_df %>%
                filter(doc_path == input$this_doc_path)
                this_selected <- as.character(this_selected_df$unit_id)
                checkboxGroupInput(inputId =  "unit_doc_links",
                                 label = "Units connected to this document:",
                                 choiceNames = checknames,
                                 choiceValues = checkvalues,
                                 selected = this_selected
                                 )
        }
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
      showNotification(paste("New unit added"), duration = 0)
      DT::dataTableOutput('units_table')
    })
    output$add_new_code <- renderUI({
      actionButton("add_new_code", "Add code")
    })
    observeEvent(   input$add_new_code, {
      codes_df <- readRDS(codes_df_path)
      qcoder::add_code(codes_df, input$new_code, input$new_code_desc,
                       codes_df_path)
      showNotification(paste("New code added"), duration = 0)
      DT::dataTableOutput('code_table')
    })
    observeEvent(input$zipfile, {
      if (!exists("project_path")){return()}
        zip::zipr(zipfile = paste0(dirname(project_path), "/",
                                   "QCoderProject-", basename(project_path), "-",
                                   Sys.Date(),".zip"), files = project_path,
                  recurse = TRUE)
         message(paste0("Zip file created: ", paste0(dirname(project_path), "/",
                                                "QCoderProject-",
                                                basename(project_path), "-",
                                                Sys.Date(),".zip")))
    })
  } # close server

# Run the application
shinyApp(ui = ui, server = server)

