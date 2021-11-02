#' Shiny app for launching the Hyfe web app based on a participant list.
#'
#' This app was designed to maximize efficiency in high-throughput screening scenarios.
#' To work, before this function is called the working directory must have a file named
#' `key.csv`, with columns `last` (last name), `first` (first name), `study.id`
#' (for the internal ID used to track participants within your study),
#' `consent` (leave blank when file is built --
#' the app will use this field to track which participants have registered consent),
#' `hyfe.id` (the Hyfe uid), `password` (the Hyfe-provided passworded that participants
#' may use to log into the research app, if the study also involves remote phone monitoring),
#' and `url`, which is the url link to the web app unique for every `hyfe.id`.
#'
#' @param demo If TRUE, the function will create `key.csv` with dummy data
#' (only if a file named `key.csv` does not exist in the working directory)
#' in order to explore the app's functionality.
#'
#' @return A shiny app.
#' @export
#'
#' @examples
#' app_screening(demo=TRUE)
app_screening <- function(demo=FALSE){

  if(demo){
    if(!file.exists("key.csv")){
      # Create dummy dataset
      df <- data.frame(last=c("Brew","Kreiger","Castro"),
                       first=c("Joe","Paul","Iulian"),
                       study.id=c("1","4","7"),
                       consent=c("","1",""),
                       hyfe.id=c("123","456","789"),
                       email=c("test+0001@hyfe.ai",
                               "test+0004@hyfe.ai",
                               "test+0007@hyfe.ai"),
                       password=c("123","456","789"),
                       url=c("https://hyfe-sewanee.web.app/?id=sewanee+0001@hyfe.ai",
                             "https://hyfe-sewanee.web.app/?id=sewanee+0004@hyfe.ai",
                             "https://hyfe-sewanee.web.app/?id=sewanee+0007@hyfe.ai"))
      df
      mr <- df
      write.csv(df,file="key.csv",quote=FALSE,row.names=FALSE)
    }else{
      # Load ID file
      mr <- read.csv("key.csv",stringsAsFactors=FALSE) ; head(mr,10)
    }
  }else{
    # Load ID file
    mr <- read.csv("key.csv",stringsAsFactors=FALSE) ; head(mr,10)
  }

  #########################################################
  #########################################################
  # Search ID list

  searchid <- function(study.id=NULL,lastname=NULL,mr){
    mri <- data.frame()
    if(!is.null(study.id)){
      matchi <- grep(tolower(study.id),tolower(mr$study.id))
      if(length(matchi)>0){
        mri <- mr[matchi,]
      }else{
        matchi <- grep(tolower(study.id),tolower(mr$last))
        if(length(matchi)>0){
          mri <- mr[matchi,]
        }
      }
    }
    return(mri)
  }

  #########################################################
  #########################################################

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Hyfe web app launcher",titleWidth=500),
    shinydashboard::dashboardSidebar(disable=TRUE ),
    shinydashboard::dashboardBody(htmltools::tags$head(htmltools::tags$script(
      'Shiny.addCustomMessageHandler("refocus",function(NULL) {
    document.getElementById("text_input1").select();
    });'
    )),
    shinyjs::useShinyjs(),

    shinydashboard::tabBox(title = "",
           id = "id", width = "800px",
           shiny::tabPanel("",
                           shiny::fluidRow(
                             shiny::column(width = 12,
                             shinydashboard::box(
                               status = "info", solidHeader = TRUE,
                               collapsible = FALSE,
                               width = 12,
                               title = "Search by Study ID or Last Name",
                               shiny::passwordInput("text_input1",label=NULL,width="90%")
                             ))),
                    htmltools::br(),
                    shiny::fluidRow(shiny::column(width = 6,shiny::uiOutput("url")),
                                    shiny::column(width=6,shiny::uiOutput("consent"))),
                    htmltools::br(),
                    shiny::fluidRow(
                      shiny::column(width = 12,
                             DT::DTOutput('search')
                      )),
                    htmltools::br(),htmltools::br(),htmltools::br(),
                    shiny::fluidRow(shiny::column(width=12,shiny::checkboxInput("debug","Turn on debugging mode?",value=FALSE)))

           ))))

  #########################################################

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues()
    rv$mraw <- mr
    rv$mr <- mr
    rv$mri <- mr
    rv$update <- 1
    rv$selected <- NULL
    rv$study.id <- NULL
    rv$url <- NULL
    rv$consent <- NULL

    #=======================================

    # Update and format MR dataset
    observe({rv$update
      isolate({
        currlast <- input$text_input1
        currsel <- rv$selected

        mr <- mri <- read.csv("key.csv",stringsAsFactors=FALSE)
        rv$mraw <- mr

        # Remove blank lines
        mri <- mri[mri$study.id!="",] ; nrow(mri)

        rv$mr <- mri
        rv$mri <- mri
        shiny::updateTextInput(session, "study.id",value="")
        if(!is.null(currlast)){
          shiny::updateTextInput(session, "study.id",value=currlast)
        }
        if(!is.null(currsel)){
          rv$selected <- currsel
        }
      })
      #}
    })

    #=======================================
    # URL Launch Button

    output$url <- shiny::renderUI({
      if(!is.null(rv$url)){
        consent <- as.character(rv$consent)
        if(!is.na(consent) & as.character(consent)=="1"){
          tit <- paste0("Launch URL for ",rv$mri$first[rv$selected]," ",rv$mri$last[rv$selected]," (Study ID: ",rv$mri$study.id[rv$selected],")")
          shiny::actionButton("actionbutton1", label = htmltools::h4(tit),width="100%") # Combine text with url variable
        }else{
          tit <- paste0("Selected: ",rv$mri$first[rv$selected]," ",rv$mri$last[rv$selected]," (Study ID: ",rv$mri$study.id[rv$selected],")")
          htmltools::h4(tit)
        }
      }
    })

    shiny::observeEvent(input$actionbutton1,{
      shiny::isolate({
        shiny::updateTextInput(session, "text_input1",value="")
        session$sendCustomMessage(type="refocus",message=list(NULL))
        url <- rv$url
        if(input$debug){url <- paste0(url,"&debug=TRUE")}
        utils::browseURL(url)

      })})


    #=======================================
    # BASE CONSENT

    output$consent <- shiny::renderUI({
      if(!is.null(rv$study.id)){
        consent <- rv$consent
        if(!is.na(consent) & as.character(consent)=="1"){
          htmltools::h4("Base consent is on record")
        }else{
          shiny::actionButton("consent",label=h4("Add Consent"))
        }
      }
    })

    shiny::observeEvent(input$consent,{
      if(!is.null(rv$study.id)){
        rv$consent <- 1
        mri <- rv$mraw
        mri$consent[mri$study.id==rv$study.id] <- "1"
        write.csv(mri,file="key.csv",quote=FALSE,row.names=FALSE,na="")
        rv$update <- rv$update + 1
      }
    })

    #=======================================

    shiny::observe({
      study.id <- lastname <- NULL
      if(input$text_input1!=""){
        study.id <- input$text_input1
      }else{
        rv$mri <- rv$mr
      }
      if(!is.null(study.id)){
        if("study.id" %in% names(rv$mr)){
          mri <- rv$mr
          mri <- searchid(study.id,lastname,mr=mri)
          mri <- mri[!is.na(mri$study.id),]
          rv$mri <- mri
          if(nrow(mri)==1){rv$selected = 1}else{rv$selected = NULL}
        }
      }
      #})
    })


    #=======================================

    output$search = DT::renderDT(rv$mri[,which(names(rv$mri) %in% c("last","first","consent","study.id"))],
                                 selection=list(mode="single",selected=rv$selected),
                                 options = list(pageLength=30,lengthChange = FALSE,dom="t"))

    # Update rv=$selected if a table row is clicked
    shiny::observe({
      input$search_row_last_clicked
      i <- input$search_row_last_clicked
      isolate({
        if(!is.null(i)){
          rv$selected <- i
        }
      })
    })

    shiny::observe({
      if(length(input$study.id)>0 && !is.null(input$study.id) &&  input$study.id==""){
        rv$selected <- NULL
      }
    })

    # Update rv$study.id if rv$selected changes
    shiny::observe({
      rv$selected
      shiny::isolate({
        if(!is.null(rv$selected)){
          rv$study.id <- rv$mri$study.id[rv$selected]
          rv$consent <- rv$mri$consent[rv$selected]
          rv$url <- rv$mri$url[rv$selected]
        }else{
          rv$study.id <- NULL
          rv$url <- NULL
        }
      })
    })

    #=======================================

  }

  #########################################################
  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
#########################################################
# Code from:
# https://stackoverflow.com/questions/38362861/focusing-the-cursor-in-textarea-after-clicking-an-action-button-in-shiny
