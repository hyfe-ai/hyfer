#' Function for manually finding offsets between a set of labels and Hyfe detections
#'
#' @param alias_id The research alias you want to inspect.
#' @param labels A `dataframe` with analyst labels. Required columns: `test` (numeric ID for each test labeled);
#' `timestamp` (numeric timestamp for the cough-second); and `label` (best label for this cough-second, 1 - 3, using the Hyfe 4-tier system:
#' 1 = a disputable cough, possibly fake; 2 = definitley a cough but barely audible; 3 = a clear authentic cough).
#' @param sounds The `sounds` `dataframe` from a `hyfe` object.
#' @param prediction_threshold Cough prediction score threshold to use for plotting coughs v other noises. Will not change the data.
#'
#' @return Nothing. When this function is run, a Shiny app launches that allows the user to identify the offset. You would
#' take note of that offset then use it to build up a `dataframe` of offsets to pass to `hyfe_performance()`.
#'
#' @export
#'
offset_explorer <- function(alias_id,
                            labels,
                            sounds,
                            prediction_threshold = .7){

  soundi <- sounds
  soundi$prediction <- soundi$prediction_score >= .7
  soundi <- soundi %>% filter(alias == alias_id)

  library(shiny)
  ui <- fluidPage(
    br(),
    fluidRow(column(12,textInput('test_id','Enter test ID',value='1'))),
    br(),
    fluidRow(column(12,sliderInput('gross','Gross adjustments',
                                   min = - 100000,
                                   max = 100000,
                                   value = 0,
                                   step = 1000,
                                   width='100%'))),
    fluidRow(column(12,sliderInput('moderate','Moderate adjustments',
                                   min = - 1000,
                                   max = 1000,
                                   value = 0,
                                   step = 10,
                                   width='100%'))),
    fluidRow(column(12,sliderInput('fine','fine adjustments',
                                   min = - 50,
                                   max = 50,
                                   value = 0,
                                   step = .5,
                                   width='100%'))),
    fluidRow(column(12,plotOutput('gross', height=200))),
    fluidRow(column(12,plotOutput('fine', height=200))),
    br(),
    fluidRow(column(12,textOutput('offset')))
  )
  server <- function(input, output, session) {

    output$gross <- renderPlot({
      labi <- labels[labels$test == input$test_id,]
      toffset <- input$gross + input$moderate + input$fine
      x_zoom <- 100000
      xlims <- c(min(labi$timestamp)-x_zoom, max(labi$timestamp)+x_zoom)
      par(mar=c(4,4,2,.5))
      plot(1,type='n',xlim=xlims,ylim=c(-1,4),axes=FALSE,ann=FALSE)
      axis(1) ; axis(2,at=c(0,1,2,3),las=2) ; title(ylab = 'Label',main=paste0('Alias ',alias_id,' :: Test ',test_id))
      points(x=labi$timestamp, y=labi$label)
      abline(v=soundi$timestamp[which(soundi$prediction==FALSE)] + toffset, col='grey')
      abline(v=soundi$timestamp[which(soundi$prediction==TRUE)] + toffset)
    })

    output$fine <- renderPlot({
      labi <- labels[labels$test == input$test_id,]
      toffset <- input$gross + input$moderate + input$fine
      x_zoom <- 100
      xlims <- c(min(labi$timestamp)-x_zoom, max(labi$timestamp)+x_zoom)
      par(mar=c(4,4,2,.5))
      plot(1,type='n',xlim=xlims,ylim=c(-1,4),axes=FALSE,ann=FALSE)
      axis(1) ; axis(2,at=c(0,1,2,3),las=2) ; title(ylab = 'Label',main=paste0('Alias ',alias_id,' :: Test ',test_id))
      points(x=labi$timestamp, y=labi$label)
      abline(v=soundi$timestamp[which(soundi$prediction==FALSE)] + toffset, col='grey')
      abline(v=soundi$timestamp[which(soundi$prediction==TRUE)] + toffset)
    })

    output$offset <- renderText({
      toffset <- input$gross + input$moderate + input$fine
      paste('Offset for ',alias_id,' during test ',input$test_id,' was ',toffset,' seconds')
    })
  }

  shinyApp(ui, server)
}

# End of function ============================================================
