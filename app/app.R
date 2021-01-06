# Load libraries, assuming these have been installed
# First, gslea

library(gslea) # includes dependency: datatable, but nothing else
library(shiny)

# Pasted from previous tabbed setup
# Important features of 

ui <- fluidPage(
#  tags$div(class = "h2",
#           tags$strong("GSLEA - Gulf of St. Lawrence Ecosystem Approach")
#  ),
  tabsetPanel(
      id="tabs",
      tabPanel(title = "Variable selection & download", 
      sidebarPanel(
          # uiOutput to contain names:
          uiOutput("input.vartypes"), #,
          # actionButton("lookup""Lookup variable descriptions for this type"),
          hr(),
          #!# Action: if including shinyjs, hide these if insufficient input
          helpText("Additional criteria:"), 
          uiOutput("input.EAR"), 
          uiOutput("input.year"),
          uiOutput("input.selected"),
          # DOWNLOAD
          downloadButton("downloadFilteredData", label = "Download selected data")
      ),
      mainPanel(
          helpText("After submitting variable type, select which variables you would like to output here."),
          textOutput("selected.vars"),
          tableOutput("var.descriptions")
      )
  ),
  tabPanel(title = "Time-series of variables", 
      h2("Visualize how your variables of interest vary over select years"),
      helpText("Note: only variables selcted in `Variable selection & download` are available here."),
      sidebarPanel(
          h3("Variables selected in previous panel"),
          #!# Action: if including shinyjs, hide these if there is insufficient input
          uiOutput("plot.vars"), # choose up to 5 variables from the previous
          uiOutput("plot.EAR"), # select up to 5 EARs
          helpText("Filter to selected years:"), # Smoothing options
          uiOutput("plot.years"), # which years to plot
          # For now, ignore graphical parameters and only include smoothing
          checkboxInput("plot.smoothing", label="Plot with smoothing", value=F),
          actionButton("plot.plot", label="Create plot")
      ),
      mainPanel(
        plotOutput("multivar.comp.plot")
      )
    ), 
    tabPanel(title = "Cross-correlation", 
      h2("Calculate cross-correlation between selected variables (with lags)"),
      helpText("Note: only variables selcted in `Variable selection & download` are available here."),
      sidebarPanel(
          # h3("Variables selected in previous panel"),
          #!# Action: if including shinyjs, hide these if there is insufficient input
          h4("Select & filter independent variable data"),
          uiOutput("corr.varx"), # choose the ind/x variable for cross-correlation
          uiOutput("corr.EARx"), # select x EAR
          h4("Select & filter dependent variable data"),
          uiOutput("corr.vary"), # choose the ind/y variable for cross-correlation
          uiOutput("corr.EARy"), # select y EAR
          h4("Select years for plotting"), 
          helpText("The largest continuous block of time in these years will be chosen. If you select years where data are not available, they are pairwise deleted before correlation is run. Only contiguous points (in time) are used because of the assumptions of lagged correlation analysis."),
          uiOutput("corr.years"), # which years to consider
          # For now, ignore graphical parameters and only include smoothing
          helpText("You can optionally difference the variables to make them stationary, and therefore correlate how the values of each of the variables change from year to year as opposed to their absolute values."),
          checkboxInput("corr.diff", label="Difference variables?", value=F)
      ),
      mainPanel(
          plotOutput("cross.corr.plot")
      )
    )
  ) 
)

server <- function(input, output, session){
    # Reactive inputs:
    datasetInput <- reactive({
        vars.f(variable.type=input$vartype.checkbox)
    })

    uniqueTypes <<- unique(gslea::variable.description$type)
    uniqueVars <<- unique(gslea::variable.description$label)
    uniqueYears <<- unique(gslea::EA.data$year)
    uniqueEARs <<- unique(gslea::EA.data$EAR)
    #!# Tab 1 input: Variable selection
    output$input.vartypes <- renderUI({
        selectInput(inputId = "vartype.checkbox", selectize = T, label = "Variable type(s) to include:", choices = c("all", uniqueTypes), selected = NULL)
    })
    output$input.EAR <- renderUI({
        selectInput(inputId = "EAR.select", label = "Filter to these EAR(s):", choices = uniqueEARs, multiple = T, selected = NULL)
    })
    output$input.year <- renderUI({
        # selectInput(inputId = "year.select", label = "Filter to these year(s):", choices = uniqueYears, multiple = T, selected = NULL)
          sliderInput(inputId = "year.select",
                  label = "Which years should be included?",
                  min = min(uniqueYears),
                  max = max(uniqueYears),
                  value = c(min(uniqueYears), max(uniqueYears)),
                  step = 1,
                  sep = "",
                  round = 0)
    })
    output$input.selected <- renderUI({
        selectInput(inputId = "var.select", 
                    label = "Which variables of the selected type would you like to download? (see descriptions to the right for more information)", 
                    choices = unique(datasetInput()$variable), 
                    multiple = T, 
                    selected = NULL)
    })
    #!# Tab 1 output: Variable descriptions
    output$var.descriptions <- renderTable(datasetInput())
    #!# Tab 1 actions: 
    # Change outputDF depending on which variables are selected
    observeEvent(input$var.select, {
      if(!is.null(input$var.select)){
          outputDF <<- reactive({
              gslea::EA.query.f(variables = input$var.select, years = seq(input$year.select[1], input$year.select[2]), EARs = as.numeric(input$EAR.select))
          })
      } else {
          outputDF <<- reactive({NULL})
      }
    })
    # Download selected data    
    output$downloadFilteredData <- downloadHandler(
    filename = function() {
      paste("gslea_data_", input$vartype.checkbox, "_", gsub("-", "_", gsub(" ", "_", Sys.time())), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(outputDF(), file, row.names = FALSE)
    }
  )
  #!# Tab 2: Plotting inputs
  # Select vars for plotting
  output$plot.vars <- renderUI({
      selectInput(inputId = "plot.vars.sel", 
                  label = "Select up to 5 variables to visualize (each variable = one row in the plot). Only the first 5 will be plotted.",
                  choices = unique(input$var.select),
                  multiple = T) 
  })
  # Updater to ensure that only five vars are selected
###  observeEvent(input$plot.vars.sel, {
###      if(length(input$plot.vars.sel) > 5){
###          # Select most recent 5
###          updateSelectInput(session, inputId = input$plot.vars.sel, selected = c(input$plot.vars.sel[c((length(input$plot.vars.sel)-5):length(input$plot.vars.sel))]))
###      }
###  })

  # Select EARs and years
  output$plot.EAR <- renderUI({
      selectInput(inputId = "plot.EAR.sel", 
                  label = "Select up to 5 EARs to visualize (each EAR = one column in the plot). Only the first 5 will be plotted.",
                  choices = uniqueEARs,
                  multiple = T)
  })
###  # Updater to ensure that only five EARs are selected
###  observeEvent(input$plot.EAR.sel, {
###      if(length(input$plot.EAR.sel) > 5){
###          # Select most recent 5
###          updateSelectInput(session, inputId = input$plot.EAR.sel, selected = c(input$plot.EAR.sel[c((length(input$plot.EAR.sel)-5):length(input$plot.EAR.sel))]))
###      }
###  })
  # Select years (unlimited)
  output$plot.years <- renderUI({
      sliderInput(inputId = "plot.years.sel",
                  label = "Which years should be included in the time-series?",
                  min = min(uniqueYears),
                  max = max(uniqueYears),
                  value = c(min(uniqueYears), max(uniqueYears)),
                  step = 1,
                  sep = "",
                  round = 0)
  })

  #!# Tab 2: Plotting outputs
  ### Only show up if there is something to plot here
  observeEvent(input$plot.plot, {
    output$multivar.comp.plot <- renderPlot({
      gslea::EA.plot.f(variables = input$plot.vars.sel, 
        years = seq(input$plot.years.sel[1], input$plot.years.sel[2]), 
        EARs = as.numeric(input$plot.EAR.sel),
        smoothing = input$plot.smoothing
      )
    })
  })

  #!# Tab 3: X-corr inputs
  # Select x variable
  output$corr.varx <- renderUI({
      selectInput(inputId = "corr.x", 
                  label = "Select the independent variable to plot on the x-axis (from those selected in the 'Variable selection & download' tab).",
                  choices = unique(input$var.select),
                  multiple = F) 
  })
  # Select y variable
  output$corr.vary <- renderUI({
      selectInput(inputId = "corr.y", 
                  label = "Select the dependent variable to plot on the y-axis (from those selected in the 'Variable selection & download' tab).",
                  choices = unique(input$var.select),
                  multiple = F) 
  })

  # Select x EAR
  output$corr.EARx <- renderUI({
      selectInput(inputId = "corr.EAR.x", 
                  label = "Filter the independent data to a specific EAR (choose '10' to select all EARs).",
                  choices = uniqueEARs,
                  multiple = F)
  })
  # Select y EAR
  output$corr.EARy <- renderUI({
      selectInput(inputId = "corr.EAR.y", 
                  label = "Filter the dependent data to a specific EAR (choose '10' to select all EARs).",
                  choices = uniqueEARs,
                  multiple = F)
  })

  # Select years (unlimited)
  output$corr.years <- renderUI({
      sliderInput(inputId = "corr.years.sel",
                  label = "Which years?",
                  min = min(uniqueYears),
                  max = max(uniqueYears),
                  value = c(min(uniqueYears), max(uniqueYears)),
                  step = 1,
                  sep = "",
                  round = 0)
  })

  #!# Tab 3: X-corr outputs
  output$cross.corr.plot <- renderPlot({
    gslea::EA.cor.f(x = input$corr.x, 
                    y = input$corr.y,
                    x.EAR = input$corr.EAR.x,
                    y.EAR = input$corr.EAR.y,
                    years = seq(input$corr.years.sel[1], input$corr.years.sel[2]),
                    diff = input$corr.diff
    )
  })
}

shiny::shinyApp(ui = ui, server = server)