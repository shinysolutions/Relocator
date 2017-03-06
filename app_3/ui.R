shinyUI(bootstrapPage(
  tagList(tags$head(tags$link(rel="stylesheet", type="text/css",href="../www/style.css"))),
  
  HTML("<h1>app_3</h1>"),
  
  ## Login module;
  div(class = "input", 
      sliderInput("N", "Number of colors", 3, 110, value = 5),
      sliderInput("R", "Radius", 0, 365, value = c(100, 300), step = 5),
      sliderInput("A", "Angles", 0, 360, value = c(30, 300), step = 5),
      checkboxInput("dim", "Dimmed", value = FALSE),
      HTML("<hr>"),
      submitButton(text = "Apply Changes", icon = NULL),
      textInput(inputId = "log", label = "", value = "")),
  
  div(class = "output", 
      tabsetPanel(
        tabPanel("Points", plotOutput("pt", width = "730px", height = "750px")),
        tabPanel("Plot",  plotOutput("plot", width = "730px", height = "750px")),
        tabPanel("Data", tableOutput("tab"), textOutput("txt"))
      )
  )
  
))
