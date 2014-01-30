shinyUI(pageWithSidebar(
  headerPanel("Occupations@PISA 2012"),

  sidebarPanel(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="mojcss.css")
    ),
    selectInput(inputId = "subject",
                label = "1. Select the subject",
                choices = c("MATH", "READ","SCIE"),
                selected = "MATH"),
    selectInput(inputId = "variable",
                label = "2. Select the country of interest",
                choices = c("Albania", "Argentina", "Australia", "Austria", "Belgium",  "Bulgaria", "Chile", "Colombia", "Costa Rica", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Indonesia", "Ireland", "Israel", "Italy", "Japan", "Jordan", "Kazakhstan", "Korea", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macao-China", "Malaysia", "Mexico", "Montenegro", "Namibia", "Namibia", "Netherlands", "New Zealand", "Norway", "Peru", "Poland", "Portugal", "Qatar", "Romania", "Russian Federation", "Serbia", "Shanghai-China", "Singapore", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Thailand", "Tunisia", "Turkey", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Vietnam"),
                selected = "Germany"),
    conditionalPanel(
      condition="input.TabPanel == 'Trees'",
      checkboxInput("showCI", "Show confidence intervals", TRUE)
    ),
    conditionalPanel(
      condition="(input.TabPanel == 'Two countries') | (input.TabPanel == 'Two countries level 2')",
      selectInput(inputId = "variable1",
                  label = "3. Select the country to compare with",
                  choices = c("Albania", "Argentina", "Australia", "Austria", "Belgium",  "Bulgaria", "Chile", "Colombia", "Costa Rica", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Indonesia", "Ireland", "Israel", "Italy", "Japan", "Jordan", "Kazakhstan", "Korea", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macao-China", "Malaysia", "Mexico", "Montenegro", "Namibia", "Namibia", "Netherlands", "New Zealand", "Norway", "Peru", "Poland", "Portugal", "Qatar", "Romania", "Russian Federation", "Serbia", "Shanghai-China", "Singapore", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Thailand", "Tunisia", "Turkey", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Vietnam"),
                  selected = "Finland")
    ),
    sliderInput("range", "Set range for vertical axis:",
                min = 300, max = 700, value = c(450,600), step= 5),
    HTML("<hr/>Or <br/><a href='OccupationsPISA2012.pdf'>download the full report</a> </br>with country profiles")),
  
      mainPanel(
        tabsetPanel(
          id = "TabPanel",
          tabPanel("Rainbow plot", plotOutput("ColorTrees")),
          tabPanel("Trees", plotOutput("Trees")),
          tabPanel("Two countries", plotOutput("TwoCnts")),
          tabPanel("Two countries level 2", plotOutput("TwoCnts2")),
          tabPanel("Map of countries", plotOutput("Map")),
          tabPanel("Download zone", htmlOutput("DownloadZone"))
        )
    )
))
