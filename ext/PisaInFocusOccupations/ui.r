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
                choices = c("Albania", "Argentina", "Australia", "Austria", "Belgium",  "Bulgaria", "Chile", "China-Shanghai", "Colombia", "Costa Rica", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Indonesia", "Ireland", "Israel", "Italy", "Japan", "Jordan", "Kazakhstan", "Korea", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macao-China", "Malaysia", "Mexico", "Montenegro", "Namibia", "Namibia", "Netherlands", "New Zealand", "Norway", "Peru", "Poland", "Portugal", "Qatar", "Romania", "Russian Federation", "Serbia", "Singapore", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Thailand", "Tunisia", "Turkey", "United Arab Emirates", "United Kingdom", "United States of America", "Uruguay", "Vietnam"),
                selected = "Germany"),
    conditionalPanel(
      condition="input.TabPanel == 'Trees'",
      checkboxInput("showCI", "Show confidence intervals", TRUE)
    ),
    conditionalPanel(
      condition="(input.TabPanel == 'Two countries') | (input.TabPanel == 'Two countries level 2')",
      selectInput(inputId = "variable1",
                  label = "3. Select the country to compare with",
                  choices = c("Albania", "Argentina", "Australia", "Austria", "Belgium",  "Bulgaria", "Chile", "China-Shanghai", "Colombia", "Costa Rica", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Indonesia", "Ireland", "Israel", "Italy", "Japan", "Jordan", "Kazakhstan", "Korea", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macao-China", "Malaysia", "Mexico", "Montenegro", "Namibia", "Namibia", "Netherlands", "New Zealand", "Norway", "Peru", "Poland", "Portugal", "Qatar", "Romania", "Russian Federation", "Serbia", "Singapore", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Thailand", "Tunisia", "Turkey", "United Arab Emirates", "United Kingdom", "United States of America", "Uruguay", "Vietnam"),
                  selected = "Finland")
    ),
    conditionalPanel(
      condition="input.TabPanel == 'Country perspective'",
      selectInput(inputId = "occupation",
                  label = "Select the occupation of interest",
                  choices = c("1 Managers ", "11 Chief executives, senior officials and legislators ", 
                              "12 Administrative and commercial managers ", "13 Production and specialized services managers ", 
                              "14 Hospitality, retail and other services managers ", "2 Professionals ", 
                              "21 Science and engineering professionals ", "22 Health professionals ", "23 Teaching professionals ", "24 Business and administration professionals ", 
                              "3 Technicians and associate professionals ", "31 Science and engineering associate professionals ", "32 Health associate professionals ", "33 Business and administration associate professional ", "34 Legal, social, cultural and related associate professionals ", 
                              "4 Clerical support workers ", "41 General and keyboard clerks ", "42 Customer services clerks ", 
                              "5 Service and sales workers ", "51 Personal service workers ", "52 Sales workers ", 
                              "6 Skilled agricultural, forestry and fishery workers ", "61 Market-oriented skilled agricultural workers ", "62 Market-oriented skilled forestry, fishing and hunting workers ", 
                              "7 Craft and related trades workers ", "71 Building and related trades workers, excluding electricians ", "72 Metal, machinery and related trades workers ", "73 Handicraft and printing workers ", "74 Electrical and electronic trades workers ", "75 Food processing, wood working, garment and other craft and related trades workers ", 
                              "8 Plant and machine operators, and assemblers ", "81 Stationary plant and machine operators ", "82 Assemblers ", "83 Drivers and mobile plant operators ", 
                              "9 Elementary occupations ", "91 Cleaners and helpers ", "92 Agricultural, forestry and fishery labourers ", "93 Labourers in mining, construction, manufacturing and transport ", "95 Street and related sales and service workers "),
                  selected = "21 Science and engineering professionals ")
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
