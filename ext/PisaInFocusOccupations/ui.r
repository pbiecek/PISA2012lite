shinyUI(pageWithSidebar(
  headerPanel("Occupations@PISA 2012"),

  sidebarPanel(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="mojcss.css"),
      tags$script("(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
  ga('create', 'UA-5650686-6', 'icm.edu.pl');
  ga('send', 'pageview');")
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
      checkboxInput("showCI", "Show standard errors", TRUE)
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
