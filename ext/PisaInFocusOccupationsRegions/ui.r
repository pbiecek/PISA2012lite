load("AllAvgSdsDec05b_regions.rda")
AllAvgSdsREG <- AllAvgSds
load("AllAvgSdsDec05.rda")
load("regNames.rda")

#cntFul <- c("Belgium", "Canada", "Spain", "United Kingdom", "Italy")
#cntSho <- c("BEL", "CAN", "ESP", "GBR", "ITA")
#regionCodes <- substr(rownames(AllAvgSdsREG[[1]]), 5, 10)

cnnts <- sort(rownames(AllAvgSds[[1]]))[c(-4,-33)]

shinyUI(pageWithSidebar(
  headerPanel(div(h3("Occupations@PISA2012"), "How much can we infer about a student's performance in school by looking at what his or her parents do for a living? To find out, PISA 2012 asked participating students about their parents' occupations. ", strong("Occupations@PISA2012"), " is a web-based application that allows you to explore the relationship between parents' occupations and their children's performance in mathematics, reading and science - in your own country and in other countries. ")),#"Occupations@PISA 2012"
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
    p("Choose perspective ->"),HTML("<hr/>"),
    selectInput(inputId = "subject",
                label = "Compare results by subject",
                choices = c("Mathematics", "Reading","Science"),
                selected = "Mathematics"),
    selectInput(inputId = "variable",
                label = " and country ",
                choices = cnnts,
                selected = "Germany"),
    # regions
    conditionalPanel(
      condition="(input.variable == 'Belgium') & (input.TabPanel == 'Two countries' | input.TabPanel == 'Rainbow plot')",
      selectInput(inputId = "variableBelgium", label = " and region ", choices = c("All", regNames[grep(regNames, pattern="BEL")]), selected = "All") ),
    conditionalPanel(
      condition="(input.variable == 'Canada') & (input.TabPanel == 'Two countries' | input.TabPanel == 'Rainbow plot')",
      selectInput(inputId = "variableCanada", label = " and region ", choices = c("All",regNames[grep((regNames), pattern="CAN")]), selected = "All") ),
    conditionalPanel(
      condition="(input.variable == 'Spain') & (input.TabPanel == 'Two countries' | input.TabPanel == 'Rainbow plot')",
      selectInput(inputId = "variableSpain", label = " and region ", choices = c("All", regNames[grep((regNames), pattern="ESP")]), selected = "All") ),
    conditionalPanel(
      condition="(input.variable == 'United Kingdom') & (input.TabPanel == 'Two countries' | input.TabPanel == 'Rainbow plot')",
      selectInput(inputId = "variableUK", label = " and region ", choices = c("All", regNames[grep((regNames),  pattern="GBR")]), selected = "All") ),
    conditionalPanel(
      condition="(input.variable == 'Italy') & (input.TabPanel == 'Two countries' | input.TabPanel == 'Rainbow plot')",
      selectInput(inputId = "variableItaly", label = " and region ", choices = c("All", regNames[grep((regNames), pattern="ITA")]), selected = "All") ),
    
    conditionalPanel(
      condition="input.TabPanel == 'Trees'",
      checkboxInput("showCI", "Show standard errors", TRUE)
    ),
    conditionalPanel(
      condition="(input.TabPanel == 'Two countries') | (input.TabPanel == 'Two countries level 2')",
      selectInput(inputId = "variable1",
                  label = "Choose the country to compare with",
                  choices = cnnts,
                  selected = "Finland")
    ),
    # regions
    conditionalPanel(
      condition="(input.variable1 == 'Belgium') & input.TabPanel == 'Two countries'",
      selectInput(inputId = "variable2Belgium", label = " and region ", choices = c("All", regNames[grep((regNames), pattern="BEL")]), selected = "All") ),
    conditionalPanel(
      condition="(input.variable1 == 'Canada')  & input.TabPanel == 'Two countries'",
      selectInput(inputId = "variable2Canada", label = " and region ", choices = c("All", regNames[grep((regNames), pattern="CAN")]), selected = "All") ),
    conditionalPanel(
      condition="(input.variable1 == 'Spain')  & input.TabPanel == 'Two countries'",
      selectInput(inputId = "variable2Spain", label = " and region ", choices = c("All", regNames[grep((regNames), pattern="ESP")]), selected = "All") ),
    conditionalPanel(
      condition="(input.variable1 == 'United Kingdom')  & input.TabPanel == 'Two countries'",
      selectInput(inputId = "variable2UK", label = " and region ", choices = c("All", regNames[grep((regNames), pattern="GBR")]), selected = "All") ),
    conditionalPanel(
      condition="(input.variable1 == 'Italy') & input.TabPanel == 'Two countries'",
      selectInput(inputId = "variable2Italy", label = " and region ", choices = c("All", regNames[grep((regNames), pattern="ITA")]), selected = "All") ),
    
    HTML("<hr/>"),
    sliderInput("range", "Set range for vertical axis:",
                min = 300, max = 700, value = c(475,575), step= 5),
    HTML("<hr/>You can download the <a href='OccupationsPISA2012.pdf'>findings</a> in our download zone</br></br>")),
  
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
