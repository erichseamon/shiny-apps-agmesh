library(shiny)
library(shinydashboard)
library(maptools)
library(raster)
library(data.table)
library(leaflet)
# library(dygraphs) # optional, used for dygraphs

# Header elements for the visualization
header <- dashboardHeader(title = "DMINE DASHBOARD", disable = FALSE)

# Sidebar elements for the search visualizations
sidebar <- dashboardSidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "custom.js"),
    tags$style(HTML('

   .skin-blue .main-sidebar {
                              background-color: #2a6894;
                              }
'))
  ),
  sidebarMenu(
    menuItem(text = "KPIs",
             menuSubItem(text = "Summary", tabName = "kpis_summary"),
             menuSubItem(text = "KPI 1", tabName = "kpi_1")
    ), # /menuItem




    # this is where other menuItems & menuSubItems would go
 selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected = "Idaho"),

selectInput("year", label = "Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected = "2002"),
selectInput("month", label = "Month", choice = c(1,2,3,4,5,6,7,8,9,10,11,12), selected = 1),


uiOutput("commoditycontrols"),
uiOutput("countycontrols"),
downloadButton("report", "Submit")








  ) # /sidebarMenu
) # /dashboardSidebar

#Body elements for the search visualizations.
body <- dashboardBody(
  tabsetPanel(
    tabPanel("WHEAT LOSS PREDICTION",
            fluidRow(valueBoxOutput("kpi_summary_box_1", width = 4),
                     valueBoxOutput("kpi_summary_box_2", width = 4),
                     valueBoxOutput("kpi_summary_box_3", width = 4)),

tabBox(width = 4,
fluidRow(plotOutput("plot4"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   ),


tabBox(width = 4,       	    
fluidRow(leafletOutput("myMap")) 
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   ),


tabBox(width = 4,
fluidRow(plotOutput("plot7y"))             
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   )),





    tabPanel("DROUGHT INFLUENCES ON WHEAT LOSS",
            # e.g. plotOutput(), textOutput(), dygraphOutput(), etc.
            p('includeMarkdown("./assets/kpi_1.md") is kinda like a README for this module'))
)   # /tabItems
) # /dashboardBody

dashboardPage(header, sidebar, body, skin = "blue")
