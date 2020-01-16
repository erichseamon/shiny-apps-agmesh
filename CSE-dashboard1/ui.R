library(shiny)
library(shinydashboard)
library(maptools)
library(raster)
library(data.table)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(plotly)
# library(dygraphs) # optional, used for dygraphs

# Header elements for the visualization
header <- dashboardHeader(title = "AG DASHBOARD", disable = FALSE)

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
    menuItem(text = "Instructions",
             menuSubItem(text = "How to use Crop Normals", tabName = "kpis_summary"),
             menuSubItem(text = "Background and Data", tabName = "kpi_1")
    ), # /menuItem




#    # this is where other menuItems & menuSubItems would go
# selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected = "Idaho"),

#selectInput("year", label = "Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected = "2002"),
#selectInput("month", label = "Month", choice = c(1,2,3,4,5,6,7,8,9,10,11,12), selected = 1),


#uiOutput("commoditycontrols"),
#uiOutput("countycontrols"),




selectInput("state7", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

#selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

 selectInput("year7", label = "Year", choice = c("1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2001"),

#selectInput("month7", "month of interest.  Use a number (1-12)", choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

uiOutput("commodity7controls"),

uiOutput("county7controls"),

uiOutput("damage7controls"),

selectInput("startyear7", label = "Start Year", choice = c("1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2004"),


     selectInput("endyear7", label = "End Year", choice = c("1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2015"),




actionButton("submit4", "Submit")

#downloadButton("report", "Submit")

  ) # /sidebarMenu




#actionButton("submit4", "Submit")










) # /dashboardSidebar

#Body elements for the search visualizations.
body <- dashboardBody(
  tabsetPanel(
    tabPanel("CROP NORMALS", value=7,
            fluidRow(valueBoxOutput("kpi_summary_box_1", width = 6),
                     valueBoxOutput("kpi_summary_box_2", width = 6)),
                     #valueBoxOutput("kpi_summary_box_3", width = 4)),

tabBox(width = 6,
fluidRow(plotOutput("cropnormals1"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   ),


tabBox(width = 6,       	    
fluidRow(leafletOutput("myMap_anomaly")) 
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   )),


#tabBox(width = 4,
#fluidRow(plotOutput("cropnormals2"))             
#            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
#   )),





    tabPanel("DROUGHT CLAIM INTERACTIONS", value=13,
# fluidRow(valueBoxOutput("kpi_summary_box_1", width = 4),
#                     valueBoxOutput("kpi_summary_box_2", width = 4),
#                     valueBoxOutput("kpi_summary_box_3", width = 4)),


tabBox(width = 6,
fluidRow(plotlyOutput("crop_interaction1"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   ),

tabBox(width = 6,
fluidRow(plotOutput("crop_interaction2"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   ))







            # e.g. plotOutput(), textOutput(), dygraphOutput(), etc.
            #p('includeMarkdown("./assets/kpi_1.md") is kinda like a README for this module'))
)   # /tabItems
) # /dashboardBody

dashboardPage(header, sidebar, body, skin = "blue")
