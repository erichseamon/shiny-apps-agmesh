library(shiny)
library(shinydashboard)
library(maptools)
library(raster)
library(data.table)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(shinyalert)

#library(plotly)
# library(dygraphs) # optional, used for dygraphs

# Header elements for the visualization
header <- dashboardHeader(title = "US Ag Insure", disable = FALSE)


# Sidebar elements for the search visualizations
sidebar <- dashboardSidebar(

         tags$style(type="text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"),
tags$style(".shiny-progress {!important;top: 100% !important;right: 50% !important;margin-top: -120px !important;margin-left: -250px;}"),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "custom.js"),
    tags$style(HTML('
   
    .skin-green .main-sidebar {
                              background-color: #2a6894;
			      }
')




)
  ),
  sidebarMenu(id = "tabs",
    menuItem(text = "Instructions", 
             menuSubItem(text = "How to use SHEAF EDA", icon = icon("dashboard"), tabName = "kpis_summary"),
             menuSubItem(text = "Background and Data", tabName = "kpi_1")
    ), # /menuItem


#menuItem("Insurance", tabName = "Insure", 

conditionalPanel("input.navbar == 'tab1_val'",

#    # this is where other menuItems & menuSubItems would go
# selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected = "Idaho"),

#selectInput("year", label = "Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected = "2002"),
#selectInput("month", label = "Month", choice = c(1,2,3,4,5,6,7,8,9,10,11,12), selected = 1),


#uiOutput("commoditycontrols"),
#uiOutput("countycontrols"),




#selectInput("state7", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

#selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

 selectInput("year7", label = "Year", choice = c("1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2001"),

#selectInput("month7", "month of interest.  Use a number (1-12)", choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

uiOutput("commodityUS_damage_controls"),

#uiOutput("county7controls"),

uiOutput("damageUS_damage_controls"),

#selectInput("startyear7", label = "Start Year", choice = c("1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2004"),


#     selectInput("endyear7", label = "End Year", choice = c("1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2015"),



selectInput("loss7", label = "Loss Types", choice = c("loss", "log10loss", "count", "lossperclaim", "lossperacre"), selected = "log10loss"),


column(1, tags$br(actionButton("submit4", "Submit")))

#downloadButton("report", "Submit")
),

conditionalPanel("input.navbar == 'tab2_val'",

selectInput("NRI_state", label = "NRI State", choice = c("Alabama",        "Arizona",        "Arkansas",       "California",    "Colorado",       "Connecticut",
 "Delaware",       "Florida",        "Georgia",        "Hawaii",         "Idaho",          "Illinois",       "Indiana",
 "Iowa",           "Kansas",         "Kentucky" ,      "Louisiana",      "Maine" ,         "Maryland",       "Massachusetts",
 "Michigan",       "Minnesota",      "Mississippi",    "Missouri",       "Montana",        "Nebraska",       "Nevada",
 "New Hampshire",  "New Jersey",     "New Mexico",     "New York",       "North Carolina", "North Dakota",   "Ohio",
 "Oklahoma",       "Oregon",         "Pennsylvania",   "Rhode Island",   "South Carolina", "South Dakota",   "Tennessee",
 "Texas",          "Utah",           "Vermont",        "Virginia",       "Washington",     "West Virginia",  "Wisconsin",
 "Wyoming"), selected = "Washington"),


selectInput("NRI_fields", label = "NRI variables", choice = c("Cropland", "CRP_land", "Pastureland", "Rangeland",  "Forest_land", "Other_rural_land", "Total_rural_land"), selected = "Cropland"),

selectInput("NRI_year", label = "NRI year", choice = c("1987", "1992", "1997", "2002", "2007", "2012"), selected = "1987"),

column(1, tags$br(actionButton("submit5", "Submit")))
),



conditionalPanel("input.navbar == 'tab3_val'",

selectInput("ag_state", label = "Ag Census State", choice = c("Alabama",        "Arizona",        "Arkansas",       "California",    "Colorado",       "Connecticut",
 "Delaware",       "Florida",        "Georgia",        "Hawaii",         "Idaho",          "Illinois",       "Indiana",
 "Iowa",           "Kansas",         "Kentucky" ,      "Louisiana",      "Maine" ,         "Maryland",       "Massachusetts",
 "Michigan",       "Minnesota",      "Mississippi",    "Missouri",       "Montana",        "Nebraska",       "Nevada",
 "New Hampshire",  "New Jersey",     "New Mexico",     "New York",       "North Carolina", "North Dakota",   "Ohio",
 "Oklahoma",       "Oregon",         "Pennsylvania",   "Rhode Island",   "South Carolina", "South Dakota",   "Tennessee",
 "Texas",          "Utah",           "Vermont",        "Virginia",       "Washington",     "West Virginia",  "Wisconsin",
 "Wyoming"), selected = "Washington"),



selectInput("ag_year", label = "AgCensus Year", choice = c("2012"), selected="2012"),

uiOutput("agcensuscontrols"),

column(1, tags$br(actionButton("submit6", "Submit")))

),

conditionalPanel("input.navbar == 'tab4_val'",

#selectInput("ag_state", label = "Ag Census State", choice = c("Alabama",        "Arizona",        "Arkansas",       "California",    "Colorado",       "Connecticut",
# "Delaware",       "Florida",        "Georgia",        "Hawaii",         "Idaho",          "Illinois",       "Indiana",
# "Iowa",           "Kansas",         "Kentucky" ,      "Louisiana",      "Maine" ,         "Maryland",       "Massachusetts",
# "Michigan",       "Minnesota",      "Mississippi",    "Missouri",       "Montana",        "Nebraska",       "Nevada",
# "New Hampshire",  "New Jersey",     "New Mexico",     "New York",       "North Carolina", "North Dakota",   "Ohio",
# "Oklahoma",       "Oregon",         "Pennsylvania",   "Rhode Island",   "South Carolina", "South Dakota",   "Tennessee",
# "Texas",          "Utah",           "Vermont",        "Virginia",       "Washington",     "West Virginia",  "Wisconsin",
# "Wyoming"), selected = "Washington"),


selectInput("eqipyearcontrols", label = "Equip Year Choices", choice = c("planned_year", "Applied.Year", "Contract.Year"), selected="planned_year"),

uiOutput("eqipcontrols"),

uiOutput("eqipyear"),

column(1, tags$br(actionButton("submit7", "Submit")))

),

conditionalPanel("input.navbar == 'tab5_val'",

#selectInput("ag_state", label = "Ag Census State", choice = c("Alabama",        "Arizona",        "Arkansas",       "California",    "Colorado",       "Connecticut",
# "Delaware",       "Florida",        "Georgia",        "Hawaii",         "Idaho",          "Illinois",       "Indiana",
# "Iowa",           "Kansas",         "Kentucky" ,      "Louisiana",      "Maine" ,         "Maryland",       "Massachusetts",
# "Michigan",       "Minnesota",      "Mississippi",    "Missouri",       "Montana",        "Nebraska",       "Nevada",
# "New Hampshire",  "New Jersey",     "New Mexico",     "New York",       "North Carolina", "North Dakota",   "Ohio",
# "Oklahoma",       "Oregon",         "Pennsylvania",   "Rhode Island",   "South Carolina", "South Dakota",   "Tennessee",
# "Texas",          "Utah",           "Vermont",        "Virginia",       "Washington",     "West Virginia",  "Wisconsin",
# "Wyoming"), selected = "Washington"),


selectInput("totalcontrols", label = "TOTAL Value Types", choice = c("dollars", "dollars_per_operation", "dollars_per_acre", "ag_income"), selected="dollars"),


selectInput("totalyear", label = "TOTAL Year", choice = c("2012")),

column(1, tags$br(actionButton("submit8", "Submit")))

)









  ) # /sidebarMenu




#actionButton("submit4", "Submit")










) # /dashboardSidebar

#Body elements for the search visualizations.
body <- dashboardBody(

  tabsetPanel(id = "navbar", 
    tabPanel(id = "tab1", title = "INSURANCE", value='tab1_val', 
            fluidRow(valueBoxOutput("kpi_summary_box_1a", width = 12)),
                     #valueBoxOutput("kpi_summary_box_2", width = 6)),
                     #valueBoxOutput("kpi_summary_box_3", width = 4)),
tabBox(title = "US Map of Insurance Loss", side = "right", width = 12, 
fluidRow(leafletOutput("crop_interaction_USdamage"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   ),


tabBox(title = "TOTAL Claim Loss by State", side = "right", width = 12,        	    
fluidRow(plotOutput("crop_interaction_USbarplot")) 
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   ),

tabBox(title = "MEAN Claim Loss by State", side = "right",  width = 12, 
fluidRow(plotOutput("crop_interaction_USbarplot2"))



)),


#tabBox(width = 4,
#fluidRow(plotOutput("cropnormals2"))             
#            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
#   )),




    tabPanel(titl = "NRI", id="tab2",value='tab2_val',
 fluidRow(valueBoxOutput("kpi_summary_box_1b", width = 12)),
#                     valueBoxOutput("kpi_summary_box_2", width = 4),
#                     valueBoxOutput("kpi_summary_box_3", width = 4)),


tabBox(title = "US Map of NRI", side = "right", width = 12,
fluidRow(leafletOutput("crop_interaction_USmap_NRI"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   ),

tabBox(title = "test", side = "right", width = 12,
fluidRow(plotOutput("crop_interaction_USbarplot_NRI"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   ),

tabBox(title = "test", side = "right", width = 12,
fluidRow(plotOutput("crop_interaction_USbarplot_NRIall"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   )





),


    tabPanel(title = "Eqip", id="tab4",value='tab4_val',
# fluidRow(valueBoxOutput("kpi_summary_box_1b_agcensus", width = 12)),
#                     valueBoxOutput("kpi_summary_box_2", width = 4),
#                     valueBoxOutput("kpi_summary_box_3", width = 4)),


tabBox(title = "US Map of Eqip", side = "right", width = 12,
fluidRow(leafletOutput("crop_interaction_USmap_eqip"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   )

#tabBox(width = 12,
#fluidRow(plotOutput("crop_interaction_USbarplot_eqip"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
#   ),

#tabBox(width = 12,
#fluidRow(plotOutput("crop_interaction_USbarplot_eqipall"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
#   )





),


    tabPanel(title = "AgCensus", id="tab3",value='tab3_val',
 fluidRow(valueBoxOutput("kpi_summary_box_1b_agcensus", width = 12)),
#                     valueBoxOutput("kpi_summary_box_2", width = 4),
#                     valueBoxOutput("kpi_summary_box_3", width = 4)),


tabBox(title = "US Map of AgCensus", side = "right", width = 12,
fluidRow(leafletOutput("crop_interaction_USmap_agcensus1"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   )

#tabBox(side = "right", width = 12,
#fluidRow(plotOutput("crop_interaction_USbarplot_NRI"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
#   ),

#tabBox(side = "right", width = 12,
#fluidRow(plotOutput("crop_interaction_USbarplot_NRIall"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
#   )





),

    tabPanel(title = "TOTAL assets/county", id="tab5",value='tab5_val',
# fluidRow(valueBoxOutput("kpi_summary_box_1b_agcensus", width = 12)),
#                     valueBoxOutput("kpi_summary_box_2", width = 4),
#                     valueBoxOutput("kpi_summary_box_3", width = 4)),


tabBox(title = "US TOTAL assets/county", side = "right", width = 12,
fluidRow(leafletOutput("crop_interaction_USmap_total"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   ),

tabBox(side = "right", width = 12,
fluidRow(plotOutput("crop_interaction_USbarplot_total1"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   )

#tabBox(side = "right", width = 12,
#fluidRow(plotOutput("crop_interaction_USbarplot_NRIall"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
#   )





)





            # e.g. plotOutput(), textOutput(), dygraphOutput(), etc.
            #p('includeMarkdown("./assets/kpi_1.md") is kinda like a README for this module'))
)   # /tabItems
) # /dashboardBody

dashboardPage(header, sidebar, body, skin = "green")
