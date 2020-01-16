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


dropdownActionMenu <- function (..., title=NULL, icon = NULL, .list = NULL, header=NULL) {
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  type <- "notifications"
  dropdownClass <- paste0("dropdown ", type, "-menu")
  tags$li(class = dropdownClass, a(href = "#", class = "dropdown-toggle",
    `data-toggle` = "dropdown", icon, title), tags$ul(class = "dropdown-menu",
    if(!is.null(header)) tags$li(class="header",header),
    tags$li(tags$ul(class = "menu", items))))
}

actionItem = function (inputId, text, icon = NULL, tabSelect=FALSE) {
  if(!is.null(icon)) {
    shinydashboard:::tagAssert(icon, type = "i")
    icon <- tagAppendAttributes(icon, class = paste0("text-", "success"))
  }
  if(tabSelect) {
    tags$li(a(onclick=paste0("shinyjs.tabSelect('",inputId,"')"),icon,text))
  } else {
    tags$li(actionLink(inputId,text,icon))
  }
}








# Header elements for the visualization
header <- dashboardHeader(title = "Erich Seamon", titleWidth = 350,
    disable = FALSE,





tags$li(class = "dropdown", tags$p("PhD Dissertation: University of Idaho"), style = "align: left !important; font-size:24px; font-weight:bold; "),

 dropdownActionMenu(title="PhD Chapters",
    actionItem("mnuFirst","Chapter 1"),
    actionItem("mnuSecond","Chapter 2"),
actionItem("mnuSecond","Chapter 3"),
actionItem("mnuSecond","Chapter 4")
)


)


# Sidebar elements for the search visualizations
sidebar <- dashboardSidebar(width = 350,

         tags$style(type="text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"),
tags$style(".shiny-progress {!important;top: 100% !important;right: 50% !important;margin-top: -120px !important;margin-left: -250px;}"),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "custom.js"),
    tags$style(HTML('
   
	.header	      {font-size:24px; 
                      font-weight:bold; 
                      float: left !important;},


# .skin-blue .main-sidebar .sidebar .sidebar-menu  {
#                              background-color: #ffffff;
                              }


')




)
  ),
  sidebarMenu(id = "tabs_paper1",
    menuItem(id = "paper1", icon = icon("dashboard"), text = "PhD Chapter 1: Table of Contents", startExpanded = FALSE, badgeColor = "green", 
             menuSubItem(text = "Introduction", tabName = "paper1_section1"),
             menuSubItem(text = "Climate Impacts and Mitigation of Risk", tabName = "kpi_1"),
menuSubItem(text = "Initial Crop Insurance Exploratory Data Analysis across the Pacific Northwest", tabName = "kpi_2"),
menuSubItem(text = "Refinement of study area, time frame, and commodity type", tabName = "kpi_1"),
menuSubItem(text = "Research Objective: Examining Variability of Crop Insurance Loss for the IPNW", tabName = "kpi_1"),
menuSubItem(text = "Methodology and Model Design", tabName = "kpi_1"),
menuSubItem(text = "Spatial and Temporal considerations", tabName = "kpi_1"),
menuSubItem(text = "Analysis Results", tabName = "kpi_1"),
menuSubItem(text = "Conclusions", tabName = "tab2"),
menuSubItem(text = "APPENDIX: Analysis", tabName = "tab2_val")



    ),

  sidebarMenu(id = "tabs2",
    menuItem(id = "paper2",text = "PhD Chapter 1: Table of Contents", startExpanded = FALSE, badgeColor = "green",
             menuSubItem(text = "Introduction", icon = icon("dashboard"), tabName = "kpis_summary"),
             menuSubItem(text = "Climate Impacts and Mitigation of Risk", tabName = "kpi_1"),
menuSubItem(text = "Initial Crop Insurance Exploratory Data Analysis across the Pacific Northwest", tabName = "kpi_2"),
menuSubItem(text = "Refinement of study area, time frame, and commodity type", tabName = "kpi_1"),
menuSubItem(text = "Research Objective: Examining Variability of Crop Insurance Loss for the IPNW", tabName = "kpi_1"),
menuSubItem(text = "Methodology and Model Design", tabName = "kpi_1"),
menuSubItem(text = "Spatial and Temporal considerations", tabName = "kpi_1"),
menuSubItem(text = "Analysis Results", tabName = "kpi_1"),
menuSubItem(text = "Conclusions", tabName = "tab2"),
menuSubItem(text = "APPENDIX: Analysis", tabName = "tab2_val")

 )
),

# /menuItem


#menuItem("Insurance", tabName = "Insure", 

conditionalPanel("input.tabs_pape1 == 'paper1'"






),



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



selectInput("loss7", label = "Loss Types", choice = c("loss", "log10loss", "count"), selected = "log10loss"),


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

tabsetPanel(id = "tabs_paper1",
tabPanel(id = "paper1", title = "Chapter 1", value='paper1',
            fluidRow(valueBoxOutput("kpi_summary_box_1a", width = 12)),
tabBox(title = "US Map of Insurance Loss", side = "right", width = 12,
fluidRow(leafletOutput("crop_interaction_USdamage"))
            #p('Include documentation via includeMarkdown("./assets/kpis_summary.md") ')
   ))),



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




    tabPanel(title = "NRI", id="tab2",value='tab2_val',
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

dashboardPage(header, sidebar, body, skin = "black")
