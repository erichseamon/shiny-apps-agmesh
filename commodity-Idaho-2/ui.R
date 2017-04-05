# Define UI for dataset viewer application
library(shinythemes)
library(shinyBS)
tabPanelAbout <- source("/srv/shiny-server/commodityEDA-Idaho-2/about.R")$value
tabPanelMethods <- source("/srv/shiny-server/commodityEDA-Idaho-2/methods.R")$value        

#fluidPage(theme = shinytheme("united"))


shinyUI(fluidPage(
tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),


     
  # Application title
  #titlePanel("Agricultural Crop Analysis Dashboard"),
  
  # Sidebar with controls to provide a caption, select a dataset,
  # and specify the number of observations to view. Note that
  # changes made to the caption in the textInput control are
  # updated in the output area immediately as you type
  sidebarLayout(
    sidebarPanel(


     tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
     #div(img(src='d1.png', width = 250), style="text-align: center;"),
     #img(src='dmine-commodity.png', width = 100, align = "center"),
     #h4("Ag Dashboard: Claim Loss"),

 tags$div(class="h4", checked=NA,
           tags$b(href="https://dmine.io/climate-dashboards/", "DMINE Agriculture Dashboard")
     ),

 tags$div(class="h5", checked=NA, style="color:darkblue",
           tags$b(href="https://dmine.io/climate-dashboards/", "Insurance Crop Claim Loss") 
),   



     p("Instructions: Fill in your state, and the year you are interested in.  This will pre-populate other fields, such as availabile commodities."),
     #p("After selecting a month and commodity, you can look at the results information in the horizontal tabs at the top."),
     selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected = "Idaho"),
     #bsTooltip("state", "Choose a state.  We currently have Washington, Oregon, and Idaho processed.",
      #   "top", options = list(container = "body")),
     selectInput("year", label = "Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected = "2002"),
     #bsTooltip("year", "Choose a year.  We currently have 2001-2015, processing 2016.",
     #    "top", options = list(container = "body")),

# selectInput("climate", label = "Climate Variables (choose one)", choice = c("pr", "tmmx"), selected = "pr"),
     uiOutput("commoditycontrols"),
#bsTooltip("commoditycontrols", "Choose a commodity.  Commodity listings will vary by year and state.",
#         "top", options = list(container = "body")),

selectInput("month", "month of interest.  Use a number (1-12)", choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
#bsTooltip("month", "Choose a month.",
#         "top", options = list(container = "body"))
#     selectInput("commodity", "Choose a commodity:", 
#                  choices = c("Apples", "Wheat", "Barley", "Sugar Beets", "Cherries", "Grapes", "Adjusted Gross Revenue", "Green Peas", "All Other Crops", "Pears", "Canola", "Sweet Corn", "Mint", "Potatoes", "Dry Peas", "Processing Beans", "Dry Beans", "Onions", "Cranberries", "Corn", "Oats", "Alfalfa Seed", "Fresh Apricots", "Fresh Freestone Peaches", "Nursery (GF&C)", "Fresh Nectarines", "Mustard", "Blueberries", "Adjusted Gross Revenue-Lite", "Plums", "Soybeans", "Whole Farm Revenue Protection", "Buckwheat")),
     
     #uiOutput("countycontrols")
     #uiOutput("damagecontrols") 
     downloadButton("report", "Generate report")
    , width=3),
    
 
    # Show the caption, a summary of the dataset and an HTML 
	 # table with the requested number of observations
	 mainPanel(
         #tags$style(type="text/css",
         # ".shiny-output-error { visibility: hidden; }",
         # ".shiny-output-error:before { visibility: hidden; }"),
    

tabPanelAbout(),

		tabsetPanel(
 			tabPanel("Monthly Loss/County",


                                fluidRow(

					plotOutput("plot", height = 500)), icon = icon("bar-chart-o")),
			
			tabPanel("Annual Loss/County",
				fluidRow(
					plotOutput("plot4", height = 500)), icon = icon("bar-chart-o")),

			tabPanel("Damage/County",
                                fluidRow(
                                        plotOutput("plot7a", height = 300),
                                        plotOutput("plot2", height = 300)), icon = icon("bar-chart-o")),

                        #tabPanel("Drought",
                        #        fluidRow(
                        #                plotOutput("plot7c", height = 1000))),

			#tabPanel("Damage",
        		#	fluidRow(
                        #                plotOutput("plot7", height = 300),
			#		dataTableOutput("plot8"),
			#	        plotOutput("plot5", height = 800))),
#			tabPanelAbout(),
 			tabPanelMethods())


 
  )
))
)
