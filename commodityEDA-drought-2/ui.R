# Define UI for dataset viewer application
library(shinythemes)

tabPanelAbout <- source("/srv/shiny-server/commodityEDA-Idaho-2/about.R")$value
tabPanelMethods <- source("/srv/shiny-server/commodityEDA-Idaho-2/methods.R")$value        
shinyUI(fluidPage(
     
  # Application title
  #titlePanel("Commodity Loss DROUGHT Dashboard
  
  #tags$div(class="header", checked=NA,
  #             tags$a(href="https://dmine.io", "Back to DMINE.io Dashboards") 
  #    ), 
  # Sidebar with controls to provide a caption, select a dataset,
  # and specify the number of observations to view. Note that
  # changes made to the caption in the textInput control are
  # updated in the output area immediately as you type
  sidebarLayout(
    sidebarPanel(

     tags$a(div(img(src='dmine-commodity.png', width =150), style="text-align: center;"), href="https://dmine.io"),
     #img(src='dmine-commodity.png', width = 100, align = "center"),
     h4("Ag Commodity Loss DROUGHT Dashboard", align = "center"),

#     tags$div(class="header", checked=NA,
#                tags$a(href="https://dmine.io/climate-dashboards/", "Back to DMINE.io", align = "center"),
#                tags$p(),
#		tags$a(href="http://dmine.io:3838/commodityEDA-Idaho-2-normal", "Commodity Loss GENERAL Dashboard", align = "center"),
#		tags$p(),
#                tags$a(href="http://dmine.io:3838/commodityEDA-drought-animation", "Commodity Loss Animation Dashboard", align = "center"),
#		tags$p()
#         ),

    # tags$div(class="header", checked=NA,
    #           tags$a(href="https://dmine.io/climate-dashboards/", "Back to DMINE.io Dashboards")
    #     ),



     p("Instructions: Fill in your state, and the year you are interested in.  This will pre-populate other fields, such as availabile commodities."),
     p("After selecting a month and commodity, you can look at the results information in the horizontal tabs at the top."),

     uiOutput("commoditycontrols"),



     selectInput("state", label = "State", choice = c("Washington", "Idaho"), selected = "Idaho"),
     selectInput("year", label = "Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected = "2002"),

#selectInput("month", "month of interest.  Use a number (1-12)", choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
#     selectInput("commodity", "Choose a commodity:", 
#                  choices = c("Apples", "Wheat", "Barley", "Sugar Beets", "Cherries", "Grapes", "Adjusted Gross Revenue", "Green Peas", "All Other Crops", "Pears", "Canola", "Sweet Corn", "Mint", "Potatoes", "Dry Peas", "Processing Beans", "Dry Beans", "Onions", "Cranberries", "Corn", "Oats", "Alfalfa Seed", "Fresh Apricots", "Fresh Freestone Peaches", "Nursery (GF&C)", "Fresh Nectarines", "Mustard", "Blueberries", "Adjusted Gross Revenue-Lite", "Plums", "Soybeans", "Whole Farm Revenue Protection", "Buckwheat")),
     
     uiOutput("countycontrols"),
     uiOutput("damagecontrols"), 
     selectInput("climate", label = "Climate Variables (choose one)", choice = c("pr", "tmmx"), selected = "pr")
    ),
    
 
    # Show the caption, a summary of the dataset and an HTML 
	 # table with the requested number of observations
	 mainPanel(
         #tags$style(type="text/css",
         # ".shiny-output-error { visibility: hidden; }",
         # ".shiny-output-error:before { visibility: hidden; }"),
    



		tabsetPanel(
 			#tabPanel("Month",
                        #        fluidRow(
			#		plotOutput("plot7a", height = 300),
                        #                plotOutput("plot", height = 1000))),
			#tabPanel("Annual Damage",
			#	fluidRow(
			#		plotOutput("plot4", height = 1000),
			#		dataTableOutput("plot2aa"))),


                        tabPanel("Annual Drought",
                                fluidRow(
                                        plotOutput("plot7c", height = 1000))),

			tabPanel("Annual Damage",
        			fluidRow(
                                        plotOutput("plot5"),
					dataTableOutput("plot8")))
				        #plotOutput("plot5", height = 800))),



 			#tabPanel("Drought claim animation",
                        #        fluidRow(
      			#		uiOutput("video"))),

#			tabPanelAbout(),
 			#tabPanelMethods())
)

 
  )
))
)
