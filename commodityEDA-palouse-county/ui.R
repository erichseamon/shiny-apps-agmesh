# Define UI for dataset viewer application
library(shinythemes)

tabPanelAbout <- source("/srv/shiny-server/commodityEDA-palouse-county/about.R")$value
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
     h4("Ag Commodity Loss PALOUSE Dashboard", align = "center"),

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



     #p("Instructions: Pick a commodity of interest.  The dashboard will dynamically fill in all the other fields for you on startup.  At any time, you can change the fields to your area or time of interest."),

     #uiOutput("commoditycontrols"),

     #selectInput("firstyear", label = "Start year:", selected = 2007, choice = c(2007:2015)), 
     #selectInput("lastyear", label = "End year:", choice = c(2007:2015), selected = 2015),
#     dateRangeInput("timerange", label = "Year Range", start = "2007-01-01", end = "2015-01-01", startview = "decade", format="yyyy", min = "2007", max = "2015"),
     #selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Washington"),
     #selectInput("year", label = "Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2001"),
#selectInput("month", "month of interest.  Use a number (1-12)", choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
#     selectInput("commodity", "Choose a commodity:", 
#                  choices = c("Apples", "Wheat", "Barley", "Sugar Beets", "Cherries", "Grapes", "Adjusted Gross Revenue", "Green Peas", "All Other Crops", "Pears", "Canola", "Sweet Corn", "Mint", "Potatoes", "Dry Peas", "Processing Beans", "Dry Beans", "Onions", "Cranberries", "Corn", "Oats", "Alfalfa Seed", "Fresh Apricots", "Fresh Freestone Peaches", "Nursery (GF&C)", "Fresh Nectarines", "Mustard", "Blueberries", "Adjusted Gross Revenue-Lite", "Plums", "Soybeans", "Whole Farm Revenue Protection", "Buckwheat")),
     #uiOutput("damagecontrols"),
     #uiOutput("countycontrols"),
     #selectInput("climate", label = "Climate Variables (choose one)", choice = c("pr", "tmmx", "tmmn", "rmax", "rmin", "psdi", "fm100", "fm1000", "pet", "th", "srad", "sph", "vs", "erc"), selected = "pr")
     
#radioButtons("var", label = "Statistical Summary Method:", choices = c("mean", "cov"), inline = TRUE, selected="mean"),

     # checkboxGroupInput("palousecounties_ID", label = "Idaho Counties:", choices = c("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai"), inline = TRUE),

#checkboxGroupInput("palousecounties_OR", label = "Oregon Counties:", choices = c("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin"), inline = TRUE),

#checkboxGroupInput("palousecounties_WA", label = "Washington Counties:", choices = c("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa"), inline = TRUE),







     checkboxGroupInput("climate", label = "Climate variables:", choices = c("Precipitation" = "pr", "Max Temp" = "tmmx", "Min Temp" = "tmmn", "Relative Min Humidity" = "rmin", "Relative Max Humidity" = "rmax", "Palmer Drought Index" = "pdsi", "100 Hr Burn Index" = "fm100", "1000 Hr Burn Index" = "fm1000", "Potential ET" = "pet"), inline= TRUE, selected=c("pr", "pdsi")),
     selectInput("predictor", label = "Predictor Variable:", choice = c("Crop Loss ($)" = "loss", "Crop Claim Frequency" = "countratio"), selected = "loss")


     #actionButton("myLoader", "Go!")    
     #downloadButton("report", "Generate report") 
    , width=3),    
     # Show the caption, a summary of the dataset and an HTML 
	 # table with the requested number of observations

	 mainPanel(
         #tags$style(type="text/css",
          #".shiny-output-error { visibility: hidden; }",
          #".shiny-output-error:before { visibility: hidden; }"),
    

    
                                      tabPanelAbout(), 





 
		tabsetPanel(


	tabPanel("Pairwise",
                                fluidRow(
                                        plotOutput("plotregression", height = 400),
                                                                      dataTableOutput("plot8f"))),


           
			    tabPanel("Decision Tree",
                                fluidRow(
                                        plotOutput("plot7dd", height = 400),
                                       dataTableOutput("plot8g"))),

			    tabPanel("Regression",
        			                  fluidRow(plotOutput("plot5aa"),
					                              verbatimTextOutput("plot5ab"))),
			    tabPanel("Treemap",
			             fluidRow(plotOutput("plot5xx"),
			                      dataTableOutput("plot8x"))),
			    tabPanel("Neuralnet",
			             fluidRow(plotOutput("plot5nn"),
			                      plotOutput("plot5nn2"))))
			    
			    

) #main panel
))
)
