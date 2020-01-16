
library(shinythemes)

tabPanelAbout <- source("about.R")$value
tabPanelAbout1 <- source("about1.R")$value
tabPanelAbout2 <- source("about2.R")$value
tabPanelAbout3 <- source("about3.R")$value
tabPanelAbout4 <- source("about4.R")$value
tabPanelAbout5 <- source("about5.R")$value
tabPanelAbout6 <- source("about6.R")$value
tabPanelAbout7 <- source("about7.R")$value
tabPanelAbout8 <- source("about8.R")$value


tabPanelMethods1 <- source("methods1.R")$value
tabPanelMethods1a <- source("methods1a.R")$value
tabPanelMethods1b <- source("methods1b.R")$value
tabPanelMethods1c <- source("methods1c.R")$value
tabPanelMethods2 <- source("methods2.R")$value
tabPanelMethods3 <- source("methods3.R")$value
tabPanelMethods4 <- source("methods4.R")$value
tabPanelMethods5 <- source("methods5.R")$value
tabPanelMethods6 <- source("methods6.R")$value
tabPanelMethods7 <- source("methods7.R")$value
tabPanelMethods8 <- source("methods8.R")$value

tabPanelMethods <- source("methods.R")$value 
tabPanelOverview <- source("overview.R")$value





shinyUI(fluidPage(
   tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),




  
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


conditionalPanel(condition="input.conditionedPanels==5",

selectInput("state5", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

#selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

 selectInput("year5", label = "Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2001")

),



 
conditionalPanel(condition="input.conditionedPanels==4",

tags$style(".well {background-color:#71acd7;}"),


 tags$div(class="h4", checked=NA, style="text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "Agriculture Knowledge Discovery Dashboard")
     ),


 tags$div(class="h5", checked=NA, style="color:darkblue; text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "Step 2: Data Assembly")
),
    p("Choose a state and a year to look at USDA claims records")


#selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

#selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

# selectInput("year", label = "Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2001")

#selectInput("month", "month of interest.  Use a number (1-12)", choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

#checkboxGroupInput("vari", label = "Climate variables:", choices = c("Precipitation" = "PR", "Max Temp" = "TMMX", "Min Temp" = "TMMN", "Relative Min Humidity" = "RMIN", "Relative Max Humidity" = "RMAX", "Palmer Drought Index" = "PDSI", "100 Hr Burn Index" = "FM100", "1000 Hr Burn Index" = "FM1000", "Potential ET" = "PET", "Energy Release Component" = "ERC", "Daily Specific Humidity" = "SPH", "Solar Radiation" = "SRAD", "Wind Direction" = "TH", "Wind Speed" = "VS"), inline= TRUE, selected=c("PR"))

#checkboxGroupInput("vari2", label = "Commodity variables:", choices = c("Year" = "year", "Month" = "month", "County FIPS" = "countyfips", "statecode" = "statecode", "state" = "state", "countycode" = "countycode", "County" = "county", "commoditycode" = "commoditycode", "Commodity" = "commodity", "Insurance plan code" = "insuranceplancode", "insurancename" = "insurancename", "stagecode" = "stagecode", "damagecausecode" = "damagecausecode", "damagecause" = "damagecause", "monthcode" = "monthcode", "acres" = "acres", "loss" = "loss"), inline= TRUE, selected=c("year", "month", "countyfips", "statecode"))

),

conditionalPanel(condition="input.conditionedPanels==3",

 tags$div(class="h4", checked=NA, style="text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "Agriculture Knowledge Discovery Dashboard")
     ),
tags$p(),
 tags$div(class="h5", checked=NA, style="color:darkblue; text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "DMINE is funded thru NOAA grant # NA15OAR4310145")
),


     tags$a(div(img(src='ui.png', width =100), style="text-align: center;"), href="https://dmine.io"),
tags$p(),
tags$p(),
tags$a(div(img(src='osu.png', width =100), style="text-align: center;"), href="https://dmine.io"),
tags$p(),
tags$p(),
tags$a(div(img(src='noaa.png', width =100), style="text-align: center;"), href="https://dmine.io"),

selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

#selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

 selectInput("year", label = "Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2001"),

actionButton("submit3", "Submit")




),

conditionalPanel(condition="input.conditionedPanels==1",

     #tags$a(div(img(src='dmine-commodity-Dash-stateloss.png', width =150), style="text-align: center;"), href="https://dmine.io"),
     #img(src='dmine-commodity.png', width = 100, align = "center"),
     #h4("Ag Commodity Loss DAMAGE Dashboard", align = "center"),

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


 tags$div(class="h4", checked=NA, style="text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "Agriculture Knowledge Discovery Dashboard")
     ),


 tags$div(class="h5", checked=NA, style="color:darkblue; text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "State Insurance Loss")
),

     uiOutput("commoditycontrols"),

     selectInput("state2", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

#dateRangeInput('dateRange',
#      label = 'Date range input: yyyy',
#      start = "2001", end = "2015"
#    ),

 selectInput("year2", label = "Year", choice = c("1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2004"),


selectInput("month", "Month of interest", choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
#     selectInput("commodity", "Choose a commodity:", 
#                  choices = c("Apples", "Wheat", "Barley", "Sugar Beets", "Cherries", "Grapes", "Adjusted Gross Revenue", "Green Peas", "All Other Crops", "Pears", "Canola", "Sweet Corn", "Mint", "Potatoes", "Dry Peas", "Processing Beans", "Dry Beans", "Onions", "Cranberries", "Corn", "Oats", "Alfalfa Seed", "Fresh Apricots", "Fresh Freestone Peaches", "Nursery (GF&C)", "Fresh Nectarines", "Mustard", "Blueberries", "Adjusted Gross Revenue-Lite", "Plums", "Soybeans", "Whole Farm Revenue Protection", "Buckwheat")),
     
     #uiOutput("countycontrols"),
     #uiOutput("damagecontrols"), 

br(),
 tags$div(class="h5", checked=NA, style="color:darkblue",
           tags$b(href="https://dmine.io/climate-dashboards/", "Claim Distribution")),

selectInput("startyear", label = "Start Year", choice = c("1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2004"), 


     selectInput("endyear", label = "End Year", choice = c("1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2015"), 

     actionButton("submit", "Submit")    
     #downloadButton("report", "Generate report") 
        
     # Show the caption, a summary of the dataset and an HTML 
	 # table with the requested number of observations
),


conditionalPanel(condition="input.conditionedPanels==12",
 tags$div(class="h4", checked=NA, style="text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "Agriculture Knowledge Discovery Dashboard")
     ),

 tags$div(class="h5", checked=NA, style="color:darkblue; text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "Univariate Climate Matrix ")
),

selectInput("state12", label = "State", choice = c("WA", "ID", "OR"), selected="WA"),

uiOutput("county12controls"),

selectInput("commodity12", label = "Commodity", choice = c("WHEAT"), selected="WHEAT"),

selectInput("damage12", label = "Damage Cause", choice = c("Drought"), selected="Drought"),


checkboxGroupInput("climate_variable12", label = "Climate variables:", choices = c("Precipitation" = "pr", "Max Temp" = "tmmx", "Min Temp" = "tmmn", "Relative Min Humidity" = "rmin", "Relative Max Humidity" = "rmax", "Palmer Drought Index" = "pdsi", "100 Hr Burn Index" = "fm100", "1000 Hr Burn Index" = "fm1000", "Potential ET" = "pet"), inline= TRUE, selected=c("pr")),
     selectInput("predictor12", label = "Predictor Variable:", choice = c("Crop Loss ($)" = "loss", "Loss per Acre" = "loss_per_acre", "Acres Loss" = "acres_loss", "Cube/Acres" = "cube_root_acres", "Cube root/loss" = "cube_root_loss"), selected = "loss"),


selectInput("monthmatrix_end", label = "Ending Month", choice = tolower(month.abb), multiple = FALSE),

selectInput("monthmatrix_number", label = "Number of months", choice = c(1:12), multiple = FALSE),

radioButtons(inputId="saveit", label="Save matrix:", 
 choices=c("save","don't save")),

actionButton("submit2","Submit")

),










conditionalPanel(condition="input.conditionedPanels==6",
 tags$div(class="h4", checked=NA, style="text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "Agriculture Knowledge Discovery Dashboard")
     ),

 tags$div(class="h5", checked=NA, style="color:darkblue; text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "Step 4: Model ")
),

checkboxGroupInput("climate", label = "Climate variables:", choices = c("Precipitation" = "pr", "Max Temp" = "tmmx", "Min Temp" = "tmmn", "Relative Min Humidity" = "rmin", "Relative Max Humidity" = "rmax", "Palmer Drought Index" = "pdsi", "100 Hr Burn Index" = "fm100", "1000 Hr Burn Index" = "fm1000", "Potential ET" = "pet"), inline= TRUE, selected=c("pr", "pdsi")),
     selectInput("predictor", label = "Predictor Variable:", choice = c("Crop Loss ($)" = "loss", "Crop Claim Frequency" = "countratio"), selected = "loss"),

uiOutput("damagecontrols"),

selectInput("year3", label = "Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2004")


),


conditionalPanel(condition="input.conditionedPanels==8",
 tags$div(class="h4", checked=NA, style="text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "Agriculture Knowledge Discovery Dashboard")
     ),

 tags$div(class="h5", checked=NA, style="color:darkblue; text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "Step 5 : Animation ")
),


selectInput("state8", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

uiOutput("commodity8controls"),



#checkboxGroupInput("climate", label = "Climate variables:", choices = c("Precipitation" = "pr", "Max Temp" = "tmmx", "Min Temp" = "tmmn", "Relative Min Humidity" = "rmin", "Relative Max Humidity" = "rmax", "Palmer Drought Index" = "pdsi", "100 Hr Burn Index" = "fm100", "1000 Hr Burn Index" = "fm1000", "Potential ET" = "pet"), inline= TRUE, selected=c("pr", "pdsi")),
#     selectInput("predictor", label = "Predictor Variable:", choice = c("Crop Loss ($)" = "loss", "Crop Claim Frequency" = "countratio"), selected = "loss"),

uiOutput("damage8controls")

#selectInput("year3", label = "Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2004")


),





conditionalPanel(condition="input.conditionedPanels==2",
 selectInput("endyear", label = "End Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2005")    

),



conditionalPanel(condition="input.conditionedPanels==10",
selectInput("state10", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

uiOutput("county10controls"),
uiOutput("statdesccontrols")
),





conditionalPanel(condition="input.conditionedPanels==7",

 tags$div(class="h4", checked=NA, style="text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "Agriculture Knowledge Discovery Dashboard")
     ),

 tags$div(class="h5", checked=NA, style="color:darkblue; text-align: center",
           tags$b(href="https://dmine.io/climate-dashboards/", "County Insurance Loss")
),






selectInput("state7", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

#selectInput("state", label = "State", choice = c("Washington", "Idaho", "Oregon"), selected="Idaho"),

 selectInput("year7", label = "Year", choice = c("1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected="2001"),

#selectInput("month7", "month of interest.  Use a number (1-12)", choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

uiOutput("commodity7controls"),

uiOutput("county7controls"),

uiOutput("damage7controls"),

actionButton("submit4", "Submit")




), 

width=2),



    mainPanel(


#         tags$style(type="text/css",
#          ".shiny-output-error { visibility: hidden; }",
#          ".shiny-output-error:before { visibility: hidden; }"),

	      tabsetPanel(id = "conditionedPanels",


		tabPanel("Overview", value=3,
                        tabPanelAbout1(),
                        tabsetPanel(
				tabPanelMethods1(),
                                tabPanelMethods1a(),
				#tabPanelMethods1b(),
				tabPanelMethods1c(),

                                tabPanel("USDA Commodity Insurance", value=2,
                                     fluidRow(column(6, dataTableOutput("USDA")),
                                             column(6, dataTableOutput("USDA2")))),

				#tabPanel("Agricultural Commodity Specifics", value=2,
                                #     fluidRow(plotOutput("plot7xz"))),

                                #tabPanel("Yearly Claims Distribution", value=2,
                                #     fluidRow(plotOutput("plot7zz"))),


                        id = "conditionedPanels")),



#		tabPanel("Assembly", value=4,  
#                        tabPanelAbout2(),
#                        tabsetPanel(
#				tabPanelMethods2(),
#                               # tabPanel("USDA Commodity Insurance", value=2,
#                               #      fluidRow(column(6, dataTableOutput("USDA")),
#			       #      column(6, dataTableOutput("USDA2")))), 
#				
#				tabPanel("Soil Moisture Observations", value=2,
#                                     fluidRow(dataTableOutput("TAMU"))),
#
#				tabPanel("GHCND", value=2,
#                                     fluidRow(plotOutput("ghcnd"))),
#
#
#                                #tabPanel("Yearly Claims Distribution", value=2,
#                                #     fluidRow(plotOutput("plot7zz"))),
#
#
#                        id = "conditionedPanels")),


 		 tabPanel("State Insurance Loss", value =1, 
                        tabPanelAbout3(),
			tabsetPanel(
				tabPanelMethods3(),
				tabPanel("Crop Loss/County", value=1,
                                     fluidRow(plotOutput("croploss11"),
					plotOutput("croploss12"))),

                                tabPanel("County Damage", value=5,

                                     fluidRow(plotOutput("lossdamage1"),
				     plotOutput("lossdamage2"))),

				tabPanel("Claims Distribution", value=2,
                                     fluidRow(plotOutput("plot7z"),
				     plotOutput("plot7zzz"))),

                                #tabPanel("Yearly Claims Distribution", value=2,
                                #     fluidRow(plotOutput("plot7zz"))),



		id = "conditionedPanels")),





		 tabPanel("County Insurance Loss", value=7, 
			tabPanelAbout4(),
                        tabsetPanel(   
				tabPanelMethods4(),
                 		tabPanel("Monthly County Counts", value=2, 
                                     fluidRow(plotOutput("plot7e"),
					column(6, dataTableOutput("plot8g4")))),

		 		tabPanel("Monthly County Loss", value=2,
                                     fluidRow(plotOutput("plot7c"),
		 		        column(6, dataTableOutput("plot8fff")))),

				tabPanel("Annual County Loss", value=2,
                                     fluidRow(plotOutput("plot7y3"),
                                        column(6, dataTableOutput("plot8annual")))),




                  	id = "conditionedPanels")),

	         tabPanel("Insurance Loss Animation", value=8,
			tabPanelAbout5(),
                        tabsetPanel(               
			        tabPanelMethods5(),
                                tabPanel("State Animation", 
 			             fluidRow(uiOutput("video"))),
                        id = "conditionedPanels")),


   tabPanel("NASS", value=10,tags$style(".main {background-color:[#99CCFF];}"),
                        tabPanelAbout7(),
                        tabsetPanel(
                        tabPanelMethods7(),
                        tabPanel("NASS Survey Stats",
                        fluidRow(plotOutput("NASS"))),
                        id = "conditionedPanels")),


  tabPanel("Climate Impacts", value=12,
                        tabPanelAbout8(),
                        tabsetPanel(
                        tabPanelMethods8(),
                        tabPanel("Climate Impacts",
                        fluidRow(plotOutput("climmatrix"),


                        tags$style(HTML("#climmatrixtable3{
                                   text-align: left; font-size: 18px; font-weight: bold;
                                                        }")),

			tags$style(HTML("#climmatrixtable4{
                                   text-align: left; font-size: 18px; font-weight: bold;
                                                        }")),


                        splitLayout(cellWidths = c("50%", "50%"), htmlOutput("climmatrixtable3"), htmlOutput("climmatrixtable4")),
                   br(),
                   br(), 
                        column(6, dataTableOutput("climmatrixtable")),
                        splitLayout(cellHeights = c("50%", "50%"), column(6, plotOutput("climatematrixmap"), plotOutput("climmatrixgraph"))))),
			#tabPanel("Climate Impacts Map",
                        #fluidRow(plotOutput("climatematrixmap"))),

                        #column(4, plotOutput("climatematrixmap")))),
                        id = "conditionedPanels")),









		 tabPanel("Model", value=6,
                        tabPanelAbout6(),
                        tabsetPanel(
			tabPanelMethods6(),
  			tabPanel("Pairwise Correlations", 
                        fluidRow(plotOutput("plotregression"),
			dataTableOutput("plotpairtable"))),

                        id = "conditionedPanels"))




)






       
    , width = 10) #main panel

))
)
