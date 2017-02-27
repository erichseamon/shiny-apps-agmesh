# Define UI for dataset viewer application
library(shinythemes)

tabPanelAbout <- source("/srv/shiny-server/commodityEDA-Idaho-2/about.R")$value
tabPanelMethods <- source("/srv/shiny-server/commodityEDA-Idaho-2/methods.R")$value        
shinyUI(fluidPage(
     
  # Application title
  #titlePanel("Agricultural Crop Analysis Dashboard"),
  
  # Sidebar with controls to provide a caption, select a dataset,
  # and specify the number of observations to view. Note that
  # changes made to the caption in the textInput control are
  # updated in the output area immediately as you type
  sidebarLayout(
    sidebarPanel(
     div(img(src='dmine-commodity.png', width = 150), style="text-align: center;"),
     #img(src='dmine-commodity.png', width = 100, align = "center"),
     h4("Agricultural Crop Loss Dashboard"),
     p("Instructions: Fill in your state, and the year you are interested in.  This will pre-populate other fields, such as availabile commodities."),
     p("After selecting a month and commodity, you can look at the results information in the horizontal tabs at the top."),
     selectInput("state", label = "State", choice = c("Washington", "Idaho"), selected = "Idaho"),
     selectInput("firstyear", label = "First Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected = "2002"),
     select

     selectInput("lastyear", label = "Last Year", choice = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), selected = "2003"),


    ),
    
 
    # Show the caption, a summary of the dataset and an HTML 
	 # table with the requested number of observations
	 mainPanel(
         #tags$style(type="text/css",
         # ".shiny-output-error { visibility: hidden; }",
         # ".shiny-output-error:before { visibility: hidden; }"),
    



		tabsetPanel(
 			tabPanel("Climate-Ag extract",
                                fluidRow(
					plotOutput("plot1", height =1000))),


 
  )
))
)
