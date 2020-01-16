function(){

conditionalPanel(condition="input.conditionedPanels==1",



        tabPanel("Help", value = 1, id="conditionedPanels",
                 HTML('
<h4><p><strong>State Level Insurance Commodity Loss Exploratory Data Analysis</strong><br/></h4>

                </p>')),



		HTML('
		
<p style="text-align:justify">The state level exploratory data analysis section provides access to crop loss and county damage levels for the three state region of Washington, Oregon, and Idaho, from 1989 - 2015.  The loss data has been adjusted for inflation using 2015 consumer price indexing for agricultural commodities. </p>'),


		value="about"
	)
}
