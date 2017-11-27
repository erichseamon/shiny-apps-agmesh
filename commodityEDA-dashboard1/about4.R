function(){

conditionalPanel(condition="input.conditionedPanels==7",



        tabPanel("Help", value = 2, id="conditionedPanels",
                 HTML('
<h4><p><strong>County Level Insurance Commodity Loss Exploratory Data Analysis </strong><br/></h4>

                </p>')),



		HTML('
		
<p style="text-align:justify">In this step we continue to explore agricultural data variations by looking at county-level claim loss and counts, as well as claim counts at an annual level.  The range of data is from 1989 - 2015 for the three state region of Washington, Oregon, and Idaho.
</p>'),


		value="about"
	)
}
