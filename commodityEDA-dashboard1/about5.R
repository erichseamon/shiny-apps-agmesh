function(){

conditionalPanel(condition="input.conditionedPanels==8",



        tabPanel("Help", value = 2, id="conditionedPanels",
                 HTML('
<h4><p><strong>Step 5: Agricultural Commodity Animation</strong><br/></4h>

                </p>')),



		HTML('
		
<p style="text-align:justify">In order to visually examine all commodities and damage causes over time, we have animated commodities vs. damage causes for the time period of 1989-2015, for Washington, Oregon, and Idaho. Each animation has a map of loss, as well as the total claim counts and loss in $. 

</p>'),


		value="about"
	)
}
