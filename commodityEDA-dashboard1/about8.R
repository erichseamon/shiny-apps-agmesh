function(){


#conditionalPanel(condition="input.conditionedPanels==1",

        tabPanel("Help", id="conditionedPanels",
                 HTML('
<h4><p><strong>Climate Impact Matrix</strong><br/></h4>

                </p>'),



		HTML('
		
<p style="text-align:justify">This analysis compares agricultural commodity loss, in varying forms, to climate - in an aggregated manner - using a matrix of monthly climate variables for comparison. </p>'),


		value="about"
	)
}
