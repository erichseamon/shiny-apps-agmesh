function(){


#conditionalPanel(condition="input.conditionedPanels==1",

        tabPanel("Help", id="conditionedPanels",
                 HTML('
<h4><p><strong>National Ag Stats Service Analysis</strong><br/></h4>

                </p>'),



		HTML('
		
<p style="text-align:justify">The National Agricultural Statistics Service (NASS) provides survey and census data with regards to agricultural commodities.  In this data review, we examine differing commodity totals (production, area harvested, sales, yield). </p>'),


		value="about"
	)
}
