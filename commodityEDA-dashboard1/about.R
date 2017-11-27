function(){

conditionalPanel(condition="input.conditionedPanels==1",



        tabPanel("Help", value = 2, id="conditionedPanels",
                 HTML('
<h3><p><strong>DMINE Agriculture Dashboard: Insurance Crop Claim State Frequency</strong><br/></h3>

                </p>')),



		HTML('
		
<p style="text-align:justify">The Regression and Modeling Analysis Dashboard gives a general overview of a dataset, with pairwise correlation results, regression analysis, as well as some other predictive modeling techniques (decision tree, neural networks).  These analytics are operating on a pre-constructed dataset of insurance claim records, summarized by county and year, for the palouse region of Idaho, Washington, and Oregon - from 2007-2015.  Climate data were associated with each summarized record, using a algorithm to match up previous climate data with each record.  For more info on this methodology, please see our DMINE methods page. </p>'),


		value="about"
	)
}
