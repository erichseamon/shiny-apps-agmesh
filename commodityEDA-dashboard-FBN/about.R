function(){
        tabPanel("Help",
                 HTML('
<h3><p><strong>DMINE Agricultural Prediction Dashboard: Palouse Wheat and Drought</strong><br/></h3>

                </p>'),


		HTML('
		
<p style="text-align:justify">The Palouse Agricultural Prediction Dashboard gives a general overview of refined crop insurance loss datasets that have been optimally correlated with climate data.  Provided are pairwise correlation results, regression analysis, as well as some other predictive modeling techniques (decision tree, neural networks).  These analytics are operating on a pre-constructed dataset of insurance claim records, summarized by county and year, for the palouse region of Idaho, Washington, and Oregon - from 1989-2015.  Climate data were associated with each summarized record, using a algorithm to match up previous climate data with each record. </p>'),


		value="about"
	)
}
