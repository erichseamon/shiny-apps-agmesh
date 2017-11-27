function(){

conditionalPanel(condition="input.conditionedPanels==1",



        tabPanel("Help", value = 2, id="conditionedPanels",
                 HTML('
<h3><p><strong>DMINE Agriculture Dashboard: Insurance Crop Claim State Frequency</strong><br/></h3>
##<a href="https://dmine.io/ag-commodity-loss-dashboard/", target="_parent">Claim Loss</a> |
##                <a href="https://dmine.io/ag-commodity-loss-dashboard-drought/" target="_parent">Claim County Frequency</a> |
##                <a href="https://dmine.io/ag-commodity-dashboard-damage-counts/", style="text-decoration:underline;",  target="_parent">Claim State Frequency</a> |

##<a href="https://dmine.io/ag-commodity-loss-dashboard-animation/", target="_parent">Claim Animation</a> |
##<a href="https://dmine.io/ag-dashboard-palouse-crop-claims-vs-climate/" target="_parent">Loss vs Climate</a> |
##<a href="https://dmine.io/ag-dashboard-regression-and-model-analysis/" target="_parent">Prediction</a> |

                </p>')),



		HTML('
		
<p style="text-align:justify">The Regression and Modeling Analysis Dashboard gives a general overview of a dataset, with pairwise correlation results, regression analysis, as well as some other predictive modeling techniques (decision tree, neural networks).  These analytics are operating on a pre-constructed dataset of insurance claim records, summarized by county and year, for the palouse region of Idaho, Washington, and Oregon - from 2007-2015.  Climate data were associated with each summarized record, using a algorithm to match up previous climate data with each record.  For more info on this methodology, please see our DMINE methods page. </p>'),


		value="about"
	)
}
