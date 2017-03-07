function(){
	tabPanel("Help",
		 HTML('
<h3><p><strong>DMINE Agriculture Dashboard: Insurance Crop Claim Loss</strong><br/></h3>
<a href="https://dmine.io/ag-commodity-loss-dashboard/", style="text-decoration:underline;", target="_parent">Claim Loss</a> |
                <a href="https://dmine.io/ag-commodity-loss-dashboard-drought/" target="_parent">Claim County Frequency</a> |
                <a href="https://dmine.io/ag-commodity-dashboard-damage-counts/" target="_parent">Claim State Frequency</a> |

<a href="https://dmine.io/ag-commodity-loss-dashboard-animation/", target="_parent">Claim Animation</a> |
<a href="https://dmine.io/ag-dashboard-palouse-crop-claims-vs-climate/" target="_parent">Loss vs Climate</a> |
<a href="https://dmine.io/ag-dashboard-regression-and-model-analysis/" target="_parent">Prediction</a> |

                </p>'),


		HTML('
		
<p style="text-align:justify">The insurance crop claim loss dashboard provides an overall view of agricultural commodity loss at a county level.  The data provided is from the USDAs risk management agency, which collects insurance claim information from companies in the United States.  This data has been transformed and organized to visualize by the county, time, commodity type, and damage cause. For more info on this methodology, please see our DMINE methods page. </p>'),


		value="about"
	)
}
