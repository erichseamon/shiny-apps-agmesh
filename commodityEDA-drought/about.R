

function(){
        tabPanel("Help",
                 HTML('
<h3><p><strong>DMINE Agriculture Dashboard: Insurance Crop Claim County Frequency</strong><br/></h3>
<a href="https://dmine.io/ag-commodity-loss-dashboard/", target="_parent">Claim Loss</a> |
                <a href="https://dmine.io/ag-commodity-loss-dashboard-drought/", style="text-decoration:underline;",  target="_parent">Claim County Frequency</a> |
                <a href="https://dmine.io/ag-commodity-dashboard-damage-counts/" target="_parent">Claim State Frequency</a> |

<a href="https://dmine.io/ag-commodity-loss-dashboard-animation/", target="_parent">Claim Animation</a> |
<a href="https://dmine.io/ag-dashboard-palouse-crop-claims-vs-climate/" target="_parent">Loss vs Climate</a> |
<a href="https://dmine.io/ag-dashboard-regression-and-model-analysis/" target="_parent">Prediction</a> |

                </p>'),


		HTML('
		
<p style="text-align:justify">
The insurance crop claim state frequency dashboard provides a view of claim data by monthly frequency, by monthly loss, and by annual county loss.   
These analytics are operating on a set of insurance claim records from 2001-2015, summarized by county and year, for three-state region of Idaho, Washington, and Oregon. </p>'),


		value="about"
	)
}
