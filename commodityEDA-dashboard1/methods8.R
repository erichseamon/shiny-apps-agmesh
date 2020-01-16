function(){
	tabPanel("Overview",
HTML('
                <div style="clear: left;"><img src="https://dmine.io/waf/agmesh-scenarios/Allstates/climatematrix_pngs/WA_Whitman_Drought_pr_designmatrix.png"; style="float: right; width: 35%; margin-right:20px" /></div>'),




		HTML('
<h5><strong>Comparing climate variables to agriculture, using a design matrix </strong></h5>

<p style="text-align:justify">In this animation analysis, 2.8 million commodity claims in Idaho, Oregon, and Washington - between 1989 and 2015 - have been assembled by their documented damage cause, commodity, month and year. Keep in mind that a farmer files a claim, and after verification by a crop agent, that claim is documented as caused by a particular damage category (e.g. drought, heat, freeze, hail, cold weather, declining prices failed irrigation supply, etc.)  This analysis compares agricultural commodity loss, in varying forms, to climate - in an aggregated fashion.  Each cell represents a correlations between a time period of ONE climate variable - which is then grouped based on an ending month, and a number of months preceding to include.  Each cells data is for a range of years, depending upon the data available.</p>'),   

                value="about"
        )
}

