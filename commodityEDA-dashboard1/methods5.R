function(){
	tabPanel("Overview",
HTML('
                <div style="clear: left;"><img src="https://dmine.io/wp-content/uploads/11.png"; style="float: right; width: 55%; margin-right:20px" /></div>'),




		HTML('
<h5><strong>Agricultural Commodty Animation, 1989 - 2015 </strong></h5>

<p style="text-align:justify">In this animation analysis, 2.8+ million commodity claims in Idaho, Oregon, and Washington - between 1989 and 2015 - have been assembled by their documented damage cause, commodity, month and year. Keep in mind that a farmer files a claim, and after verification by a crop agent, that claim is documented as caused by a particular damage category (e.g. drought, heat, freeze, hail, cold weather, declining prices failed irrigation supply, etc).  

The provided animation shows crop loss in $, as well as the commodity claim counts and the claim counts by county for each month.  If there were no claims for a particular commodity in a state, then no animation was generated and that commmodity will not be listed in the pulldown menu on the left.
</p>'),

                value="about"
        )
}

