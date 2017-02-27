function(){
	tabPanel("About",
		 HTML('
<br></br>
                <div style="clear: left;"><img src="https://s.gravatar.com/avatar/450f85ccf35a87cf1806fe3330af4877?s=80" alt="" style="float: left; margin-right:5px" /></div>
    <p>Erich Seamon<br/>
                PhD Student | University of Idaho<br/>
                <a href="http://erich.io" target="_blank">University page</a> |
                <a href="http://dmine.io" target="_blank">DMINE.io server</a> |
                <a href="https://pnwcirc.org" target="_blank">NOAA grant team - CIRC</a> |
                <a href="http://github.com/erichseamon", target="_blank">github</a>
                </p>'),


		HTML('
<h5><strong>About the Crop Loss Dashboard </strong></h5>		

<p style="text-align:justify">The DMINE commodity predictive model provides impact scores for a location, based on optimized models that are run on a regular basis.  The models contain meteorological and climatological data that are used over extended periods of time to predict crop commodity loss outcomes.    Over time we optimize our models to attempt to get better predictions.

Insurance claims of agricultural commodity loss are quite spatially and temporally extensive.  The USDA summarizes claims at a county and monthly level, and provides data going back to 1980.</p>


<p style="text-align:justify">Methodology.  Each of the steps performed to assembly, transform, organize, and analyze this grouping of data can be seen on the Ag Dashboard Methodology page.  We have developed a set of R and Ipython notebooks that describe the steps and data used, for open science reproducibility.</p>

<p style="text-align:justify">Datasets.  The datasets that are used for this analysis dashboard can viewed here.</p>


		<p style="text-align:justify">This project is supported by NOAA thru the Climate Impacts Research Consortium, or CIRC (www.pnwcirc.org)</p>

		<p style="text-align:justify">Also, thanks to Matthew Leonawicz and the SNAP team in Alaska, for the code and structure for these applications.</p>'),

		fluidRow(
			column(4,
				strong('Other Climatic Process Models'),
				p(HTML('<ul>'),
					HTML('<li>'),a("Random Forest Climatic Process Modeling", href="http://dmine.io:3838/random_forest_example/", target="_blank"),HTML('</li>'),
					HTML('<li>'),a("Neural Network Modeling", href="http://shiny.snap.uaf.edu/RV_distributionsV2/", target="_blank"),HTML('</li>'),
				HTML('</ul>')),
				br()
			),
			column(4,
				strong('Relevant research team sites'),
				p(HTML('<ul>'),
					HTML('<li>'),a("Climate Impacts Research Consortium (CIRC)", href="http://pnwcirc.org/", target="_blank"),HTML('</li>'),
					HTML('<li>'),a("Regional Approaches to Climate Change (REACCH)", href="http://reacchpna.org/", target="_blank"),HTML('</li>'),
				HTML('</ul>')),
				br()
			),
			column(4,
				strong('Other Resources'),
				p(HTML('<ul>'),
					HTML('<li>'),a('Coded in R', href="http://www.r-project.org/", target="_blank"),HTML('</li>'),
					HTML('<li>'),a('Built with the Shiny', href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
					HTML('<li>'),"Primary supporting R packages",HTML('</li>'),
					HTML('<ul>'),
						HTML('<li>'),a('gbm', href="http://cran.r-project.org/web/packages/gbm/index.html", target="_blank"),HTML('</li>'),
						HTML('<li>'),a('ggplot2', href="http://ggplot2.org", target="_blank"),HTML('</li>'),
						HTML('<li>'),a('plyr', href="http://cran.r-project.org/web/packages/plyr/index.html", target="_blank"),HTML('</li>'),
						HTML('<li>'),a('reshape2', href="http://cran.r-project.org/web/packages/reshape2/index.html", target="_blank"),HTML('</li>'),
					HTML('<ul>'),
				HTML('</ul>'))
			)
		),
		value="about"
	)
}
