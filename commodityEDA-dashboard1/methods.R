function(){
	tabPanel("Help",
		 HTML('
<br></br>
                <div style="clear: left;"><img src="https://s.gravatar.com/avatar/450f85ccf35a87cf1806fe3330af4877?s=80" alt="" style="float: left; margin-right:5px" /></div>
    <p>Erich Seamon<br/>
                PhD Student | University of Idaho<br/>
                <a href="http://erich.io" target="_blank">University page</a> |
                <a href="http://dmine.io" target="_blank">DMINE.io server</a> |
                <a href="https://pnwcirc.org" target="_blank">NOAA team - CIRC</a> |
                <a href="http://github.com/erichseamon", target="_blank">github</a>
                </p>'),


		HTML('
<h5><strong>About the Crop Loss Dashboard </strong></h5>

<p style="text-align:justify">The DMINE commodity predictive model provides impact scores for a location, based on optimized models that are run on a regular basis.  The models contain meteorological and climatological data that are used over extended periods of time to predict crop commodity loss outcomes.    Over time we optimize our models to attempt to get better predictions.

Insurance claims of agricultural commodity loss are quite spatially and temporally extensive.  The USDA summarizes claims at a county and monthly level, and provides data going back to 1980.</p>


<p style="text-align:justify">Methodology.  Each of the steps performed to assembly, transform, organize, and analyze this grouping of data can be seen on the Ag Dashboard Methodology page.  We have developed a set of R and Ipython notebooks that describe the steps and data used, for open science reproducibility.</p>

<p style="text-align:justify">Datasets.  The datasets that are used for this analysis dashboard can viewed here.</p>

<h5><strong>Crop Loss Analysis Methodology </strong></h5>
Economic crop loss has a close relationship to food resilience and security.  Under this premise, we have been developing a case scenario example of data mining and machine learning - that results in a predictive climate impact score for a location or region.  The result will be a probability map indicating the crop loss value predicted at a location, as well as the probability value as to the accuracy of that prediction.
<p></p>
In order to create our model, a set of steps need to be undertaken to:
<p style="text-align: left;">1) Assemble and inventory data,</p>
<p style="text-align: left;">2) transform and explore the data, and</p>
<p style="text-align: left;">3) model and predict agricultural crop loss outcomes.</p>

<hr />
<p></p>
<h4><strong>Methodology step thru procedures</strong></h4>
The following is a set of scientific programming notebooks that walk thru each of the steps to enable this final goal.

&nbsp;

<p></p>
<em><strong></strong></em><strong>DMINE Ag Resilience - Part 1: Crop Loss Overview:</strong>  Here we provide a general<strong> </strong>overview of the example problem (can we use machine learning and data mining techniques to predict eonomic crop loss - using climatic variables?), and the process/steps to be completed.<em><em><em>      </em></em></em><a href="http://dmine.io/DMINE-circ-ag-overview-part1.html" target="_blank">HTML IPython Notebook</a>

<hr />
<p></p>

<em><strong></strong></em><strong>DMINE Ag Resilience - Part 2: Data Transformation:</strong> The data transformation step mines data from a location, and organizes it matrix-style format, ready foranalysis purposes.     <a href="http://dmine.io/dmine-circ-ag-data-transformation-part2.html" target="_blank">HTML RMarkdown Notebook</a>

<hr />
<p></p>

<em><em><em><strong></strong></em></em></em><strong>DMINE Ag Resilience - Part 3: Exploratory Data Analysis:</strong>  After extracting and transforming the data, as well as getting it into a feature/response structure, we explore the data to see any initial relationships that may be important.  We tested for collinearity, heteroskedasticity, autocorrelation, and perform linear regression fitting.    <a href="http://dmine.io/wp-content/uploads/2016/06/dmine-circ-ag-eda-part3.pdf" target="_blank">HTML IPython Notebook</a>    |   Interactive Python Notebook

<hr />
<p></p>

<strong><strong><em></em></strong>DMINE Ag Resilience - Part 4: Data Modeling:</strong>  In the data
modeling portion of the example, we create, run, and optimize several models (decision tree, random forest) in an effort to find the best overall model that will work for our predictive efforts in the Pacific Northwest.  <a href="http://dmine.io/dmine-circ-ag-modeling-part4.html" target="_blank">HTML IPython Notebook</a>     |     <a href="http://reacchapp.nkn.uidaho.edu:8888/notebooks/dmine/circ/dmine-circ-ag-modeling-part4.ipynb" target="_blank">Interactive Python Notebook</a>

<hr />
<p></p>

<strong><strong><em>></em></strong>DMINE Ag Resilience - Part 5: Creating an API:</strong>  In this portion of our example, we use python and flask to generate an json-based API that uses a pickled optimized version of the model that comes from STEP 4.  HTML Python Notebook     |    Interactive Python Notebook

<hr />
<p></p>

<em><strong></strong></em><strong>DMINE Ag Resilience - Part 6: BETA Crop Loss Tool that uses developed API:</strong> In the final portion of our example, we use the crop loss API url that was created in STEP 5, to create a simple html application for submitting information, and getting a predicted response with a map.     HTML Python Notebook | Interactive Python Notebook
<hr />
<p></p>
                <p style="text-align:justify">This project is supported by NOAA thru the Climate Impacts Research Consortium, or CIRC (www.pnwcirc.org)</p>

                <p style="text-align:justify">Also, thanks to Matthew Leonawicz and the SNAP team in Alaska, for the initial base code and structure for these applications.</p>'),

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

