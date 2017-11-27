function(){
	tabPanel("Overview",  
		 HTML('
<div style="clear: left;"><img src="https://dmine.io/wp-content/uploads/Screen-Shot-2017-07-13-at-12.16.37-PM.png" alt="" style="float: right; width: 40%; margin-right:5px" /></div>'),

		HTML('
<h5><strong>Data Assembly and Organization - Agricultural Commodity Systems </strong></h5>

<p style="text-align:justify">
Data acquisition, transformation, and integration will be an important step for the proposed model development. In Step 2, we begin the assembly process of all of our datasets for this agricultural analysis, including;
<p></p>
<ul>
<li>The USDA’s agricultural crop loss data archive (1980-2016). The USDA’s Risk Management Agency has insurance claim records associated with commodity crop loss from 1980 to 2016.  Specifically, we are using the cause of loss archive datasets, which are .csv files which summarize insurance claims by month and by county.  This data is available for the entire United States. <i class="fa fa-external-link" aria-hidden="true"></i></li>
<p></p>
<li>Downscaled gridded climate datasets from 1980-2016. There are several excellent resources that provide gridded meteorological data for both historical and future scenarios (Abatzoglou, 2010, Thorton et al, 2014).  With regards to this analysis, we will use GRIDMET data, in combination with other relevant datasets that might assist in understanding crop loss variability and spatial patterns. <i class="fa fa-external-link" aria-hidden="true"></i></p></li>
</ul>
<p></p>'),


                value=4
        )
}

