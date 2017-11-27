function(){

conditionalPanel(condition="input.conditionedPanels==4",



        tabPanel("Data Sources", value=4, id="conditionedPanels",
                 HTML('
<h4><p><strong>Step 2: Data Assembly and Organization</strong><br/></h4>

                </p>')),



                HTML('

<p style="text-align:justify">In Step 2, we assemble and organize a grouping of data that will be examined, including agricultural commodity loss from the USDA, as well as downscaled climate data.  This step allows for reviewing the structure and content of the included datasets.


</p>'),


                value="about"
        )
}
