function(){

conditionalPanel(condition="input.conditionedPanels==3",



        tabPanel("Overview", value = 3, id="conditionedPanels",
                 HTML('
<h4><p><strong>Agricultural Commodity Loss Analysis - Overview and Data Assemby</strong><br/></h4>

                </p>')),



                HTML('

<p style="text-align:justify">
The Agriculture Knowledge Discovery Dashboard is a research-focused interactive application, that provides a step-by-step methodology for knowledge discovery and machine learning prediction for the area of agriculture. 
<p></p>
</p>'),


                value="about"
        )
}
