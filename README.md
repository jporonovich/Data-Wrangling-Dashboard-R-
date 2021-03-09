# Data Wrangling  & Interactive Dashboard  **'R'**
Last updated: March, 2021 <br />
By: Jordan 

Description:<br />
*Consolidate and clean Alcohol, Tobacco, GDP and CPI data into files into three separate CSV files (Monthly, Quarterly, Yearly). Merge .shp file with Alcohol sales per capita csv file and finally Developed dashboard for visualization & analysis*

*Current Progress. Heatmap and drop down designed. Next step, link user input and heatmap.*

![Dashboard visualization](https://raw.githubusercontent.com/jporonovich/R_-_DataWrangling_Dashboard-Shiny/main/Dashboard(Work-In%20Progress).PNG)


* [x] Consolidate files (Monthly, Quarterly, Annual) 
  * [x] AlcoholSales(Clean).csv
  * [x] Canada_CPI(Clean).csv
  * [x] Canada_GDP(Clean).csv
  * [x] TobaccoSales(Clean).csv

* [x] Merge Shape file(.shp) and Alcohol per capita  
  * [x] gpr_000b11a_e.shp
  * [x] consolidatedAlcoholPercapita.csv

* [ ] Interactive Dashboard.
  * [x] Line chart Percentage increase/decrease
  * [ ] Heat map of canada provincial alcohol sales _(Work in progess)_ 
  * [ ] Interactive Table

### Snippet from Dashboard.R
*Full file available in repository*
<details>
  <Summary> Click here. </Summary>
 
 ``` r
           #dynamic Line chart 
           ggplot(ConsolidatedAnnual, aes(x = Year)) +
           geom_line(aes(y = if (is.na(match("Dollar Value",input$SourceData))) {GDP.Prct.Chg} else {GDP}), 
                 col = if (is.na(match("GDP",input$ThreeMetrics))) {NA} else {"#0e7bcf"}, 
                 na.rm=TRUE, size = 1.15 )+ # Adding GDP % Change
           geom_line(aes(y = if (is.na(match("Dollar Value",input$SourceData))) {Alcohol.Prct.Chg} else {Alcohol.Sales.CAD}),
                 col = if (is.na(match("Alcohol",input$ThreeMetrics))) {NA} else {"#de9307"},
                 na.rm=TRUE, size = 1.15) + # Adding Alcohol % Change
           geom_line(aes(y = if (is.na(match("Dollar Value",input$SourceData))) {Tobacco.Prct.Chg} else {Tobacco.Sale.CAD}),
                 col = if (is.na(match("Tobacco",input$ThreeMetrics))) {NA} else {"#08a65c"},
                 na.rm=TRUE, size = 1.15) + # Adding tobacco % Change
           ylim(t = if (is.na(match("Dollar Value",input$SourceData))) {c(-20,20)} else {c(0,25000000)}) + #setting y range
           xlim(2000,2020)+
           xlab("Year") + #renaming x axis
           ylab(if (is.na(match("Dollar Value",input$SourceData))) {"Percentage Change(%)"} else {"Dollar Value CAD"})+ #renaming y axis
           ggtitle("Annual Expenditures & Growth/Decline")+
           theme(
             panel.background = element_blank(),
             panel.grid = element_blank(),
             #panel.grid.major.x = element_line(color = "gray50", size = 0.05),
             panel.grid.major = element_line(color = "gray50", size = 0.05),
             plot.title = element_text(size = 14, face = "bold.italic", color = "#0c73c2")

```

</details>
