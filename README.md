# Data Wrangling  & Interactive Dashboard  **'R'**
Last updated: March, 2021 <br />
By: Jordan 

Description:<br />
*Consolidate and clean Alcohol, Tobacco, GDP and CPI data into files into three separate CSV files (Monthly, Quarterly, Yearly). Merge .shp file with Alcohol sales per capita csv file and finally Developed dashboard for visualization & analysis*

*Current Progress*

![Dashboard visualization]()


* [x] Consolidate files (Monthly, Quarterly, Annual) 
  * [x] AlcoholSales(Clean).csv
  * [x] Canada_CPI(Clean).csv
  * [x] Canada_GDP(Clean).csv
  * [x] TobaccoSales(Clean).csv

* [x] Merge Shape file(.shp) and Alcolhol per capita  
  * [x] gpr_000b11a_e.shp
  * [x] consolidatedAlcoholPercapita.csv

* [ ] Interactive Dashboard.
  * [x] Line chart Percentage increase/decrease
  * [ ] Heat map of canada provincial alcohol sales _(Work in progess)_ 
  * [ ] Interactive Table


<details>
  <Summary> Snippet from Consolidating.R </Summary>
    ```R:
        
        #Creating a dataframe that will hold monthly sales and GDP information. 

        #Empty data frame
        ConsolidatedMonthly = data.frame() 
        
        #Translating Horizontal Alcohol sale datato vertical
        for (i in 1:length(colnames(AlcoholSales))){
          ConsolidatedMonthly[i,1] = colnames(AlcoholSales)[i] # Whole date`` 
          ConsolidatedMonthly[i,2] = "" # Leaving blank for Year & Quarter (See 3rd line down)
          ConsolidatedMonthly[i,3] = as.numeric(substr(colnames(AlcoholSales)[i],1,4)) # Year 
          ConsolidatedMonthly[i,4] = paste0("Q",ceiling(as.numeric(substr(colnames(AlcoholSales)[i],5,6))/3)) # Quarter
          ConsolidatedMonthly[i,2] = paste0(ConsolidatedMonthly[i,4],".",ConsolidatedMonthly[i,3]) # Year & Quarter
          ConsolidatedMonthly[i,5] = as.numeric(AlcoholSales[1,i]) # sales
        }

        #Alcohol SALE  Percentage Change
        for (i in 1:(length(row.names(ConsolidatedMonthly))-1)){
          ConsolidatedMonthly[(i+1),6] = round(as.numeric(((ConsolidatedMonthly[(i+1),5]/ConsolidatedMonthly[i,5])-1)*100),2)
        }
    ```

</details>
