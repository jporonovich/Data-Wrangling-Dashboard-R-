#Loading CSV in Dataframes
GDP = read.csv("Canada_GDP(Clean).csv", header = TRUE)
CPI = read.csv("Canada_CPI(Clean).csv",header = TRUE)
ImpPrice = read.csv("Implicit_price_indexes_GDP(Clean).csv", header = TRUE)
AlcoholSales = read.csv("AlcoholSales(Clean).csv", header = TRUE)
TobaccoSale = read.csv("TobaccoSales(Clean).csv", header = TRUE)

#----------------------------------------------------------------------------------------------
#Tobacco Sales file Adding"Year.Qtr" row
#Reminder: [Row, Column]

TobaccoSale[5,] = substr(colnames(TobaccoSale),2,9)
TobaccoSale[6,] = substr(TobaccoSale[5,],1,4)
TobaccoSale[7,] = paste0("Q",ceiling(as.numeric(substr(TobaccoSale[5,],5,6))/3))
TobaccoSale[8,] = paste0(TobaccoSale[7,],".",TobaccoSale[6,])

#Setting row names and removing first columns
row.names(TobaccoSale) = c("Production","Total.Sales","Opening.Inventory","Closing.Inventory","NumDate","Year","Qtr","Year.Qrt")
TobaccoSale[,1] = NULL


#----------------------------------------------------------------------------------------------
#Consolidating data in single monthly file


#Replaces removes x in dates and cleans row
colnames(AlcoholSales) = substr(colnames(AlcoholSales),2,9)
row.names(AlcoholSales) = AlcoholSales[,1]
AlcoholSales[,1] = NULL

#Making table vertical and creating year, quarter column
#Reminder: [Row, Column]
#Creating empty data frame
ConsolidatedMonthly = data.frame() 

# Warning messages are created this is because NAs are introduced 
for (i in 1:length(colnames(AlcoholSales))){
  ConsolidatedMonthly[i,1] = colnames(AlcoholSales)[i] # Whole date`` 
  ConsolidatedMonthly[i,2] = "" # Leaving blank for Year & Quarter (See 3rd line down)
  ConsolidatedMonthly[i,3] = as.numeric(substr(colnames(AlcoholSales)[i],1,4)) # Year 
  ConsolidatedMonthly[i,4] = paste0("Q",ceiling(as.numeric(substr(colnames(AlcoholSales)[i],5,6))/3)) # Quarter
  ConsolidatedMonthly[i,2] = paste0(ConsolidatedMonthly[i,4],".",ConsolidatedMonthly[i,3]) # Year & Quarter
  ConsolidatedMonthly[i,5] = as.numeric(AlcoholSales[1,i]) # sales
  
}

#adding % change in sales
for (i in 1:(length(row.names(ConsolidatedMonthly))-1)){
  ConsolidatedMonthly[(i+1),6] = round(as.numeric(((ConsolidatedMonthly[(i+1),5]/ConsolidatedMonthly[i,5])-1)*100),2)
}

#Alcohol SALE percentage change column
for (i in 1:(length(row.names(ConsolidatedMonthly))-1)){
  ConsolidatedMonthly[(i+1),6] = round(as.numeric(((ConsolidatedMonthly[(i+1),5]/ConsolidatedMonthly[i,5])-1)*100),2)
}

#TobbacoSale 
#if statement require to deal with Logical(0) error
#Returns Logical(0) --> as.numeric(TobaccoSale[2,match(ConsolidatedMonthly[i,2],TobaccoSale[8,])])
for (i in 1:252) {
  if (length(which(!is.na(as.numeric(TobaccoSale[2,match(ConsolidatedMonthly[i,2],TobaccoSale[8,])])))) == 0) {
    ConsolidatedMonthly[i,7] = NA
  } else {
    ConsolidatedMonthly[i,7] = as.numeric(TobaccoSale[2,match(ConsolidatedMonthly[i,2],TobaccoSale[8,])])
    
  }  
}

#Tobacco percentage change column
for (i in 1:(length(row.names(ConsolidatedMonthly))-1)){
  ConsolidatedMonthly[(i+1),8] = round(as.numeric(((ConsolidatedMonthly[(i+1),7]/ConsolidatedMonthly[i,7])-1)*100),2)
}


#Adding Annualized GDP to ConsolidatedMonthly file
#Warning as there is no match in GDP file for "Q4 2020" 
#for (i in 1:(length(row.names(ConsolidatedMonthly))-1)) {
# ConsolidatedMonthly[i,9] = as.numeric(GDP[1,match(ConsolidatedMonthly[i,2],colnames(GDP))])
#}

for (i in 1:(length(row.names(ConsolidatedMonthly))-1)) {
  if (length(which(!is.na(as.numeric(GDP[31,match(ConsolidatedMonthly[i,2],colnames(GDP))])))) == 0) {
    ConsolidatedMonthly[i,9] = NA
  } else {
    ConsolidatedMonthly[i,9] = as.numeric(GDP[31,match(ConsolidatedMonthly[i,2],colnames(GDP))])*10
    
  }  
}

#GDP percentage change column
for (i in 1:(length(row.names(ConsolidatedMonthly))-1)){
  ConsolidatedMonthly[(i+1),10] = round(as.numeric(((ConsolidatedMonthly[(i+1),9]/ConsolidatedMonthly[i,9])-1)*100),2)
}

#Adds CPI to ConsolidatedMonthly
#Add a "Year" Row to CPI 
CPI[16,] = substr(colnames(CPI),2,9)

#Renaming Row names and removing first columns
row.names(CPI) = CPI[,1]
CPI[,1] = NULL

#converting all strings to numeric
for (i in 1:length(colnames(CPI))){ CPI[,i] = as.numeric(CPI[,i])}

#setting  CPI to base year 2012 as GDP is set to 2012 dollars
for (i in 1:length(colnames(CPI))){ CPI[1,i] = round((CPI[1,i]/CPI[1,13])*100,2)}

for (i in 1:(length(row.names(ConsolidatedMonthly)))){
  ConsolidatedMonthly[i,11] = as.numeric(CPI[1,match(ConsolidatedMonthly[i,3],CPI[16,])])
  
}

#Resetting Header
colnames(ConsolidatedMonthly) = c("NumDate","Year.Qtr","Year","Quarter","Alcohol.Sale.CAD", "Alch.Growth.Pct",
                                  "Tobacco.Sale.CAD","TBC.Growth.Pct","Annualized.GDP","GDP.Growth.Pct","CPI.2012")

#resetting Year.Qtr column to Year than Qtr for easier ordering.
ConsolidatedMonthly[,2] = paste0(ConsolidatedMonthly$Year,ConsolidatedMonthly$Quarter)


write.csv(ConsolidatedMonthly,"ConsolidatedMonthly.csv", row.names = FALSE, )

#clearing environment
remove(list=ls())

#loading consolidated file from source
ConsolidatedMonthly = read.csv("ConsolidatedMonthly.csv")


# --------- Creating Annual file ---------

#creating Annual summary with GDP 
ConsolidatedAnnual = aggregate(Annualized.GDP~Year,ConsolidatedMonthly,mean)

#adding Percentage change to annual consolidation
for (i in 1:(length(ConsolidatedAnnual$Year)-1)){
  ConsolidatedAnnual[i+1,3] = round(((ConsolidatedAnnual[i+1,2]/ConsolidatedAnnual[i,2])-1)*100,2)
  
}

#Adding Alcohol sales 
for (i in 1:(length(ConsolidatedAnnual$Year))){
  ConsolidatedAnnual[i,4] = aggregate(Alcohol.Sale.CAD~Year,ConsolidatedMonthly,sum)[match(ConsolidatedAnnual$Year[i],aggregate(Alcohol.Sale.CAD~Year,ConsolidatedMonthly,sum)[,1]),2]
  
}

#Adding alcohol sales Percentage change 
for (i in 1:(length(ConsolidatedAnnual$Year)-1)){
  ConsolidatedAnnual[i+1,5] = round(((ConsolidatedAnnual[i+1,4]/ConsolidatedAnnual[i,4])-1)*100,2)
  
}


#Adding Tobacco sales 
for (i in 1:(length(ConsolidatedAnnual$Year))){
  ConsolidatedAnnual[i,6] = aggregate(Tobacco.Sale.CAD~Year,ConsolidatedMonthly,sum)[match(ConsolidatedAnnual$Year[i],aggregate(Tobacco.Sale.CAD~Year,ConsolidatedMonthly,sum)[,1]),2]
  
}

#Adding Tobacco Percentage change 
for (i in 1:(length(ConsolidatedAnnual$Year)-1)){
  ConsolidatedAnnual[i+1,7] = round(((ConsolidatedAnnual[i+1,6]/ConsolidatedAnnual[i,6])-1)*100,2)
  
}

colnames(ConsolidatedAnnual) = c("Year","GDP","GDP.Prct.Chg","Alcohol.Sales.CAD","Alcohol.Prct.Chg",
                                 "Tobacco.Sale.CAD","Tobacco.Prct.Chg")


write.csv(ConsolidatedAnnual,"ConsolidatedAnnual.csv", row.names = FALSE, )

remove(list=ls())



# --------- Creating quarterly  file ---------

#creating Annual summary with GDP 
ConsolidatedMonthly = read.csv("ConsolidatedMonthly.csv")
ConsolidatedQtr = aggregate(Annualized.GDP~Year.Qtr,ConsolidatedMonthly,mean)

#adding YYYYMMDD to Quarter
ConsolidatedQtr[,3] = ConsolidatedQtr[,2]
ConsolidatedQtr[,2] = ConsolidatedMonthly$NumDate[match(ConsolidatedQtr$Year.Qtr,ConsolidatedMonthly$Year.Qtr)]

#adding Percentage change to annual consolidation
for (i in 1:(length(ConsolidatedQtr$Year.Qtr)-1)){
  ConsolidatedQtr[i+1,4] = round(((ConsolidatedQtr[i+1,3]/ConsolidatedQtr[i,3])-1)*100,2)
  
}

#Adding Alcohol sales 
for (i in 1:(length(ConsolidatedQtr$Year.Qtr))){
  ConsolidatedQtr[i,5] = aggregate(Alcohol.Sale.CAD~Year.Qtr,ConsolidatedMonthly,sum)[match(ConsolidatedQtr$Year.Qtr[i],aggregate(Alcohol.Sale.CAD~Year.Qtr,ConsolidatedMonthly,sum)[,1]),2]
  
}

#Adding alcohol sales Percentage change 
for (i in 1:(length(ConsolidatedQtr$Year.Qtr)-1)){
  ConsolidatedQtr[i+1,6] = round(((ConsolidatedQtr[i+1,5]/ConsolidatedQtr[i,5])-1)*100,2)
  
}


#Adding Tobacco sales 
for (i in 1:(length(ConsolidatedQtr$Year.Qtr))){
  ConsolidatedQtr[i,7] = aggregate(Tobacco.Sale.CAD~Year.Qtr,ConsolidatedMonthly,sum)[match(ConsolidatedQtr$Year.Qtr[i],aggregate(Tobacco.Sale.CAD~Year.Qtr,ConsolidatedMonthly,sum)[,1]),2]
  
}

#Adding Tobacco Percentage change 
for (i in 1:(length(ConsolidatedQtr$Year.Qtr)-1)){
  ConsolidatedQtr[i+1,8] = round(((ConsolidatedQtr[i+1,7]/ConsolidatedQtr[i,7])-1)*100,2)
  
}

colnames(ConsolidatedQtr) = c("Year.Qtr","YearNum","GDP","GDP.Prct.Chg","Alcohol.Sales.CAD","Alcohol.Prct.Chg",
                              "Tobacco.Sale.CAD","Tobacco.Prct.Chg")


write.csv(ConsolidatedQtr,"ConsolidatedQtr.csv", row.names = FALSE, )

remove(list = ls())

#loading consolidated file from source for verification
ConsolidatedMonthly = read.csv("ConsolidatedMonthly.csv")
ConsolidatedAnnual = read.csv("ConsolidatedAnnual.csv")
ConsolidatedQtr = read.csv("ConsolidatedQtr.csv")
CanPop = read.csv("CanadaPopulation(Clean).csv", header = TRUE)
AlcoholSales = read.csv("AlcoholSales(Clean).csv", header = TRUE)






