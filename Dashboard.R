library(ggplot2)
library(shiny)
library(sf) #loading data

ConsolidatedAnnual = read.csv("ConsolidatedAnnual.csv")
CanadaMapShape = read_sf("gpr_000b11a_e.shp")

#loading Alcohol percapita file
PerCapita = read.csv("consolidatedAlcoholPercapita.csv")

#setting Province IDs to string
PerCapita$PRUID = c("","10","11","12","13","24","35","46","47","48","59","60","61","62")

#Reminder: [Row, Column]
OneFile = merge(CanadaMapShape,PerCapita, by = c("PRUID"), all = TRUE)

#Removing Canada row as it causes error due to the fact it does not have cordinate data.
#coor data at province level
OneFile = OneFile[2:14,]
rownames(OneFile) = 1:13

#creating list drop down menu for map
MapDropDown = colnames(OneFile)[12:28]

ui = fluidPage(
  titlePanel(h1("VICES & ECONOMIC RECESSIONS ")),
  sidebarPanel(selectInput(inputId = "SourceData", label = "Dollars or Growth/Decline(YoY)", choices = c("Percentage Change","Dollar Value") ),
               checkboxGroupInput(inputId = "ThreeMetrics",label = "Components", choices = c("GDP", "Alcohol", "Tobacco")),
               "Note: GDP is divided by 100 as to make the trend and any potential correlation more visible."),
  mainPanel(plotOutput("Line")),
  sidebarPanel(selectInput(inputId = "MapYear", label = "Choose Year", choices = MapDropDown)),
  mainPanel(plotOutput("Map"))
  )

server = function(input, output) {
  output$Line = renderPlot({
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
        panel.grid.major.x = element_line(color = "gray50", size = 0.05),
        plot.title = element_text(size = 14, face = "bold.italic", color = "#0c73c2")
      )
    }) # Line Graph ends
  output$Map = renderPlot({
      ggplot(data = OneFile)+ 
        geom_sf(aes(fill = OneFile$FY2012))+
        geom_sf_label(aes(label = OneFile$FY2012 ))+
        scale_fill_gradient(low = "#adc7db", high = "#04537d", na.value = NA)+
        ggtitle("Alcohol per Capita")+
        theme(panel.background = element_blank(),
              legend.position = 'none', 
              axis.title=element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(size = 14, face = "bold.italic", color = "#0c73c2")
              )
      })
}

shinyApp(ui, server)
