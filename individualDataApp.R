#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)

CompData <- read.csv ("M:/TrevorProject/Data Project/R Files/Rough Draft Adding Items/Final Project/ShinyCompDataIndividual.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Compensation for Executives by Person"),
  sidebarLayout(
    sidebarPanel(selectInput("nameInput", "Name", choices = CompData$PERSON)),
    mainPanel(plotOutput("toPlot"),
    br(), br(),
    tableOutput("results")
    )
  ) 
)
   
   
server <- function(input, output) {
  
  output$toPlot <- renderPlot({
  
    sumTotalsPerson <- CompData %>%
      filter(PERSON == input$nameInput)
    
    
    sumTotalsPerson[is.na(sumTotalsPerson)] <- 0
    
    sumTotalsPerson$TOTAL <- sumTotalsPerson[,"SALARY"] + sumTotalsPerson[,"STOCK"] + sumTotalsPerson[,"BONUS"] + sumTotalsPerson[,"OPTION"] + sumTotalsPerson[,"NONEQUITY"] + sumTotalsPerson[,"NQDEFCOMP"] + sumTotalsPerson[,"OTHER"]
    
    sumTotalsPerson <- data.frame(sumTotalsPerson)
    
    sumTotalsPerson$SalaryPercent <- sumTotalsPerson[,"SALARY"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$BonusPercent <- sumTotalsPerson[,"BONUS"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$StockPercent <- sumTotalsPerson[,"STOCK"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$OptionPercent <- sumTotalsPerson[,"OPTION"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$NonequityPercent <- sumTotalsPerson[,"NONEQUITY"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$NqdefcompPercent <- sumTotalsPerson[,"NQDEFCOMP"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$OtherPercent <- sumTotalsPerson[,"OTHER"] / sumTotalsPerson[, "TOTAL"]
    
    percentageTotalsPerson <- select(sumTotalsPerson, YEAR,TOTAL,SalaryPercent, BonusPercent,StockPercent,OptionPercent,NonequityPercent, NqdefcompPercent, OtherPercent)
    
    mdatPerson <- melt(percentageTotalsPerson, id = c("YEAR", "TOTAL"))
    
    ggplot(mdatPerson, aes(x=YEAR, y = value, fill = variable)) + geom_bar(stat="identity")  + scale_x_continuous(breaks = mdatPerson$YEAR)
      
  })
  
  output$results <- renderTable({
    
    sumTotalsPerson <- CompData %>%
      filter(PERSON == input$nameInput)
    
    
    sumTotalsPerson[is.na(sumTotalsPerson)] <- 0
    
    sumTotalsPerson$TOTAL <- sumTotalsPerson[,"SALARY"] + sumTotalsPerson[,"STOCK"] + sumTotalsPerson[,"BONUS"] + sumTotalsPerson[,"OPTION"] + sumTotalsPerson[,"NONEQUITY"] + sumTotalsPerson[,"NQDEFCOMP"] + sumTotalsPerson[,"OTHER"]
    
    sumTotalsPerson <- data.frame(sumTotalsPerson)
    
    sumTotalsPerson$SalaryPercent <- sumTotalsPerson[,"SALARY"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$BonusPercent <- sumTotalsPerson[,"BONUS"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$StockPercent <- sumTotalsPerson[,"STOCK"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$OptionPercent <- sumTotalsPerson[,"OPTION"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$NonequityPercent <- sumTotalsPerson[,"NONEQUITY"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$NqdefcompPercent <- sumTotalsPerson[,"NQDEFCOMP"] / sumTotalsPerson[, "TOTAL"]
    sumTotalsPerson$OtherPercent <- sumTotalsPerson[,"OTHER"] / sumTotalsPerson[, "TOTAL"]
    
    select(sumTotalsPerson, YEAR,TOTAL,SalaryPercent, BonusPercent,StockPercent,OptionPercent,NonequityPercent, NqdefcompPercent, OtherPercent)
    

  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

