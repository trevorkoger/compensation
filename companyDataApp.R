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

#CompData <- read.csv ("C:/Users/Trevor Koger/Desktop/Shiny Items/ShinyCompDataCompany.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Compensation for Executives by Companies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("nameInput", "Name", choices = CompData$Company.Name),
                 selected = "CONAGRA BRANDS INC"),
    mainPanel(plotOutput("toPlot"),
    br(), br(),
    tableOutput("results")
    )
  ) 
)
   
   
server <- function(input, output) {
  
  output$toPlot <- renderPlot({
  
    CMC2 <- CompData %>%
      filter(Company.Name == input$nameInput)
    
    CMC2[is.na(CMC2)] <- 0
    
    CMCSalary <- tapply(CMC2$SALARY, CMC2$YEAR, sum, na.rm=TRUE)
    CMCBonus <- tapply(CMC2$BONUS, CMC2$YEAR, sum, na.rm=TRUE)
    CMCStock <- tapply(CMC2$STOCK, CMC2$YEAR, sum, na.rm=TRUE)
    CMCOption <-tapply(CMC2$OPTION, CMC2$YEAR, sum, na.rm=TRUE)
    CMCNonequity <-tapply(CMC2$NONEQUITY, CMC2$YEAR, sum, na.rm=TRUE)
    CMCNqdefcomp <-tapply(CMC2$NQDEFCOMP, CMC2$YEAR, sum, na.rm=TRUE)
    CMCOther <- tapply(CMC2$OTHER, CMC2$YEAR, sum, na.rm=TRUE)
    
    
    sumTotals <- data.frame(cbind(as.vector(CMCSalary), as.vector(CMCBonus), as.vector(CMCStock), as.vector(CMCOption), as.vector(CMCNonequity), as.vector(CMCNqdefcomp), as.vector(CMCOther)) )
    
    
    names(sumTotals) = c("SALARY", "BONUS", "STOCK", "OPTION", "NONEQUITY", "NQDEFCOMP", "OTHER")
    
    sumTotals
    CMCSalary
    years<- CMC2 %>% 
      select(YEAR, "SALARY") %>%
      group_by(YEAR) %>%
      count(YEAR)
    companyYearData <- years$YEAR
    
    sumTotals$YEAR <- companyYearData
    
    sumTotals$TOTAL <- sumTotals[,"SALARY"] + sumTotals[,"STOCK"] + sumTotals[,"BONUS"] + sumTotals[,"OPTION"] + sumTotals[,"NONEQUITY"] + sumTotals[,"NQDEFCOMP"] + sumTotals[,"OTHER"]
    
    
    sumTotals$SalaryPercent <- sumTotals[,"SALARY"] / sumTotals[, "TOTAL"]
    sumTotals$BonusPercent <- sumTotals[,"BONUS"] / sumTotals[, "TOTAL"]
    sumTotals$StockPercent <- sumTotals[,"STOCK"] / sumTotals[, "TOTAL"]
    sumTotals$OptionPercent <- sumTotals[,"OPTION"] / sumTotals[, "TOTAL"]
    sumTotals$NonequityPercent <- sumTotals[,"NONEQUITY"] / sumTotals[, "TOTAL"]
    sumTotals$NqdefcompPercent <- sumTotals[,"NQDEFCOMP"] / sumTotals[, "TOTAL"]
    sumTotals$OtherPercent <- sumTotals[,"OTHER"] / sumTotals[, "TOTAL"]
    
    percentageTotals <- select(sumTotals, YEAR,TOTAL,SalaryPercent, BonusPercent,StockPercent,OptionPercent,NonequityPercent, NqdefcompPercent, OtherPercent)
    
    
    mdat <- melt(percentageTotals, id = c("YEAR", "TOTAL"))
    
    ggplot(mdat, aes(x=YEAR, y = value, fill = variable)) + geom_bar(stat="identity")  + scale_x_continuous(breaks = companyYearData)
      
  })
  
  output$results <- renderTable({
    
    CMC2 <- CompData %>%
      filter(Company.Name == input$nameInput)
    
    CMC2[is.na(CMC2)] <- 0
    
    CMCSalary <- tapply(CMC2$SALARY, CMC2$YEAR, sum, na.rm=TRUE)
    CMCBonus <- tapply(CMC2$BONUS, CMC2$YEAR, sum, na.rm=TRUE)
    CMCStock <- tapply(CMC2$STOCK, CMC2$YEAR, sum, na.rm=TRUE)
    CMCOption <-tapply(CMC2$OPTION, CMC2$YEAR, sum, na.rm=TRUE)
    CMCNonequity <-tapply(CMC2$NONEQUITY, CMC2$YEAR, sum, na.rm=TRUE)
    CMCNqdefcomp <-tapply(CMC2$NQDEFCOMP, CMC2$YEAR, sum, na.rm=TRUE)
    CMCOther <- tapply(CMC2$OTHER, CMC2$YEAR, sum, na.rm=TRUE)
    
    
    sumTotals <- data.frame(cbind(as.vector(CMCSalary), as.vector(CMCBonus), as.vector(CMCStock), as.vector(CMCOption), as.vector(CMCNonequity), as.vector(CMCNqdefcomp), as.vector(CMCOther)) )
    
    
    names(sumTotals) = c("SALARY", "BONUS", "STOCK", "OPTION", "NONEQUITY", "NQDEFCOMP", "OTHER")
    
    sumTotals
    CMCSalary
    years<- CMC2 %>% 
      select(YEAR, "SALARY") %>%
      group_by(YEAR) %>%
      count(YEAR)
    companyYearData <- years$YEAR
    
    sumTotals$YEAR <- companyYearData
    
    sumTotals$TOTAL <- as.integer(sumTotals[,"SALARY"] + sumTotals[,"STOCK"] + sumTotals[,"BONUS"] + sumTotals[,"OPTION"] + sumTotals[,"NONEQUITY"] + sumTotals[,"NQDEFCOMP"] + sumTotals[,"OTHER"])
    
    
    sumTotals$SalaryPercent <- sumTotals[,"SALARY"] / sumTotals[, "TOTAL"]
    sumTotals$BonusPercent <- sumTotals[,"BONUS"] / sumTotals[, "TOTAL"]
    sumTotals$StockPercent <- sumTotals[,"STOCK"] / sumTotals[, "TOTAL"]
    sumTotals$OptionPercent <- sumTotals[,"OPTION"] / sumTotals[, "TOTAL"]
    sumTotals$NonequityPercent <- sumTotals[,"NONEQUITY"] / sumTotals[, "TOTAL"]
    sumTotals$NqdefcompPercent <- sumTotals[,"NQDEFCOMP"] / sumTotals[, "TOTAL"]
    sumTotals$OtherPercent <- sumTotals[,"OTHER"] / sumTotals[, "TOTAL"]
    
    select(sumTotals, YEAR,TOTAL,SalaryPercent, BonusPercent,StockPercent,OptionPercent,NonequityPercent, NqdefcompPercent, OtherPercent)
    

  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

