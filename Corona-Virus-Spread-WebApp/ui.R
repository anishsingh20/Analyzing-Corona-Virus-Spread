#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(highcharter)
require(dplyr)
require(tidyr)
require(shiny)
require(shinydashboard)
require(data.table)

#reading the dataset and doing some modifications

nCOV<-read_csv("data/2019_nCoV_data.csv",col_names = TRUE)
nCOV$Date<-anydate(nCOV$Date)
#ommitting any NA values if there
nCOV<-na.omit(nCOV)

#chaiging the column name
colnames(nCOV)[3]="State"

#attaching the data frame
attach(nCOV)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Analyzing COVID-19 Spread worldwide"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

        # Show a plot of the generated distribution
        mainPanel("This dataset contains data from 22 JAN 2020 till 9 FEB 2020.") , 
    ), #end sidebarlayout
    
    
    
)) #end shinUI
