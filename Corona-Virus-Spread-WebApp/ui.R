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
dashboardPage(
    
    
    
    
    
    
) # end dashboard page