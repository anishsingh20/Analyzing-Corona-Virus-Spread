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
require(readr)
require(anytime)

#reading the dataset and doing some modifications

nCOV<-read_csv("../data/2019_nCoV_data.csv",col_names = TRUE)
nCOV$Date<-anydate(nCOV$Date)
#ommitting any NA values if there
nCOV<-na.omit(nCOV)

#chaiging the column name
colnames(nCOV)[3]="State"

#attaching the data frame
attach(nCOV)


# Define UI for application that draws a histogram
dashboardPage(
    skin="black",
    dashboardHeader(title="COVID-19"),
    
    
    #dashboard sidebar
    dashboardSidebar(
        
        sidebarMenu(
            
            menuItem("Main Menu", tabName = "tab1" ,icon=icon("dashboard")),
            menuItem("Major Countries affected", tabName = "tab12")
            
        ) #end sidebarmenu
        
    ), # end dashboardsidebar
    
    
    #dashboardBody
    dashboardBody(
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        
        #adding all the tabs
        tabItems(
            
            tabItem(tabName ="tab1",
                    
                    h2("Analysing Corona Virus Spread from 22 Jan 2020 till 9th Feb 2020.",align="center",style="margin-top:-5px;"),
                    br()
                    
            ), #end tabItem1
            
            tabItem(tabName = "tab2",
                    
                    h3("Top Most countries Affected with most COVID-19 cases",align="center"),
                    br()
                    
                
            ) #end tabitem 2
            
        ) # end tabItems
        
    )#end dashboardBody
    
) # end dashboardPage