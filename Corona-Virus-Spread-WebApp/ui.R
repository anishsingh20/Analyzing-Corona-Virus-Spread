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

urlConfirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

urlDeaths<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"

urlRecoveries<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

latestConf<-read_csv(url(urlConfirmed))

latestDeaths<-read_csv(url(urlDeaths))

latestRecoveries<-read_csv(url(urlRecoveries))

latestConf_long<- gather(latestConf, Date, Count, `1/22/20`:ncol(latestConf))

latestDeaths_long<- gather(latestDeaths, Date, Count, `1/22/20`:ncol(latestDeaths) )

latestRecoveries_long<- gather(latestRecoveries, Date, Count, `1/22/20`:ncol(latestRecoveries) )





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
                    br() ,
                    
                fluidRow(
                    
                    box(
                    
                        h4("Total confirmed Cases till date:", align="left") , 
                        textOutput("Confirmed")#end text Output
        
                    ), #end box
                    
                    box(
                        
                        h4("Total deaths till date:", align="left") , 
                        textOutput("Deaths") #end text Output
                        
                    ) ,
                    
                    box(
                        
                        h4("Total Recoveries till date:", align="left") , 
                        textOutput("Recoveries") #end text Output
                        
                    )
                    
                ) #end fluid row
                    
            ), #end tabItem1
            
            tabItem(tabName = "tab2",
                    
                    h3("Top Most countries Affected with most COVID-19 cases",align="center"),
                    br()
                    
                
            ) #end tabitem 2
            
        ) # end tabItems
        
    )#end dashboardBody
    
) # end dashboardPage