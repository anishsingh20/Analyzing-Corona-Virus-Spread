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

latestConf_long<- gather(latestConf, Date, Count, `1/22/20`:ncol(latestConf))

latestDeaths_long<- gather(latestDeaths, Date, Count, `1/22/20`:ncol(latestDeaths) )

latestRecoveries_long<- gather(latestRecoveries, Date, Count, `1/22/20`:ncol(latestRecoveries) )

Date_latestConf_long <- latestConf_long %>% 
    group_by(Date) %>%
    summarise(nConfirmed=sum(Count)) %>% 
    arrange((nConfirmed))


Date_latestDeaths_long_date <- latestDeaths_long %>% 
    group_by(Date) %>%
    summarise(nDeaths=sum(Count)) %>% 
    arrange((nDeaths))


Date_latestRecoveries_long_date <- latestRecoveries_long %>% 
    group_by(Date) %>%
    summarise(nRecoveries=sum(Count)) %>% 
    arrange((nRecoveries))


#making a dataset for sorting the countires in ascending order of cases for the dropdown
CountrylatestConf_long<- latestConf_long %>% 
    group_by(`Country/Region`) %>% 
    summarise(nConf = sum(Count)) %>% 
    arrange(desc(nConf))

# Define UI for application that draws a histogram
dashboardPage(
    skin="black",
    dashboardHeader(title="COVID-19"),
    
    
    #dashboard sidebar
    dashboardSidebar(
        
        sidebarMenu(
            
            menuItem("Main Menu", tabName = "tab1" ,icon=icon("dashboard")),
            menuItem("Major Countries affected", tabName = "tab2")
            
        ) #end sidebarmenu
        
    ), # end dashboardsidebar
    
    
    #dashboardBody
    dashboardBody(
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Merriweather|Playfair+Display|Raleway")
        ),
        
        #adding all the tabs
        tabItems(
            
            tabItem(tabName ="tab1",
                    
                    h2("Analysing Corona Virus Spread",align="center",style="margin-top:-5px;"),
                    br() ,
                    
                fluidRow(
                    
                    box(
                    
                        h3("Total confirmed Cases till date:", align="left") , 
                        width=4,
                        textOutput("Confirmed"), #end text Output
                        #adding custom CSS for the text
                        tags$head(tags$style("#Confirmed{
                                 font-size: 20px;
                                 color:black;
                                 font-family:'Raleway', sans-serif;
                                 }"
                        )
                        ) # end head
                        
                    ), #end box
                    
                    box(
                        
                        h3("Total deaths till date:", align="left") , 
                        textOutput("Deaths"), #end text Output
                        width=4,
                        #adding custom CSS for the text
                        tags$head(tags$style("#Deaths{
                                 color: red;
                                 font-size: 20px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                        )) #end head
                    ) ,
                    
                    box(
                        
                        h3("Total Recoveries till date:", align="left") , 
                        textOutput("Recoveries"), #end text Output
                        width=4,
                        tags$head(tags$style("#Recoveries{
                                 color: green;
                                 font-size: 20px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                        )) #end head
                    ) ,
                    
                    
                    box(
                        
                        highchartOutput("StackedCOVID"),
                        width=12
                        
                    )#end box
                    
                ) #end fluid row
                    
            ) , #end tabItem1
            
            
            
            tabItem(tabName = "tab2",
                    
                    h3("Countries Affected with COVID-19 cases",align="center"),
                    br(),
                    
                   fluidRow(
                       
                       column(12,
                              
                            box(
                                
                                width = 12,
                                selectInput("country" , label = "Select Country(Sorted by total COVID-19 case counts)",choices = CountrylatestConf_long[,1])
                            ),
                            
                            box(
                                
                               h3("Total Cases:"),
                               textOutput("CountryCases"),
                               #adding custom CSS for the text
                               tags$head(tags$style("#CountryCases{
                                 font-size: 20px;
                                 color:black;
                                 font-family:'Raleway', sans-serif;
                                 }"
                               )
                               ) ,# end head
                                width=4
                                
                            ) ,
                            
                            
                            box(
                                
                                h3("Total Deaths:"),
                                textOutput("CountryDeaths"),
                                #adding custom CSS for the text
                                tags$head(tags$style("#CountryDeaths{
                                 font-size: 20px;
                                 color:red;
                                 font-family:'Raleway', sans-serif;
                                 }"
                                )
                                ),
                                # end head
                                width=4
                                
                            ),
                            
                            box(
                                
                                h3("Total Recovered:"),
                                textOutput("CountryRecovered"), 
                                #adding custom CSS for the text
                                tags$head(tags$style("#CountryRecovered{
                                 font-size: 20px;
                                 color:green;
                                 font-family:'Raleway', sans-serif;
                                 }"
                                )
                                ), # end head
                                width=4
                                
                            ) ,
                            
                            box(
                                
                                highchartOutput("CountryChart"),
                                width=12
                                
                            ) 
                              
                            ) #end column
                       
                       
                       
                   ) #end FluidRow
                    
                
            ) #end tabitem 2
            
        ) # end tabItems
        
    )#end dashboardBody
    
) # end dashboardPage