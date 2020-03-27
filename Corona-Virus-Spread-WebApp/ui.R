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
require(DT)

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package
require(plotly)

#reading the dataset and doing some modifications

urlConfirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

urlDeaths<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

urlRecoveries<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

#reading the latest day cases data frame:
latest_day <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-25-2020.csv"

latest_day_cases<- read_csv(url(latest_day))


latestConf<-read_csv(url(urlConfirmed))


latestDeaths<-read_csv(url(urlDeaths))

latestRecoveries<-read_csv(url(urlRecoveries))




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


#making a dataframe on only the most latest cases on the most recent date(We simply extract the last column 
#which are the cases on that date and country)

CountrylatestConf<- latestConf %>% 
    #selecting column 2 i.e the country and last column which is the latest date added by WHO to the dataset
    select(2,ncol(latestConf)) 

#renaming the last column of the above dataframe for ease of data manipulation

colnames(CountrylatestConf) <- c("Country","LatestConf") 

# now we manipulate the dataframe: grouping buy the country and summarising the sum of cases for that country on the most recent date
CountrylatestConf <- CountrylatestConf %>% 
    group_by(Country) %>% 
    summarise(nCount = sum(LatestConf)) %>% 
    arrange(desc(nCount))

#the above dataset is the Count of confirmed cases for a country for the most recent date



#making a new dataset for Countries which have State data in the dataset(for tab3 selectInput)
State_data_country <- latest_day_cases %>% 
    #we will only have countries whose state data is available and not NA
    filter(!is.na(Province_State)) %>% 
    select(Country_Region,Confirmed)


#changing the col names
colnames(State_data_country) <- c("Country","Count") 
    
    
State_data_country <- State_data_country %>%     
    group_by(Country) %>% 
    summarise(nCount=sum(Count))


map_data_conf <- latestConf_long %>% 
    select(Lat,Long,Date,Count)




# Define UI for application that draws a histogram
dashboardPage(
    skin="red",
    dashboardHeader(title="COVID-19"),
    
    
    #dashboard sidebar
    dashboardSidebar(
        
        sidebarMenu(
            
            menuItem("Main Menu", tabName = "tab1" ,icon=icon("dashboard")),
            menuItem("Major Countries affected", tabName = "tab2",icon= icon("globe")),
            menuItem("States/Province affected", tabName = "tab3",icon= icon("cog")),
            menuItem("Daily Change in Cases", tabName = "tab4",icon=icon("calendar")),
            menuItem("Geospatial Analysis",tabName = "map",icon = icon("map"))
            
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
                    
                        h4("Total confirmed Cases till date:", align="left") , 
                        width=3,
                        textOutput("Confirmed"), #end text Output
                        #adding custom CSS for the text
                        tags$head(tags$style("#Confirmed{
                                 font-size: 18px;
                                 color:black;
                                 font-family:'Raleway', sans-serif;
                                 }"
                        )
                        ) # end head
                        
                    ), #end box
                    
                    box(
                        
                        h4("Total deaths till date:", align="left") , 
                        textOutput("Deaths"), #end text Output
                        width=3,
                        #adding custom CSS for the text
                        tags$head(tags$style("#Deaths{
                                 color: red;
                                 font-size: 18px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                        )) #end head
                    ) ,
                    
                    box(
                        
                        h4("Total Recoveries till date:", align="left") , 
                        textOutput("Recoveries"), #end text Output
                        width=3,
                        tags$head(tags$style("#Recoveries{
                                 color: green;
                                 font-size: 18px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                        )) #end head
                    ) , #end box 
                    
                    box(
                        
                        h4("Countries/Regions Affected:"),
                        width=3,
                        textOutput("countCountries"),
                        tags$head(tags$style("#countCountries{
                                 color: black;
                                 font-size: 18px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                        )) #end head
                        
                    ),#end box
                    
                    br(),
                    br(),
                    
                    
                    box(
                        
                        highchartOutput("StackedCOVID"),
                        width=12
                        
                    ), #end box
                    
                    br(),
                    hr(),
                    br(),
                    br(),
                    
                    
                    h3("Table of Countires and most Recent Cases till date",align="center"), 
                    br(),
                    #table of most comfirmed cases till date
                    box(
                        width = 4,
                        h3("Total cases:"),
                        p("Sorted by Case counts in desending order:"),
                        br(),
                        dataTableOutput("LatestConf"),
                        tags$head(tags$style("#LatestConf{
                                 color: black;
                                 font-size: 15px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                        )) #end head
                        
                        
                    ),#end box
                    
                    #table of most death cases till date
                    box(
                        width = 4,
                        h3("Total Deaths:"),
                        p("Sorted by number deaths in desending order:"),
                        br(),
                        dataTableOutput("LatestDeath") ,
                        tags$head(tags$style("#LatestDeath{
                                 color: red;
                                 font-size: 15px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                        )) #end head
                        
                        
                    ), #end box
                    
                    #table of recoveries till date
                    box(
                        width = 4,
                        h3("Total Recoveries:"),
                        p("Sorted by no of recoveries in desending order:"),
                        br(),
                        dataTableOutput("LatestRecovered") ,
                        tags$head(tags$style("#LatestRecovered{
                                 color: green;
                                 font-size: 15px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                        )) #end head
                        
                        
                    ) #end box
                    
                ) #end fluid row
                    
            ) , #end tabItem1
            
            
            tabItem(tabName = "tab2",
                    
                    h3("Countries Affected with COVID-19 cases",align="center"),
                    br(),
                    
                   fluidRow(
                       
                       column(12,
                              
                            box(
                                
                                width = 12,
                                selectInput("country" , label = "Select Country(Sorted by total COVID-19 case counts)",choices = CountrylatestConf[,1])
                            ),
                            
                            box( width=4,
                                
                               h3("Total Cases:"),
                               textOutput("CountryCases"),
                               #adding custom CSS for the text
                               tags$head(tags$style("#CountryCases{
                                 font-size: 20px;
                                 color:black;
                                 font-family:'Raleway', sans-serif;
                                 }"
                               )) # end head
                        ) ,
                            
                            
                            box(
                                width=4 ,
                                
                                h3("Total Deaths:"),
                                textOutput("CountryDeaths"),
                                #adding custom CSS for the text
                                tags$head(tags$style("#CountryDeaths{
                                 font-size: 20px;
                                 color:red;
                                 font-family:'Raleway', sans-serif;
                                 }"
                                )) # end head
                               
                                
                                
                            ),
                            
                            box(
                                width=4,
                                h3("Total Recovered:"),
                                textOutput("CountryRecovered"), 
                                #adding custom CSS for the text
                                tags$head(tags$style("#CountryRecovered{
                                 font-size: 20px;
                                 color:green;
                                 font-family:'Raleway', sans-serif;
                                 }"
                                )) # end head
                              
                                
                            ) ,
                            
                            box(
                                
                                highchartOutput("CountryChart"),
                                width=12
                                
                            ) 
                              
                    ) #end column
                       
                       
                       
                ) #end FluidRow
                    
                
            ), #end tabitem 2
            
            
            #Tab for state wise analysis
            tabItem(tabName = "tab3",
                    
                    
                fluidRow(
                    
                    column(12,
                           
                           box(
                               width = 12,
                               selectInput("countryState" , label = "Select Country(Only those which have state-wise cases and data being recorded)",choices = State_data_country[,1])
                           ),
                           
                           br(),
                           
                           br(),
                           
                           box(
                               width=4,
                               
                               h3("Cumalative Confirmed cases:"),
                               p("Sorted by max case counts"),
                               br(),
                               dataTableOutput("statesdata_conf")
                               
                           ), #end box
                           
                           box(
                               width=4,
                               
                               h3("Cumalative deaths:"),
                               p("Sorted by max Deaths"),
                               br(),
                               dataTableOutput("statesdata_death")
                               
                           ), #end box
                           
                           box(
                               width=4,
                               
                               h3("Cumalative Recoveries:"),
                               p("Sorted by max Recoveries"),
                               br(),
                               dataTableOutput("statesdata_recovered")
                               
                           ), #end box
                           
                           
                           box(
                               width=4,
                               
                               h3("Barplot of total cased in respective states:"),
                               p("Sorted by max case counts"),
                               br(),
                               highchartOutput("states_conf_chart")
                               
                           ), #end box
                           
                           box(
                               width=4,
                               
                               h3("Barplot of total deaths in respective states:"),
                               p("Sorted by max Deaths"),
                               br(),
                               highchartOutput("states_death_chart")
                               
                           ), #end box
                           
                           box(
                               width=4,
                               
                               h3("Barplot of total recoveries in respective states:"),
                               p("Sorted by max Recoveries"),
                               br(),
                               highchartOutput("states_recovered_chart")
                               
                           ) #end box
                           
                )#end column
                
            ) #end fluid row 
                    
                
                
            ), #end tab3
            
            
            #tab to record daily changes in cases
            tabItem(tabName = "tab4",
                    
                    
                    fluidRow(
                        
                    column(12,
                           
                           box(
                               
                               width = 12,
                               selectInput("countryChanges" , label = "Select Country(Sorted by total COVID-19 case counts)",choices = CountrylatestConf[,1])
                           ),
                           
                           box( width=4,
                                
                                h4("Daily Change in number of active cases:"),
                                p("The increase in cases recorded in last 2 days:"),
                                textOutput("caseChanges"),
                                br(),
                                br(),
                                dataTableOutput("caseTable"),
                               
                                #adding custom CSS for the text
                                tags$head(tags$style("#caseChanges{
                                 font-size: 25px;
                                 color:black;
                                 font-family:'Raleway', sans-serif;
                                 }"
                                )) # end head
                           ) ,
                           
                           
                           box(
                               width=4 ,
                               
                               h4("Daily Changes in number of Deaths:"),
                               p("The increase in deaths recorded in last 2 days:"),
                               textOutput("deathChanges"),
                               br(),
                               br(),
                               dataTableOutput("deathTable"),
                               
                               #adding custom CSS for the text
                               tags$head(tags$style("#deathChanges{
                                 font-size: 25px;
                                 color:red;
                                 font-family:'Raleway', sans-serif;
                                 }"
                               )) # end head
                               
                               
                               
                           ),
                           
                           box(
                               width=4,
                               h4("Daily Changes in number of recoveries"),
                               p("The increase in recoveries recorded in last 2 days:"),
                               textOutput("recoveryChanges"), 
                               br(),
                               br(),
                               dataTableOutput("RecoveryTable"),
                               #adding custom CSS for the text
                               tags$head(tags$style("#recoveryChanges{
                                 font-size: 25px;
                                 color:green;
                                 font-family:'Raleway', sans-serif;
                               }"
                                                    
                               )) # end head
                               
                               
                           ) ,
                           
                          #charts of daily changes of COVID-19 cases 
                           box(
                               width = 12,
                               
                               
                               highchartOutput("ChangeCountryConfChart")
                           ),
                          
                          #Daily changes bar plot of countries
                          box(
                              width = 12,
                              
                              
                              highchartOutput("ChangeCountryDeathChart")
                          ),
                          
                          
                          #Daily changes bar plot for recoveries
                          box(
                              width = 12,
                              
                              
                              highchartOutput("ChangeCountryRecoverChart")
                          )
                          
                           
                           
                               
                               
                               
                     ) #end column
                        
                        
               ) #end fluid row
                
         ) , #end tab item 4
            
            tabItem(tabName = "map",
                    
                    
                  #  fluidRow(
                        
                  #      column(12,
                               
                               
                              
                                   
                                   h3("Confirmed COVID-19 Cases world map"),
                                   leafletOutput("worldmap1") ,
                                   tags$head(tags$style("#worldmap1{
                                            width:80% ;
                                            height:80%;
                                
                                        }" ))
                            
                          #  ) # end column
                        
                   #  ) #end fluidRow
                
                
            ) #end tab5(Map)
            
        ) # end tabItems
        
    )#end dashboardBody
    
) # end dashboardPage
