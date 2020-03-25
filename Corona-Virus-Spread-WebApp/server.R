#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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


library(maps)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package

library(ggthemes)

library(gganimate)

#filtering only those rows which have State data
  



# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
  # adding all the data reads inside the Shiny Server function. So that each time the datset is updated it reflects in the app. 
  
  
  urlConfirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  
  urlDeaths<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  
  urlRecoveries<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
  
  #reading the latest day cases data frame:
  latest_day <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-24-2020.csv"
  
  latest_day_cases<- read_csv(url(latest_day))
  
  
  #previous day data
  prev_day <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-23-2020.csv"
  
  prev_day_cases<- read_csv(url(prev_day))
  
  
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
  
  
#Below are the dataframes of most recent deaths, cases, and recoveries till date:
  
  
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
  
  
  #dataframe of latest deaths 
  CountrylatestDeath <- latestDeaths %>% 
    select(2,ncol(latestDeaths)) 
  
  colnames(CountrylatestDeath) <- c("Country","LatestDeath") 
  
  CountrylatestDeath <- CountrylatestDeath %>% 
    group_by(Country) %>% 
    summarise(nCount = sum(LatestDeath)) %>% 
    arrange(desc(nCount))
  
  #dataframe of latest Recoveries
 CountrylatestRecovered <- latest_day_cases %>% 
    select(Country_Region,Recovered) %>% 
    group_by(Country_Region) %>% 
    summarise(nCount=sum(Recovered)) %>% 
    arrange(desc(nCount))
  
#changing the names  
colnames(CountrylatestRecovered) <- c("Country","nCount")
  
  
    output$Confirmed <- renderText({
      #R-code goes inside this
      
      lastcol<-ncol(latestConf) #getting the last column from the time series dataframe
  
  
      sum(latestConf[lastcol],na.rm = TRUE) #printing the sum of values in the column, and removing any NA values
      
    }) #end Confirmed 
    
    output$Deaths <- renderText({
      
      lastcol<-ncol(latestDeaths)
      
      sum(latestDeaths[lastcol],na.rm = TRUE)
      
      
    })

    
    output$Recoveries <- renderText({
      
      #lastcol<-ncol(latestRecoveries)
      
      #sum(latestRecoveries[lastcol],na.rm = TRUE)
      sum(latest_day_cases$Recovered)
      
      
    })  
    
    
    
    output$StackedCOVID <- renderHighchart({
      
      highchart() %>% 
        hc_xAxis(categories=Date_latestConf_long$Date) %>% 
        hc_add_series(name="Deaths", data=Date_latestDeaths_long_date$nDeaths) %>% 
       # hc_add_series(name="Recoveries",data=Date_latestRecoveries_long_date$nRecoveries) %>% 
        hc_add_series(name="Confirmed Cases", data=Date_latestConf_long$nConfirmed) %>% 
        hc_colors(c("red","green","black")) %>% 
        hc_add_theme(hc_theme_elementary()) %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Analysis of count of deaths and cases for COVID-19 till date(Cumalative count)",align="center")
      
      
    })
    
    output$countCountries <- renderText({
      
      #this counts the no of rows in the dataset which is the number of countries/regions affected by COVID-19
       nrow(CountrylatestConf)
      
    })
    
    output$LatestConf <- renderDataTable({
      
      CountrylatestConf
      
    }) #end Table1
    
    output$LatestDeath <- renderDataTable({
      
      CountrylatestDeath
    }) #end table2 
    
    output$LatestRecovered <-renderDataTable({
      
      CountrylatestRecovered
     
      
    }) #end table 3
    
    output$CountryCases <- renderText({
       
      # Filtering the time series dataset for the country from the dropdown
      df <- latestConf %>%
        filter(`Country/Region` == input$country)
      
      #PICKING THE LAST COLUMN OF THE DATASET
      lastcol <- ncol(df)
      
      #sum of all the counts from last column
      sum(df[lastcol],na.rm = TRUE)
        
    })
    
    output$CountryDeaths <- renderText({
      
      # Filtering the time series dataset for the country from the dropdown
      df <- latestDeaths %>%
        filter(`Country/Region` == input$country)
      
      #PICKING THE LAST COLUMN OF THE DATASET
      lastcol <- ncol(df)
      
      #sum of all the counts from last column
      sum(df[lastcol],na.rm = TRUE)
      
    })
    
    output$CountryRecovered <- renderText({
      
      # Filtering the time series dataset for the country from the dropdown
      df <- latest_day_cases %>%
        filter(Country_Region == input$country)
      
      sum(df$Recovered)
  
    })
    
    
    #country specific chart of cases
    output$CountryChart <- renderHighchart({
      
      #creating a new data frame with only the selected country from user
      new_df_country_conf <- latestConf_long %>% 
        select(`Country/Region`,Date,Count) %>%
        filter(`Country/Region` == input$country) %>% 
        group_by(Date) %>% 
        summarise(nConf=sum(Count)) %>% 
        arrange(nConf)
      
      #Dataframe of deaths for the selected country
      new_df_country_death <- latestDeaths_long %>% 
        select(`Country/Region`,Date,Count) %>%
        filter(`Country/Region` == input$country) %>% 
        group_by(Date) %>% 
        summarise(nDeaths=sum(Count)) %>% 
        arrange(nDeaths)
      
      #Dataframe of recoveries of the selected country
     
      
      
     
      
      highchart() %>% 
        hc_xAxis(categories=new_df_country_conf$Date) %>% 
        hc_add_series(name="Deaths", data=new_df_country_death$nDeaths) %>% 
        #hc_add_series(name="Recoveries",data=new_df_country_recovered$nRecovered) %>% 
        hc_add_series(name="Confirmed Cases", data=new_df_country_conf $nConf) %>% 
        hc_colors(c("red","black")) %>% 
        hc_add_theme(hc_theme_elementary()) %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Time series Analysis of count of deaths and cases for COVID-19 till date(Cumalative count)",align="center")
      
        
        
      
      
    })
    
    
    output$statesdata_conf <- renderDataTable({
      
      
      #dataframe with country,states and most recent cases
      df_state <- latest_day_cases %>% 
        filter(!is.na(Province_State)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(3,4,8)
      
      colnames(df_state) <- c("State","Country","nCount") 
      
      #filtering the selected country and grouping by the State and summarising the total cases
      df_specific_country <- df_state %>% 
        filter(Country == input$countryState) %>% 
        group_by(State) %>% 
        summarise(nConf=sum(nCount)) %>% 
        arrange(desc(nConf))
      
      
      df_specific_country
      
    })
    
    
    output$statesdata_death <- renderDataTable({
      
      #dataframe with country,states and most recent cases
      df_state <- latest_day_cases %>% 
        filter(!is.na(Province_State)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(3,4,9)
      
      colnames(df_state) <- c("State","Country","nCount") 
      
      #filtering the selected country and grouping by the State and summarising the total cases
      df_specific_country <- df_state %>% 
        filter(Country == input$countryState) %>% 
        group_by(State) %>% 
        summarise(nDeaths=sum(nCount)) %>% 
        arrange(desc(nDeaths))
      
    }) #end states death
    
    
    output$statesdata_recovered <- renderDataTable({
      
      #dataframe with country,states and most recent cases
      df_state <- latest_day_cases %>% 
        filter(!is.na(Province_State)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(3,4,10)
      
      colnames(df_state) <- c("State","Country","nCount") 
      
      #filtering the selected country and grouping by the State and summarising the total cases
      df_specific_country <- df_state %>% 
        filter(Country == input$countryState) %>% 
        group_by(State) %>% 
        summarise(nRecovered=sum(nCount)) %>% 
        arrange(desc(nRecovered))
      
      
      
    }) #end states_recovered

    
    output$states_conf_chart <- renderHighchart({
      
      #dataframe with country,states and most recent cases
      df_state <- latest_day_cases %>% 
        filter(!is.na(Province_State)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(3,4,8)
      
      colnames(df_state) <- c("State","Country","nCount") 
      
      #filtering the selected country and grouping by the State and summarising the total cases
      df_specific_country <- df_state %>% 
        filter(Country == input$countryState) %>% 
        group_by(State) %>% 
        summarise(nConf=sum(nCount)) %>% 
        arrange(desc(nConf))
      
      
      hchart(df_specific_country, "column", hcaes(x = State,y = nConf), name="Confirmed cases:",color="black") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Number of COVID-19 cases",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
      
    })
    
    output$states_death_chart <- renderHighchart({
      
      
      #dataframe with country,states and most recent cases
      df_state <- latest_day_cases %>% 
        filter(!is.na(Province_State)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(3,4,9)
      
      colnames(df_state) <- c("State","Country","nCount") 
      
      #filtering the selected country and grouping by the State and summarising the total cases
      df_specific_country <- df_state %>% 
        filter(Country == input$countryState) %>% 
        group_by(State) %>% 
        summarise(nDeaths=sum(nCount)) %>% 
        arrange(desc(nDeaths))
      
      
      hchart(df_specific_country, "column", hcaes(x = State,y = nDeaths), name="Deaths:",color="red") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Number of deaths by COVID-19",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
    })
    
    output$states_recovered_chart <- renderHighchart({
      
      #dataframe with country,states and most recent cases
      df_state <- latest_day_cases %>% 
        filter(!is.na(Province_State)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(3,4,10)
      
      colnames(df_state) <- c("State","Country","nCount") 
      
      #filtering the selected country and grouping by the State and summarising the total cases
      df_specific_country <- df_state %>% 
        filter(Country == input$countryState) %>% 
        group_by(State) %>% 
        summarise(nRecovered=sum(nCount)) %>% 
        arrange(desc(nRecovered))
      
      hchart(df_specific_country, "column", hcaes(x = State,y = nRecovered), name="Recoveries:",color="green") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Number of Recoveries from COVID-19",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
    })
    
    
    output$caseChanges <- renderText({
      
      last_col <- ncol(latestConf)
      
      changes_df <- latestConf %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        select(last_col-1,last_col)
      
      #changing the names
      colnames(changes_df) <- c("Prev_Day","Today")
     
       changes_df <- changes_df %>% 
        mutate(Change = abs(Today-Prev_Day)) %>% #finding the difference
        summarise(Total_change=sum(Change))
      
       changes_df$Total_change
      
    })
    
    
    output$deathChanges <- renderText({
      
      last_col <- ncol(latestDeaths)
      
      changes_df <- latestDeaths %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        select(last_col-1,last_col)
      
      #changing the names
      colnames(changes_df) <- c("Prev_Day","Today")
      
      changes_df <- changes_df %>% 
        mutate(Change = abs(Today-Prev_Day)) %>% #finding the difference
        summarise(Total_change=sum(Change))
      
      changes_df$Total_change
      
    })
    
    
    output$recoveryChanges <- renderText({
      
      last_col <- ncol(latestRecoveries)
      
      changes_df <- latestRecoveries %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        select(last_col-1,last_col)
      
      #changing the names
      colnames(changes_df) <- c("Prev_Day","Today")
      
      changes_df <- changes_df %>% 
        mutate(Change = abs(Today-Prev_Day)) %>% #finding the difference
        summarise(Total_change=sum(Change))
      
      changes_df$Total_change
      
      
    })
    
    output$caseTable <- renderDataTable({
      
      last_col <- ncol(latestConf)
      
      df <- latestConf %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        select(`Province/State`,last_col-1,last_col) 
       
      
      
      df
        
    })
    
    output$deathTable <- renderDataTable({
      
      last_col <- ncol(latestDeaths)
      
      df <- latestDeaths %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        select(`Province/State`,last_col-1,last_col)
      
      
      df
      
    })
    
    output$RecoveryTable <- renderDataTable({
      
      last_col <- ncol(latestRecoveries)
      
      df <- latestRecoveries %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        select(`Province/State`,last_col-1,last_col)
      
      
      df
      
      
      
    })
    
    
    
    #world map of cases
    output$worldmap1 <- renderHighchart({
      
     
      
    })
    
    
    output$worldmap2 <- renderHighchart({
      
      
      
      
    
      
      
    })
    
    
    
    output$worldmap3 <- renderHighchart({
      
      
    
      
    })
    
    
   } #end function
) #end shinyServer
