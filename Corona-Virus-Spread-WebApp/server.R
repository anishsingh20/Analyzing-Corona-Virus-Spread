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
  
  urlRecoveries<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  
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
 
  CountrylatestRecovered <- latestRecoveries %>% 
    select(2,ncol(latestRecoveries)) 
  
  colnames(CountrylatestRecovered) <- c("Country","LatestRecovered") 
  
  CountrylatestRecovered <- CountrylatestRecovered %>% 
    group_by(Country) %>% 
    summarise(nCount = sum(LatestRecovered)) %>% 
    arrange(desc(nCount))
    
  
  
    output$Confirmed <- renderText({
      #R-code goes inside this
      
      lastcol<-ncol(latestConf) #getting the last column from the time series dataframe
      sum(latestConf[lastcol]) #printing the sum of values in the column
      
    }) #end Confirmed 
    
    output$Deaths <- renderText({
      
      lastcol<-ncol(latestDeaths)
      
      sum(latestDeaths[lastcol])
      
      
    })

    
    output$Recoveries <- renderText({
      
      lastcol<-ncol(latestRecoveries)
      
      sum(latestRecoveries[lastcol])
      
      
    })  
    
    
    
    output$StackedCOVID <- renderHighchart({
      
      highchart() %>% 
        hc_xAxis(categories=Date_latestConf_long$Date) %>% 
        hc_add_series(name="Deaths", data=Date_latestDeaths_long_date$nDeaths) %>% 
        hc_add_series(name="Recoveries",data=Date_latestRecoveries_long_date$nRecoveries) %>% 
        hc_add_series(name="Confirmed Cases", data=Date_latestConf_long$nConfirmed) %>% 
        hc_colors(c("red","green","black")) %>% 
        hc_add_theme(hc_theme_elementary()) %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Analysis of count of deaths,recoveries and cases for COVID-19 till date(Cumalative count)",align="center")
      
      
    })
    
    #A daily increase line graph
    output$StackedDailyIncLineGraph <- renderHighchart({
      
      #Conf cases date dataset
      ChangeConfdf <- latestConf_long %>% 
        group_by(Date) %>% 
        summarise(nConf = sum(Count)) 
       
       #ordering the dates
      ChangeConfdf <- ChangeConfdf[order(as.Date(ChangeConfdf$Date, format="%m/%d/%Y")),]
      
      #adding a new column of daily changes in cases
      ChangeConfdf <- ChangeConfdf %>% mutate(Daily_Confirmed = (nConf - lag(nConf)))
      
      
      
      #death dataset
      ChangeDeathdf <- latestDeaths_long %>% 
        group_by(Date) %>% 
        summarise(nDeaths = sum(Count))
      
      #ordering the dates
      ChangeDeathdf <- ChangeDeathdf[order(as.Date(ChangeDeathdf$Date, format="%m/%d/%Y")),]
      
      ChangeDeathdf <- ChangeDeathdf %>% mutate(Daily_Deaths = (nDeaths - lag(nDeaths)))
      
      #recovery dataset
      ChangeRecdf <- latestRecoveries_long %>% 
        group_by(Date) %>% 
        summarise(nRecovered = sum(Count))
      
      #ordering the dates
      ChangeRecdf <- ChangeRecdf[order(as.Date(ChangeRecdf$Date, format="%m/%d/%Y")),]
      
      ChangeRecdf <- ChangeRecdf %>% mutate(Daily_Recovered = (nRecovered - lag(nRecovered)))
      
      highchart() %>% 
        hc_xAxis(categories=ChangeConfdf$Date) %>% 
        hc_add_series(name="New Deaths", data=ChangeDeathdf$Daily_Deaths) %>% 
        hc_add_series(name="New Recoveries",data=ChangeRecdf$Daily_Recovered) %>% 
        hc_add_series(name="New Confirmed Cases", data=ChangeConfdf$Daily_Confirmed) %>% 
        hc_colors(c("red","green","purple")) %>% 
        hc_add_theme(hc_theme_elementary()) %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="New cases recorded on each date",align="center")
      
      
    })
    
    
    output$CountryChangeCasesChart <- renderHighchart({
      
      #Conf cases date dataset
      ChangeConfdf <- latestConf_long %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        group_by(Date) %>% 
        summarise(nConf = sum(Count)) 
      
      #ordering the dates
      ChangeConfdf <- ChangeConfdf[order(as.Date(ChangeConfdf$Date, format="%m/%d/%Y")),]
      
      #adding a new column of daily changes in cases
      ChangeConfdf <- ChangeConfdf %>% mutate(Daily_Confirmed = (nConf - lag(nConf)))
      
      
      
      #death dataset
      ChangeDeathdf <- latestDeaths_long %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        group_by(Date) %>% 
        summarise(nDeaths = sum(Count))
      
      #ordering the dates
      ChangeDeathdf <- ChangeDeathdf[order(as.Date(ChangeDeathdf$Date, format="%m/%d/%Y")),]
      
      ChangeDeathdf <- ChangeDeathdf %>% mutate(Daily_Deaths = (nDeaths - lag(nDeaths)))
      
      #recovery dataset
      ChangeRecdf <- latestRecoveries_long %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        group_by(Date) %>% 
        summarise(nRecovered = sum(Count))
      
      #ordering the dates
      ChangeRecdf <- ChangeRecdf[order(as.Date(ChangeRecdf$Date, format="%m/%d/%Y")),]
      
      ChangeRecdf <- ChangeRecdf %>% mutate(Daily_Recovered = (nRecovered - lag(nRecovered)))
      
      highchart() %>% 
        hc_xAxis(categories=ChangeConfdf$Date) %>% 
        hc_add_series(name="New Deaths", data=ChangeDeathdf$Daily_Deaths) %>% 
        hc_add_series(name="New Recoveries",data=ChangeRecdf$Daily_Recovered) %>% 
        hc_add_series(name="New Confirmed Cases", data=ChangeConfdf$Daily_Confirmed) %>% 
        hc_colors(c("red","green","purple")) %>% 
        hc_add_theme(hc_theme_elementary()) %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Number of New cases,Deaths and Recoveries recorded on each date",align="center")
      
      
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
      sum(df[lastcol])
        
    })
    
    output$CountryDeaths <- renderText({
      
      # Filtering the time series dataset for the country from the dropdown
      df <- latestDeaths %>%
        filter(`Country/Region` == input$country)
      
      #PICKING THE LAST COLUMN OF THE DATASET
      lastcol <- ncol(df)
      
      #sum of all the counts from last column
      sum(df[lastcol])
      
    })
    
    output$CountryRecovered <- renderText({
      
      # Filtering the time series dataset for the country from the dropdown
      df <- latestRecoveries %>%
        filter(`Country/Region` == input$country)
      
      #PICKING THE LAST COLUMN OF THE DATASET
      lastcol <- ncol(df)
      
      #sum of all the counts from last column
      sum(df[lastcol])
      
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
      new_df_country_recovered <- latestRecoveries_long %>% 
        select(`Country/Region`,Date,Count) %>%
        filter(`Country/Region` == input$country) %>% 
        group_by(Date) %>% 
        summarise(nRecovered=sum(Count)) %>% 
        arrange(nRecovered)
      
      
     
      
      highchart() %>% 
        hc_xAxis(categories=new_df_country_conf$Date) %>% 
        hc_add_series(name="Deaths", data=new_df_country_death$nDeaths) %>% 
        hc_add_series(name="Recoveries",data=new_df_country_recovered$nRecovered) %>% 
        hc_add_series(name="Confirmed Cases", data=new_df_country_conf $nConf) %>% 
        hc_colors(c("red","green","black")) %>% 
        hc_add_theme(hc_theme_elementary()) %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Time series Analysis of count of deaths,recoveries for and cases for COVID-19 till date(Cumalative count)",align="center")
      
        
        
      
      
    })
    
    
    output$statesdata_conf <- renderDataTable({
      
      
      #dataframe with country,states and most recent cases
      df_state <- latestConf %>% 
        filter(!is.na(`Province/State`)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(1,2,ncol(latestConf))
      
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
      df_state <- latestDeaths %>% 
        filter(!is.na(`Province/State`)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(1,2,ncol(latestDeaths))
      
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
      df_state <- latestRecoveries %>% 
        filter(!is.na(`Province/State`)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(1,2,ncol(latestRecoveries))
      
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
      df_state <- latestConf %>% 
        filter(!is.na(`Province/State`)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(1,2,ncol(latestConf))
      
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
      df_state <- latestDeaths %>% 
        filter(!is.na(`Province/State`)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(1,2,ncol(latestDeaths))
      
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
     
      df_state <- latestRecoveries %>% 
        filter(!is.na(`Province/State`)) %>% 
        #picking the last column which is the cumalative case cound for the latest date.
        select(1,2,ncol(latestRecoveries))
      
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
      
      #Conf cases date dataset
      ChangeConfdf <- latestConf_long %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        group_by(Date) %>% 
        summarise(nConf = sum(Count)) 
      
      #ordering the dates
      ChangeConfdf <- ChangeConfdf[order(as.Date(ChangeConfdf$Date, format="%m/%d/%Y")),]
      
      #adding a new column of daily changes in cases
      ChangeConfdf <- ChangeConfdf %>% mutate(Daily_Confirmed = (nConf - lag(nConf)))
      
      case= (ChangeConfdf[nrow(ChangeConfdf),3])
      
      case$Daily_Confirmed
      
    })
    
    output$caseTable <- renderDataTable({
      
      #Conf cases date dataset
      ChangeConfdf <- latestConf_long %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        group_by(Date) %>% 
        summarise(nConf = sum(Count)) 
      
      #ordering the dates
      ChangeConfdf <- ChangeConfdf[order(as.Date(ChangeConfdf$Date, format="%m/%d/%Y")),]
      
      #adding a new column of daily changes in cases
      ChangeConfdf <- ChangeConfdf %>% mutate(Daily_Confirmed = (nConf - lag(nConf)))
      
      ChangeConfdf
    })
    
    output$deathChanges <- renderText({
      
      #Conf cases date dataset
      ChangeDeathdf <- latestDeaths_long %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        group_by(Date) %>% 
        summarise(nDeaths = sum(Count)) 
      
      #ordering the dates
      ChangeDeathdf <- ChangeDeathdf[order(as.Date(ChangeDeathdf$Date, format="%m/%d/%Y")),]
      
      #adding a new column of daily changes in cases
      ChangeDeathdf <- ChangeDeathdf %>% mutate(Daily_Deaths = (nDeaths - lag(nDeaths)))
      
      case = (ChangeDeathdf[nrow(ChangeDeathdf),3])
      
      case$Daily_Deaths
    })
    
    output$deathTable <- renderDataTable({
      
      #Conf cases date dataset
      ChangeDeathdf <- latestDeaths_long %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        group_by(Date) %>% 
        summarise(nDeaths = sum(Count)) 
      
      #ordering the dates
      ChangeDeathdf <- ChangeDeathdf[order(as.Date(ChangeDeathdf$Date, format="%m/%d/%Y")),]
      
      #adding a new column of daily changes in cases
      ChangeDeathdf <- ChangeDeathdf %>% mutate(Daily_Deaths = (nDeaths - lag(nDeaths)))
      
      ChangeDeathdf
      
    })
    
    output$recoveryChanges <- renderText({
      
      ChangeRecdf <- latestRecoveries_long %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        group_by(Date) %>% 
        summarise(nRecovered = sum(Count))
      
      #ordering the dates
      ChangeRecdf <- ChangeRecdf[order(as.Date(ChangeRecdf$Date, format="%m/%d/%Y")),]
      
      #finding daily change in cases
      ChangeRecdf <- ChangeRecdf %>% mutate(Daily_Recovered = (nRecovered - lag(nRecovered)))
      
      case = (ChangeRecdf[nrow(ChangeRecdf),3])
      
      case$Daily_Recovered
      
    })
    
    
    output$RecoveryTable <- renderDataTable({
      
      ChangeRecdf <- latestRecoveries_long %>% 
        filter(`Country/Region` == input$countryChanges) %>% 
        group_by(Date) %>% 
        summarise(nRecovered = sum(Count))
      
      #ordering the dates
      ChangeRecdf <- ChangeRecdf[order(as.Date(ChangeRecdf$Date, format="%m/%d/%Y")),]
      
      #finding daily change in cases
      ChangeRecdf <- ChangeRecdf %>% mutate(Daily_Recovered = (nRecovered - lag(nRecovered)))
      
      ChangeRecdf
      
      
      
    })
    
    
   } #end function
) #end shinyServer
