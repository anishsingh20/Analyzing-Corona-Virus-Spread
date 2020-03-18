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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
  # adding all the data reads inside the Shiny Server function. So that each time the datset is updated it reflects in the app. 
  
  
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

    
   } #end function
) #end shinyServer
