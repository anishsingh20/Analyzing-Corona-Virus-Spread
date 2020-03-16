#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


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


library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

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
        hc_title(text="Analysis of count of deaths,recoveries and cases for COVID-19 till date",align="center")
      
      
    })

    
   } #end function
) #end shinyServer
