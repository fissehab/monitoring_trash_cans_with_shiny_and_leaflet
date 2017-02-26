library(shiny)
library(leaflet)
library(plotly)
library(dplyr)

trashicons=function(condition){  # a function to make our own icons based on the sensor data
        makeIcon(
                iconUrl =paste0("trashcan_",condition,".png"),
                iconWidth =40, 
                iconHeight =45,
                iconAnchorX = 0, iconAnchorY = 0
        )}


localdb <- src_postgres(dbname = '',    # connect to an existing postgresql database
                        host = 'localhost',
                        port = 5432,
                        user = 'postgres',
                        password = 'postgrestutorial')


data_at_start=tbl(localdb,"trashcan_sensor_data")%>% # connect to tables within that database
        collect() # get the data

# get the most recent row for each sensor
data_at_start=arrange(data_at_start,-row_number())
data_at_start=distinct(data_at_start,sensorID,.keep_all = TRUE)

# If the cans are 95% full, change the icons to warning icon
# If a sensor for any can is not sending data, change the icon to failed icon
# If it has been more than 10 seconds since any sensor sent data, change the icon to failed
data_at_start$condition=ifelse(data_at_start$status>0.95,"warning","ok")
data_at_start$condition[is.na(data_at_start$status)]="failed"
data_at_start$condition[is.null(data_at_start$status)]="failed"
data_at_start$condition=ifelse((data_at_start$timestamp+10)< Sys.time(),"failed",data_at_start$condition)


testfunction <- function(){  #  function whose values over time will be tested for equality;
                             # inequality indicates that the underlying value has changed and 
                             # needs to be invalidated and re-read using ReadAllSensorData
        
        query="select max(timestamp) as max from trashcan_sensor_data"
        df=tbl(localdb,sql(query))%>%collect()
        df$max
}

ReadAllSensorData =function(){ # A function that gets data from the database
                               # based on the testfunction above
        query="SELECT * FROM trashcan_sensor_data"
        temp=tbl(localdb,sql(query))%>%collect(n = Inf)
        temp
}


shinyServer(function(input, output,session) {
        
        sensorData <- reactivePoll(100, session,testfunction, ReadAllSensorData)    
              # 100: number of milliseconds to wait between calls to testfunction

  
      output$leaflet_map <- renderLeaflet({
          dat=data_at_start
          top=max(dat$latitude)-0.004
          bottom=min(dat$latitude)-0.004
          right=max(dat$longitude)
          left=min(dat$longitude)
          leaflet(options = leafletOptions(minZoom = 15,maxZoom =18 ))%>%fitBounds(right,bottom,left,top)%>%
                   addTiles()%>%
                  addMarkers(data=dat,
                             lng= ~ longitude,
                             lat= ~ latitude,
                             icon = ~trashicons(dat$condition),
                             label=~as.character(sensorID),
                             labelOptions = labelOptions(textOnly = T,noHide =FALSE)
                  )
     })
 
    last_data=reactive({ # every time there is new data, get the last row for each sensorID
                         # If the cans are 95% full, change the icons to warning icon
                         # If a sensor for any can is not sending data, change the icon to failed icon
                         # If it has been more than 10 seconds since any sensor sent data, change the icon to failed
          dat=sensorData()
          dat=arrange(dat,-row_number())
          dat=distinct(dat,sensorID,.keep_all = TRUE)
          dat$condition=ifelse(dat$status>0.95,"warning","ok")
          dat$condition[is.na(dat$status)]="failed"
          dat$condition[is.null(dat$status)]="failed"
          dat$condition=ifelse((dat$timestamp+10)< Sys.time(),"failed",dat$condition)
          dat
  })
  
isdata_still_coming <- reactive({ # If all sensors are not sending data
                                  # such as when the gateway fails, we want to change all icons to failed
                invalidateLater(10000)    # Re-execute this reactive expression after 10 seconds
                dat=last_data()
                max_timestamp=max(dat$timestamp)

                (max_timestamp+10)>Sys.time() # return false if no data came in the last 10 seconds
})

 condition_observer=reactiveValues(condition=data_at_start$condition)
 
  # use leafletProxy to manage the dynamic icons: change icon color based on the data
  observe({
           dat=last_data()
            if(isdata_still_coming()==FALSE){
                  leafletProxy("leaflet_map", data = last_data()) %>%
                          clearMarkers() %>%
                          addMarkers(data=last_data(),
                                     lng= ~ longitude,
                                     lat= ~ latitude,
                                     icon = ~trashicons("failed"),
                                     label=~as.character(sensorID),
                                     labelOptions = labelOptions(textOnly = T)
                          )
                  condition_observer$condition="failed"
                  }else if(any(condition_observer$condition!=dat$condition)){
                  leafletProxy("leaflet_map", data = dat) %>%
                          clearMarkers() %>%
                          addMarkers(data=dat,
                                     lng= ~ longitude,
                                     lat= ~ latitude,
                                     icon = ~trashicons(dat$condition),
                                     label=~as.character(sensorID),
                                     labelOptions = labelOptions(textOnly = T)
                          )
                          condition_observer$condition=dat$condition
                  }
  })

  # Time series plot:helps us to verify that the icons are changing based on conditions we provided
  output$plotly_timeseries <- renderPlotly({
          dat= sensorData()
         
          z=ggplot(dat,aes(x=timestamp,y=status*100,color=sensorID))+geom_line()+
                  geom_hline(yintercept = 95,linetype="dotted",color="red")+ylab("Status")+
                  ylim(0,110)+xlab("")+ggtitle("Trash Can Status")+ylab("Percet Full")+
                  theme(axis.title.y = element_text(colour="blue",size=14),
                        axis.text = element_text(colour="darkred",size=12),
                        plot.title = element_text(colour="darkgreen",size=16,hjust=0.5))
          
          ggplotly(z)
  })

})
