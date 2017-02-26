library(plot3D)
library(RPostgreSQL)

# Open up a database connection.

 pg = dbDriver("PostgreSQL")

 con = dbConnect(pg, user = 'postgres', password = "postgrestutorial",
                 host="localhost", port=5432, dbname="")

 ## Let's generate latitude and longitude locations in Alexandira, VA
 
lats=seq(from=38.80,to=38.8148,by=0.005)
lons=seq(from=-77.06,to=-77.04,by=0.005)
latlons=mesh(lons,lats)
longitude=as.vector(latlons$x)
latitude=as.vector(latlons$y)

sensorID=paste0("SN",10001:10015)  # senorIDS that help to identify trash cans
        

# Insert data to the database
# All 15 sensors will send data every 2 seconds for 20 seconds
# Generating random data in increasing order: ratio of volume full


i=0
status=matrix(0,ncol=15,nrow=1)
while(i<20){
        
        temp=matrix(runif(15,min=0,max=0.03),ncol=15)
        status=rbind(status,temp)
        status=colSums(status)
        status[status>=1]=1
        
        sensordata=data.frame(timestamp=Sys.time(),
                              sensorID=sensorID,
                              longitude=longitude,
                              latitude=latitude,
                              status=status,
                              stringsAsFactors = FALSE
                             )
        dbWriteTable(con,'trashcan_sensor_data',sensordata, row.names=FALSE,append=TRUE)
        

i=i+1
Sys.sleep(2)
}


# Insert data to the database
# Let's assume two sensors are not sending data
# 13 sensors will send data every 2 seconds for 20 seconds

i=0

while(i<20){
        
        temp=matrix(c(runif(13,min=0,max=0.03),rep(NA,2)),ncol=15)
        status=rbind(status,temp)
        status=colSums(status)
        status[status>=1]=1
        
        sensordata=data.frame(timestamp=Sys.time(),
                              sensorID=sensorID,
                              longitude=longitude,
                              latitude=latitude,
                              status=status,
                              stringsAsFactors = FALSE
        )
        dbWriteTable(con,'trashcan_sensor_data',sensordata, row.names=FALSE,append=TRUE)
        
        
        i=i+1
        Sys.sleep(2)
}


# Insert data to the database
# Now, let's assume five sensors are not sending data
# 10 sensors will send data every 2 seconds for 20 seconds

## Assume the gateway will fail fater 20+20+20 seconds and hence all
## sensors will not send data at all


i=0

while(i<20){
        
        temp=matrix(c(runif(10,min=0,max=0.03),rep(NA,5)),ncol=15)
        status=rbind(status,temp)
        status=colSums(status)
        status[status>=1]=1
        
        sensordata=data.frame(timestamp=Sys.time(),
                              sensorID=sensorID,
                              longitude=longitude,
                              latitude=latitude,
                              status=status,
                              stringsAsFactors = FALSE
        )
        dbWriteTable(con,'trashcan_sensor_data',sensordata, row.names=FALSE,append=TRUE)
        
        
        i=i+1
        Sys.sleep(2)
        
}
