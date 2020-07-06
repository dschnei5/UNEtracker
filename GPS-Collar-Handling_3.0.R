#######################################################################################################
##########Completes UNETracker GPS Processing with Timestamp Fix#######################################
#######################################################################################################

####Versioning####
# 3.0 - Adapted from GPS Collar Handling 2.0
#     - Fixes timestamp issue
#     - Doesnt include LRI Modelling
#     - Next step is to incorporate gDistance and spatial metrics

####Requires Libraries####

library(lubridate);
library(rgdal);

####Import Data####

setwd(choose.dir(default = "C:\\R\\UNEtracker\\", caption = "Choose location containing only raw tracking text files"));  ###Location of Raw Tracking .TXT Files###
files <- list.files(getwd(), pattern = ".txt", ignore.case = TRUE);
dname <- vector('list',2);

####Set your projection####
UCRS.new <- CRS("+proj=utm +zone=50 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs");

####Process data by file####

for (i in seq(along=files)) {
  #i = 1
  dname[i] <- paste("data",i, sep = ".");
  assign(dname[[i]], read.csv(files[i], header = FALSE));
  tmp1.df <- get(dname[[i]]);
  GGA.tmp <- subset(tmp1.df, V1=="$GPGGA");
  RMC.tmp <- subset(tmp1.df, V1=="$GPRMC");
  tmp1.df <- cbind(GGA.tmp, RMC.tmp);
  if (length(tmp1.df[,1]) == 0) {message("File empty, moving to next file....")
  } else {
      
  colnames(tmp1.df) <- c("GPGGA","UTC_Time","Lat","N/S1","Long","E/W1","Fix","Satellites","HDOP","Altitude","M","g.g","M","Unknown","Tag","$GPRMC","Time2","A","Latitude","N/S2","Longitude","E/W2","Speed","Course","UTC_Date","Blank","Checksum","Unk1","Unk2","Unk3");
  assign(dname[[i]],tmp1.df[,-c(1,4,6,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,30)]);
  tmp2.df <- get(dname[[i]]);
  tmp2.df$UTC_Time <- ifelse(tmp2.df$UTC_Time <= 99.99, paste0("0000",tmp2.df$UTC_Time),
                              ifelse(tmp2.df$UTC_Time <= 999.99, paste0("000",tmp2.df$UTC_Time),
                                     ifelse(tmp2.df$UTC_Time <=9999.99, paste0("00",tmp2.df$UTC_Time),
                                            ifelse(tmp2.df$UTC_Time <= 99999.99, paste0("0", tmp2.df$UTC_Time),
                                                   tmp2.df$UTC_Time))));
  tmp2.df$UTC_Date <- ifelse(tmp2.df$UTC_Date < 99999, paste0("0",tmp2.df$UTC_Date), tmp2.df$UTC_Date);
  tmp2.df <- cbind(tmp2.df, paste(tmp2.df$UTC_Date,tmp2.df$UTC_Time));
  colnames(tmp2.df) <- c("UTC_Time","Lat","Long","Fix","Satellites","HDOP","Altitude","UTC_Date", "UTC_DateTime");
  tmp2.df <- cbind(tmp2.df, dmy_hms(tmp2.df$UTC_DateTime, tz = "GMT"));
  tmp2.df <- tmp2.df[,-(9)];
  colnames(tmp2.df) <- c("UTC_Time","Lat","Long","Fix","Satellites","HDOP","Altitude","UTC_Date", "UTC_DateTime");
  tmp2.df$EST_DateTime <- with_tz(tmp2.df$UTC_DateTime,"Australia/Brisbane");
  tmp2.df$EST_Hour <- hour(tmp2.df$EST_DateTime);
  tmp2.df$SecInt <- tmp2.df$UTC_DateTime[-1] - tmp2.df$UTC_DateTime;
  tmp2.df$SecInt <- as.numeric(tmp2.df$SecInt);
  tmp2.df$Long <- as.character(tmp2.df$Long);
  tmp2.df$Long <- as.numeric(tmp2.df$Long);
  tmp2.df$Lat <- as.character(tmp2.df$Lat);
  tmp2.df$Lat <- as.numeric(tmp2.df$Lat);
  tmp2.df$Long.con <- tmp2.df$Long/100;
  tmp2.df$Lat.con <- tmp2.df$Lat/100;
  tmp2.df$Long.con1 <- as.integer(tmp2.df$Long.con);
  tmp2.df$Lat.con1 <- as.integer(tmp2.df$Lat.con);
  tmp2.df$Long.con2 <- (tmp2.df$Long.con-tmp2.df$Long.con1)*100;
  tmp2.df$Lat.con2 <- (tmp2.df$Lat.con-tmp2.df$Lat.con1)*100;
  tmp2.df$Longitude <- (tmp2.df$Long.con1+(tmp2.df$Long.con2/60));
  tmp2.df$Latitude <- -(tmp2.df$Lat.con1+(tmp2.df$Lat.con2/60));
  tmp2.df <- tmp2.df[,-c(2,3,13,14,15,16,17,18)];
  tmp2.df$Longitude <- ifelse(tmp2.df$Longitude <= 1,NA,tmp2.df$Longitude);
  tmp2.df <- tmp2.df[!is.na(tmp2.df$Longitude),]
  tmp2.df$Latitude <- ifelse(tmp2.df$Latitude >= 1,NA,tmp2.df$Latitude);
  tmp2.df <- tmp2.df[!is.na(tmp2.df$Latitude),]
  tmp2.df$Longitude <- as.numeric(tmp2.df$Longitude)
  tmp2.df$Latitude <- as.numeric(tmp2.df$Latitude)
  coords <- SpatialPoints(data.frame(X = tmp2.df$Longitude,Y = tmp2.df$Latitude),CRS("+proj=longlat +datum=WGS84"))
  d.UTM <- spTransform(coords, UCRS.new);
  tmp2.df <- cbind(tmp2.df,d.UTM@coords);
  tmp2.df$StepLength <- sqrt((tmp2.df$X[-1] - tmp2.df$X)^2+(tmp2.df$Y[-1] - tmp2.df$Y)^2);
  tmp2.df$Speed <- tmp2.df$StepLength/as.numeric(tmp2.df$SecInt);
  tmp2.df$File <- files[i];
  tmp2.df$CollarID <- i;
  assign(dname[[i]],tmp2.df);
  }
} #####End of i loop####

###Clean up###

rm(tmp1.df,tmp2.df,GGA.tmp,RMC.tmp,coords,d.UTM,dname,i);

###Write Individual Files to Folder###

data.out <- ls();
a <- length(files)+1;
b <- length(files)+2;
data.out <- data.out[-c(b,a)];
rm(a,b);

AllDataOut.df <- data.frame();

for (j in seq(along=data.out)) {
  
  tmp3.df <- get(data.out[[j]]);
  write.csv(tmp3.df,paste0(getwd(),"/", files[j],".CSV"));
  AllDataOut.df <- rbind(AllDataOut.df,get(data.out[[j]]));
}; ##End j loop##

###Write all files to one file##

write.csv(AllDataOut.df,file = paste0(getwd(),"/AllCollarsUncleaned.CSV"));
