## Extract an insitu and satellite concurrent time series and compare
##    first by simple subtraction of the two to get satellite bias
##    second by extracting late morning temperatures only
##    

## Requires '*dataTSall.Rdata' created by '2.0_getTS'


# the base URL for all data
#sourceURL <-("/media/robert/Seagate Backup Plus Drive/Backup Satellite Data/SST/GHRSST/")
sourceURL <-("/media/robert/Seagate Backup Plus Drive/SST/GHRSST/")

product <- c(
  "AVHRR_L3",
  "AVHRR-OI_L4",
  "CMC_L4",
  "G1SST_L4",
  "K10_L4",
  "MUR_L4",
  "MW-IR-v1_L4",
  "MW-IR-v4_L4",
  "ODYSSEA_L4",
  "OSTIA_L4")
sourceTS <- c(
  "L3C/AVHRR_L3/ts/",
  "L4/AVHRR-OI/ts/",
  "L4/CMC/ts/",
  "L4/G1SST/ts/",
  "L4/K10/ts/",
  "L4/MUR/ts/",
  "L4/MW_IR_v1.0/ts/",
  "L4/MW_IR_v4.0/ts/",
  "L4/ODYSSEA_SAF/ts/",
  "L4/OSTIA/ts/")
sensorRes <- c(4,25,20,1,10,1,9,9,10,5)       # vector of product pixel resolution

# load in situ stations
temp.space <- new.env()
temp <- load("~/R_projects-UWC/ExtractTS/Input/Insitu_daily.Rdata",temp.space)
data.insitu <- get(temp, temp.space)
rm(temp.space,temp)

# get list of station names
load("~/R_projects-UWC/ExtractTS/Input/site_list.Rdata")

for (j in 3:3){#length(product)){
  
  prod.name <- product[j]
  prod.res <- sensorRes[j]
  # for each station in 'station.names' get a subset of matching in situ and satellite stations nearshore
  
  # load SST product data for station
  temp.space <- new.env()
  temp <- load(paste0(sourceURL,sourceTS[j],product[j],"_dataTSall.Rdata"))
  data.sat <- get(temp,temp.space)
  rm(temp.space,temp)
  
  for (i in 1:length(station.names)){
    # if station name exists in 'data.sat'
    subset.insitu <- subset(data.insitu[data.insitu$site==station.names[i,1] &
                                        data.insitu$src==station.names[i,2],
                                        c("site","date","temp")])
    subset.sat <- subset(data.sat[data.sat$station==station.names[i] &
                                    data.sat$depth=="station",
                                  c("station","date","sst")])
    subset.match <- merge(subset.insitu,subset.sat, by="date")
    
    if (nrow(subset.match)==0){
      next
    }
    
    filename1 <- paste0(sourceURL,sourceTS[j],"images/",prod.name,"_",station.names[i],"_bias.png")
    
    png(filename1,width=8,height = 10,units="in",res=300)
    
    # plot the difference
    par(mfrow=c(3,1),oma = c(0, 0, 4, 0),font = 2, cex = 1.0, cex.axis = 0.8,mgp=(c(2.2,1,0)))
    par(mar=(c(4,4,3,4)))
    
    plot(subset.sat$date,subset.sat$sst,type="l",
         col="darkgreen",xlab="time",
         ylab=expression(paste("Satellite SST (",degree~C,")")),
         ylim=c(8,28), xaxt="n", cex.lab=1.2,font.lab=2,
         main="Satellite Data")
    axis.Date(1, at=seq(min(subset.sat$date), max(subset.sat$date), by="1 year"), format="%Y")
    abline(v=seq(min(subset.sat$date), max(subset.sat$date), by="1 year"),lty=2,col="darkgray")
    
    plot(subset.match$date,subset.match$temp,type="l",
         col="darkorange1",xlab="time",
         ylab=expression(paste("In situ Temperature (",degree~C,")")),
         ylim=c(8,28), xaxt="n", cex.lab=1.2,font.lab=2,
         main="In situ vs. Satellite data")
    lines(subset.match$date,subset.match$sst,col="darkgreen")
    axis.Date(1, at=seq(min(subset.insitu$date), max(subset.insitu$date), by="1 year"), format="%Y")
    abline(v=seq(min(subset.match$date), max(subset.match$date), by="1 year"),lty=2,col="darkgray")
    
    plot(subset.match$date,subset.match$sst-subset.match$temp,type="l",
         col="blue",xlab="time",
         ylab=expression(paste("Satellite Bias (",degree~C,")")),
         ylim=c(-10,10), xaxt="n", cex.lab=1.2,font.lab=2,
         main="Difference: Satellite - In situ")
    abline(0,0)
    axis.Date(1, at=seq(min(subset.match$date), max(subset.match$date), by="1 year"), format="%Y")
    abline(v=seq(min(subset.match$date), max(subset.match$date), by="1 year"),lty=2,col="darkgray")
    
    mtext(paste0(prod.name," (",prod.res," km Res.)\n",station.names[i]), outer = TRUE, cex = 1.5, font = 2)
    dev.off()
  }
}





