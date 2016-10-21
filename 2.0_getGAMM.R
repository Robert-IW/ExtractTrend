## Get the GAMM and differences


source("~/R_projects-UWC/ExtractTS/func_gls.R")
source("~/R_projects-UWC/ExtractTS/func_nonlingamm.R")


for (a in 1:length(row_stations)){
  
  dataTS.subset <- subset(dataTS, dataTS$station==row_stations[a])
  
  filename1 <- paste0(sourceURL,dataSources[i,2],prod_name,"_gammAll")
  filename3 <- paste0(filename1,"_",row_stations[a],".Rdata")
  filename4 <- paste0(sourceURL,dataSources[i,3],prod_name,"_",row_stations[a],"_gamm.png")
  
  mod <- as_tibble(dataTS.subset)
  mod_gamm_non <- dlply(mod, .(depth), .progress = "text",.parallel = TRUE, gamm_non_fun)
  
  save(mod_gamm_non,file=filename3) 
  
  plot.names <- 
    
  png(filename4,width = 30,height = 20,units = 'in',res = 300)
  
  layout(matrix(c(1,1,1,2,3,4,5,5,5,6,7,8,9,9,9,10,11,12,13,13,13,14,15,16), ncol = 3, byrow = TRUE), heights = c(2,4,2,4,2,4,2,4))
  
  par(mar=c(.1,3,.1,3),lwd=2)
  plot.new()
  text(0.5,0.3,paste(row_stations[a],"Nearshore\n",prod_name),cex=4,font=2)
  par(mar=c(7,10,1,4),mgp=c(6,3,0))
  plot(mod_gamm_non$station$gam, cex.axis=3, cex.lab=3)
  plot(dataTS.subset$date[dataTS.subset$depth=="station"],dataTS.subset$sst[dataTS.subset$depth=="station"],type="l",cex.axis=3,cex.lab=3)
  
  par(mar=c(.1,3,.1,3))
  plot.new()
  text(0.5,0.3,"200 m Depth",cex=4,font=2)
  par(mar=c(7,10,1,4))
  plot(mod_gamm_non$`-200`$gam, cex.axis=3, cex.lab=3)
  plot(dataTS.subset$date[dataTS.subset$depth=="-200"],dataTS.subset$sst[dataTS.subset$depth=="-200"],type="l",cex.axis=3,cex.lab=3)
  
  par(mar=c(.1,3,.1,3))
  plot.new()
  text(0.5,0.3,"500 m Depth",cex=4,font=2)
  par(mar=c(7,10,1,4))
  plot(mod_gamm_non$`-500`$gam, cex.axis=3, cex.lab=3)
  plot(dataTS.subset$date[dataTS.subset$depth=="-500"],dataTS.subset$sst[dataTS.subset$depth=="-500"],type="l",cex.axis=3,cex.lab=3)
  
  par(mar=c(.1,3,.1,3))
  plot.new()
  text(0.5,0.3,"2000 m Depth",cex=4,font=2)
  par(mar=c(7,10,1,4))
  plot(mod_gamm_non$`-2000`$gam, cex.axis=3, cex.lab=3)
  plot(dataTS.subset$date[dataTS.subset$depth=="-2000"],dataTS.subset$sst[dataTS.subset$depth=="-2000"],type="l",cex.axis=3,cex.lab=3)
  layout(1)
  dev.off()
  
  rm(dataTS.subset, mod_gamm_non)
  gc()
}