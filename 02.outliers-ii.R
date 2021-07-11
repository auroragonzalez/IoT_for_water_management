source("myplots.R")
library(data.table)
library(tsoutliers)
library(jmotif)
## ------------------------------------------------------------------------
files = dir("aggregatedMeters/")
averias = read.table("faults.csv", sep=";", head=T)

for (i in 1:length(files)){
  print(i)
  df = fread(paste0("aggregatedMeters/",files[i]), sep=";", head=T) #usamos fread porque son conjuntos grandes  
  averia = averias[as.character(averias$contador) == as.character(df$contador[1]),]
  AVdate = as.Date(averia$anomalyDetected, "%d/%m/%Y")
  # 30 días antes y 3 días después
  if(sum(df$Date == AVdate)!=0){  # checking if there are data in such date
    if((min(which(df$Date == AVdate))- 12*30) <0){  # comprobar que hay al menos 30 dias anteriores a la avería
      limInf = 1
      print(paste0("No hay datos de 30 dias anteriores, solo de ", min(which(df$Date == AVdate))/12))
    }else{
      limInf = (min(which(df$Date == AVdate))- 12*30)
    }
    cont2 = df[limInf:(min(which(df$Date == AVdate))+ 12*3),]
    plot(cont2$Sum, type="l", xlab="", ylab="consumo", main = cont2$nameCont[1], xaxt = 'n')
    axis(1, at=c(1,nrow(cont2)/2,nrow(cont2)), labels=c(cont2$Date[1], cont2$Date[nrow(cont2)/2],cont2$Date[nrow(cont2)])) 

    # ARIMA Y DEMÁS
    dat.ts<- ts(cont2$Sum,frequency=1)
    data.ts.outliers <- tso(dat.ts, types = c("AO", "LS", "TC"))
    dev.off()
    pdf(paste0("plots/", i,".",files[i],"AR.pdf"), height = 4, width = 8)
    myplots(data.ts.outliers)
    axis(1, at=c(1,nrow(cont2)/2,nrow(cont2)), labels=c(cont2$Date[1], cont2$Date[nrow(cont2)/2],cont2$Date[nrow(cont2)])) 
    ind =which(format(as.Date(averia$anomalyDetected,"%d/%m/%Y"), "%Y-%m-%d" ) == cont2$Date)
    abline(v=ind[length(ind)/2], col="red", lwd=3)  
    while (!is.null(dev.list()))  dev.off()
    
    
    #HOT SAX
    
    len = 10
    discords = find_discords_hotsax(cont2$Sum, len, 4, 4, 0.01, 5)
    while (!is.null(dev.list()))  dev.off()
    pdf(paste0("plots/", i,".",files[i],"HS.pdf"), height = 4, width = 8)
    plot(cont2$Sum, type = "l", col = "blue", main = "", xaxt = "n", xlab = "", ylab = "Water Consumption")
    axis(1, at=c(1,nrow(cont2)/2,nrow(cont2)), labels=c(cont2$Date[1], cont2$Date[nrow(cont2)/2],cont2$Date[nrow(cont2)])) 
    abline(v=ind[length(ind)/2], col="red", lwd=3)
    for(i in 1:nrow(discords)){
      lines(x=c(discords[i,2]:(discords[i,2]+len)),
            y=cont2$Sum[discords[i,2]:(discords[i,2]+len)], col="red")
    }
    while (!is.null(dev.list()))  dev.off()
    
  }else{
    print("The dates of the fault and meter data do not coincide")
  }
  
  }



