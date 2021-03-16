setwd("C:/Users/A02323599/Dropbox/SmMammal/cam sites only")

indexdat<-read.csv("DensityData.csv")
head(indexdat)

# alldat<-merge(newdat,indexdat,by="Site")
# head(alldat)


srdat<-sqrt(indexdat[,-c(1,2,4,8,12,16,83:89)])
srdat[is.na(srdat)]<-0
head(srdat)
newsrdat<-cbind(indexdat[,2],srdat,indexdat[,c(83:89)])
head(newsrdat)

analyze <- function (model){
  print(summary(model))
  plot(model)
}

analyze2 <- function (model){
  print(summary(model))
  # plot(model$model[,1]~model$model[,2])
  # x <- seq(min(model$model[,2]),max(model$model[,2]),0.01)
  # newdat<-data.frame(x)
  # print(newdat)
  # colnames(newdat)<-colnames(model$model[2])
  # abline(model)
  # points(x,predict(model,newdata=newdat))
  plot(model, main = colnames(model$model)[2])
}


mouselm1 <- lm(mouse_den~mouseCPUE_7,data=newsrdat)
mouselm2 <- lm(mouse_den~mouseMNA_7,data=newsrdat)
mouselm3 <- lm(mouse_den~MouseInd,data=newsrdat)
mouselm4 <- lm(mouse_den~MouseProp2,data=newsrdat)

volelm1 <- lm(vole_den~voleCPUE_7,data=newsrdat)
volelm2 <- lm(vole_den[-c(7,11)]~voleMNA_7[-c(7,11)],data=newsrdat)
volelm3 <- lm(vole_den~VoleInd,data=newsrdat)
volelm4 <- lm(vole_den~VoleProp2,data=newsrdat)

chiplm1 <- lm(chip_den~chipCPUE_7,data=newsrdat)
chiplm2 <- lm(chip_den[-c(9)]~chipMNA_7[-c(9)],data=newsrdat)
chiplm3 <- lm(chip_den~ChipInd,data=newsrdat)
chiplm4 <- lm(chip_den~ChipProp2,data=newsrdat)

totlm1 <- lm(total_den~totCPUE_7,data=newsrdat)
totlm2 <- lm(total_den~totMNA_7,data=newsrdat)
totlm3 <- lm(total_den~TotInd,data=newsrdat)
totlm4 <- lm(total_den~TotProp2,data=newsrdat)

pdf("diagnostics.pdf")
par(mfrow=c(2,2))
analyze2(mouselm1)
analyze2(mouselm2)
analyze2(mouselm3)
analyze2(mouselm4)

analyze2(volelm1)
analyze2(volelm2)
analyze2(volelm3)
analyze2(volelm4)

analyze2(chiplm1)
analyze2(chiplm2)
analyze2(chiplm3)
analyze2(chiplm4)

analyze2(totlm1)
analyze2(totlm2)
analyze2(totlm3)
analyze2(totlm4)

dev.off()


str(summary(mouselm1))
summary(mouselm1)$coefficients[3]
round(c(summary(mouselm1)$coefficients[1],summary(mouselm1)$coefficients[3],
  summary(mouselm1)$coefficients[2],summary(mouselm1)$coefficients[4],
  summary(mouselm1)$r.squared,summary(mouselm1)$coefficients[8]),3)
modlist<-list(mouselm1,mouselm2,mouselm3,mouselm4,volelm1,volelm2,volelm3,volelm4,
           chiplm1,chiplm2,chiplm3,chiplm4,totlm1,totlm2,totlm3,totlm4)
resultstab<-matrix(round(c(summary(modlist[[1]])$coefficients[1],confint(modlist[[1]])[1],confint(modlist[[1]])[3],
                                    summary(modlist[[1]])$coefficients[2],confint(modlist[[1]])[2],confint(modlist[[1]])[4],
                                    summary(modlist[[1]])$r.squared,summary(modlist[[1]])$coefficients[8]),3)
                                    ,nrow=16,ncol=8,byrow = T)
modnames<-list(names(modlist[[1]]$model[2]))
for(i in 2:length(modlist)){
  resultstab[i,]<-(round(c(summary(modlist[[i]])$coefficients[1],confint(modlist[[i]])[1],confint(modlist[[i]])[3],
                           summary(modlist[[i]])$coefficients[2],confint(modlist[[i]])[2],confint(modlist[[i]])[4],
                           summary(modlist[[i]])$r.squared,summary(modlist[[i]])$coefficients[8]),3))
  modnames[[i]]<-names(modlist[[i]]$model[2])
}
resultstab<-as.data.frame(resultstab)
colnames(resultstab)<-c("Intercept","Lower","Upper","Slope","Lower","Upper","R2","p")
resultstab$model<-unlist(modnames)
resultstab

write.csv(resultstab,file="RegressionresultsCI.csv")

####MNA####

ylabels<-c("Minimum mice alive after \n 1 caputre event",
           "Minimum mice alive after \n 2 caputre events",
           "Minimum mice alive after \n 3 caputre events",
           "Minimum mice alive after \n 4 caputre events",
           "Minimum mice alive after \n 5 caputre events",
           "Minimum mice alive after \n 6 caputre events",
           "Minimum mice alive after \n 7 caputre events")

micemna <-with(newsrdat,as.data.frame(cbind(mouseMNA_1,mouseMNA_2,mouseMNA_3,mouseMNA_4,mouseMNA_5,mouseMNA_6,mouseMNA_7)))

pdf("MouseMNA.pdf")
lmMTI<-list()
mousemna.rsquared<-vector(length=7)
par(mar=c(5,7,2,2))
for(i in 1:length(micemna)){
  plot(newsrdat$mouse_den,micemna[,i],xlab="Mouse density (#/hectare)",ylab="",cex.lab=1.75,cex.axis=1.25,bty="l",pch=19)
  title(ylab=ylabels[i],cex.lab=1.75)
  lmMTI[[i]]<-summary(lm(newsrdat$mouse_den~micemna[,i]))
  # abline(lmMTI)
  mousemna.rsquared[i]<-lmMTI[[i]]$r.squared
}
dev.off()

ylabels<-c("Minimum voles alive after \n 1 caputre event",
           "Minimum voles alive after \n 2 caputre events",
           "Minimum voles alive after \n 3 caputre events",
           "Minimum voles alive after \n 4 caputre events",
           "Minimum voles alive after \n 5 caputre events",
           "Minimum voles alive after \n 6 caputre events",
           "Minimum voles alive after \n 7 caputre events")

volesmna <-with(newsrdat,as.data.frame(cbind(voleMNA_1,voleMNA_2,voleMNA_3,voleMNA_4,voleMNA_5,voleMNA_6,voleMNA_7)))

pdf("VoleMNA.pdf")
lmVTI<-list()
volemna.rsquared<-vector(length=7)
par(mar=c(5,7,2,2))
for(i in 1:length(volesmna)){
  plot(newsrdat$vole_den[-c(7,11)],volesmna[,i][-c(7,11)],xlab="Vole density (#/hectare)",ylab="",cex.lab=1.75,cex.axis=1.25,bty="l",pch=19)
  title(ylab=ylabels[i],cex.lab=1.75)
  lmVTI[[i]]<-summary(lm(newsrdat$vole_den[-c(7,11)]~volesmna[,i][-c(7,11)]))
  # abline(lmMTI)
  volemna.rsquared[i]<-lmVTI[[i]]$r.squared
}
dev.off()

ylabels<-c("Minimum chipmunks alive after \n 1 caputre event",
           "Minimum chipmunks alive after \n 2 caputre events",
           "Minimum chipmunks alive after \n 3 caputre events",
           "Minimum chipmunks alive after \n 4 caputre events",
           "Minimum chipmunks alive after \n 5 caputre events",
           "Minimum chipmunks alive after \n 6 caputre events",
           "Minimum chipmunks alive after \n 7 caputre events")

chipmna <-with(newsrdat,as.data.frame(cbind(chipMNA_1,chipMNA_2,chipMNA_3,chipMNA_4,chipMNA_5,chipMNA_6,chipMNA_7)))

pdf("chipMNA.pdf")
lmCTI<-list()
chipmna.rsquared<-vector(length=7)
par(mar=c(5,7,2,2))
for(i in 1:length(chipmna)){
  plot(newsrdat$chip_den[-9],chipmna[,i][-9],xlab="Chipmunk density (#/hectare)",ylab="",cex.lab=1.75,cex.axis=1.25,bty="l",pch=19)
  title(ylab=ylabels[i],cex.lab=1.75)
  lmCTI[[i]]<-summary(lm(newsrdat$chip_den[-9]~chipmna[,i][-9]))
  # abline(lmMTI)
  chipmna.rsquared[i]<-lmCTI[[i]]$r.squared
}
dev.off()

ylabels<-c("Minimum total alive after \n 1 caputre event",
           "Minimum total alive after \n 2 caputre events",
           "Minimum total alive after \n 3 caputre events",
           "Minimum total alive after \n 4 caputre events",
           "Minimum total alive after \n 5 caputre events",
           "Minimum total alive after \n 6 caputre events",
           "Minimum total alive after \n 7 caputre events")

totalmna <-with(newsrdat,as.data.frame(cbind(totMNA_1,totMNA_2,totMNA_3,totMNA_4,totMNA_5,totMNA_6,totMNA_7)))

pdf("TotalMNA.pdf")
lmTTI<-list()
totalmna.rsquared<-vector(length=7)
par(mar=c(5,7,2,2))
for(i in 1:length(totalmna)){
  plot(newsrdat$total_den,totalmna[,i],xlab="Total density (#/hectare)",ylab="",cex.lab=1.75,cex.axis=1.25,bty="l",pch=19)
  title(ylab=ylabels[i],cex.lab=1.75)
  lmTTI[[i]]<-summary(lm(newsrdat$total_den~totalmna[,i]))
  # abline(lmMTI)
  totalmna.rsquared[i]<-lmTTI[[i]]$r.squared
}
dev.off()
mousemna.rsquared
volemna.rsquared
chipmna.rsquared
totalmna.rsquared


####MNA Pub Figure####

par(mar=c(5,5,2,2))
par(mfrow=(c(2,2)))
plot(1:7,mousemna.rsquared,type = "b",xlab="Capture occasions - mice",ylab="MNA R-squared",cex.lab=2,cex.axis=1.5,ylim=c(0.25,1),bty="l",yaxt="n",pch=19)
text(x=1,y=1,labels = "a",cex=1.5)
axis(side=2,at=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),labels=c('',"0.4",'','0.6','','0.8','','1.0'),cex.axis=1.5,las=2)
plot(1:7,volemna.rsquared,type = "b",xlab="Capture occasions - voles",ylab="MNA R-squared",cex.lab=2,cex.axis=1.5,ylim=c(0.25,1),bty="l",yaxt="n",pch=19)
text(x=1,y=1,labels = "b",cex=1.5)
axis(side=2,at=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),labels=c('',"0.4",'','0.6','','0.8','','1.0'),cex.axis=1.5,las=2)
plot(1:7,chipmna.rsquared,type = "b",xlab="Capture occasions - chipmunks",ylab="MNA R-squared",cex.lab=2,cex.axis=1.5,ylim=c(0.25,1),bty="l",yaxt="n",pch=19)
text(x=1,y=1,labels = "c",cex=1.5)
axis(side=2,at=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),labels=c('',"0.4",'','0.6','','0.8','','1.0'),cex.axis=1.5,las=2)
plot(1:7,totalmna.rsquared,type = "b",xlab="Capture occasions - total",ylab="MNA R-squared",cex.lab=2,cex.axis=1.5,ylim=c(0.25,1),bty="l",yaxt="n",pch=19)
text(x=1,y=1,labels = "d",cex=1.5)
axis(side=2,at=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),labels=c('',"0.4",'','0.6','','0.8','','1.0'),cex.axis=1.5,las=2)



####CPUE####

ylabels<-c("Mouse captures per 100 trap nights \n in 1 caputre event",
           "Mouse captures per 100 trap nights \n in 2 caputre events",
           "Mouse captures per 100 trap nights \n in 3 caputre events",
           "Mouse captures per 100 trap nights \n in 4 caputre events",
           "Mouse captures per 100 trap nights \n in 5 caputre events",
           "Mouse captures per 100 trap nights \n in 6 caputre events",
           "Mouse captures per 100 trap nights \n in 7 caputre events")

micecpue <-with(newsrdat,as.data.frame(cbind(mouseCPUE_1,mouseCPUE_2,mouseCPUE_3,mouseCPUE_4,mouseCPUE_5,mouseCPUE_6,mouseCPUE_7)))
nightscpue <- with(newsrdat,as.data.frame(cbind(nights_1,nights_2,nights_3,nights_4,nights_5,nights_6,nights_7)))

pdf("Mouse100.pdf")
Mouse100n.rsquared<-vector(length=7)
par(mar=c(5,7,2,2))
for(i in 1:length(micecpue)){
  plot(newsrdat$mouse_den,I(micecpue[,i]/nightscpue[,i]),ylab="",xlab="Mouse density (#/hectare)",
       cex.lab=1.75,cex.axis=1.25,bty="l",pch=19)
  title(ylab=ylabels[i],cex.lab=1.75)
  lmMTN<-lm(newsrdat$mouse_den~I(micecpue[,i]/nightscpue[,i]))
  # abline(lmMTN)
  Mouse100n.rsquared[i]<-summary(lmMTN)$r.squared
}
dev.off()

ylabels<-c("Vole captures per 100 trap nights \n in 1 caputre event",
           "Vole captures per 100 trap nights \n in 2 caputre events",
           "Vole captures per 100 trap nights \n in 3 caputre events",
           "Vole captures per 100 trap nights \n in 4 caputre events",
           "Vole captures per 100 trap nights \n in 5 caputre events",
           "Vole captures per 100 trap nights \n in 6 caputre events",
           "Vole captures per 100 trap nights \n in 7 caputre events")

volecpue <-with(newsrdat,as.data.frame(cbind(voleCPUE_1,voleCPUE_2,voleCPUE_3,voleCPUE_4,voleCPUE_5,voleCPUE_6,voleCPUE_7)))
nightscpue <- with(newsrdat,as.data.frame(cbind(nights_1,nights_2,nights_3,nights_4,nights_5,nights_6,nights_7)))

pdf("Vole100.pdf")
Vole100n.rsquared<-vector(length=7)
par(mar=c(5,7,2,2))
for(i in 1:length(volecpue)){
  plot(newsrdat$vole_den,I(volecpue[,i]/nightscpue[,i]),ylab="",xlab="Vole density (#/hectare)",
       cex.lab=1.75,cex.axis=1.25,bty="l",pch=19)
  title(ylab=ylabels[i],cex.lab=1.75)
  lmVTN<-lm(newsrdat$vole_den~I(volecpue[,i]/nightscpue[,i]))
  # abline(lmMTN)
  Vole100n.rsquared[i]<-summary(lmVTN)$r.squared
}
dev.off()

ylabels<-c("Chipmunk captures per 100 trap nights \n in 1 caputre event",
           "Chipmunk captures per 100 trap nights \n in 2 caputre events",
           "Chipmunk captures per 100 trap nights \n in 3 caputre events",
           "Chipmunk captures per 100 trap nights \n in 4 caputre events",
           "Chipmunk captures per 100 trap nights \n in 5 caputre events",
           "Chipmunk captures per 100 trap nights \n in 6 caputre events",
           "Chipmunk captures per 100 trap nights \n in 7 caputre events")

chipcpue <-with(newsrdat,as.data.frame(cbind(chipCPUE_1,chipCPUE_2,chipCPUE_3,chipCPUE_4,chipCPUE_5,chipCPUE_6,chipCPUE_7)))
nightscpue <- with(newsrdat,as.data.frame(cbind(nights_1,nights_2,nights_3,nights_4,nights_5,nights_6,nights_7)))

pdf("Chip100.pdf")
Chip100n.rsquared<-vector(length=7)
par(mar=c(5,7,2,2))
for(i in 1:length(chipcpue)){
  plot(newsrdat$chip_den,I(chipcpue[,i]/nightscpue[,i]),ylab="",xlab="Chipmunk density (#/hectare)",
       cex.lab=1.75,cex.axis=1.25,bty="l",pch=19)
  title(ylab=ylabels[i],cex.lab=1.75)
  lmCTN<-lm(newsrdat$chip_den~I(chipcpue[,i]/nightscpue[,i]))
  # abline(lmCTN)
  Chip100n.rsquared[i]<-summary(lmCTN)$r.squared
}
dev.off()

ylabels<-c("Total captures per 100 trap nights \n in 1 caputre event",
           "Total captures per 100 trap nights \n in 2 caputre events",
           "Total captures per 100 trap nights \n in 3 caputre events",
           "Total captures per 100 trap nights \n in 4 caputre events",
           "Total captures per 100 trap nights \n in 5 caputre events",
           "Total captures per 100 trap nights \n in 6 caputre events",
           "Total captures per 100 trap nights \n in 7 caputre events")

totalcpue <-with(newsrdat,as.data.frame(cbind(totCPUE_1,totCPUE_2,totCPUE_3,totCPUE_4,totCPUE_5,totCPUE_6,totCPUE_7)))
nightscpue <- with(newsrdat,as.data.frame(cbind(nights_1,nights_2,nights_3,nights_4,nights_5,nights_6,nights_7)))

pdf("Total100.pdf")
Total100n.rsquared<-vector(length=7)
par(mar=c(5,7,2,2))
for(i in 1:length(totalcpue)){
  plot(newsrdat$total_den,I(totalcpue[,i]/nightscpue[,i]),ylab="",xlab="Total density (#/hectare)",
       cex.lab=1.75,cex.axis=1.25,bty="l",pch=19)
  title(ylab=ylabels[i],cex.lab=1.75)
  lmTTN<-lm(newsrdat$total_den~I(totalcpue[,i]/nightscpue[,i]))
  # abline(lmMTN)
  Total100n.rsquared[i]<-summary(lmTTN)$r.squared
}
dev.off()


Mouse100n.rsquared
Vole100n.rsquared
Chip100n.rsquared
Total100n.rsquared


####CPUE Pub Figure####
par(mar=c(5,5,2,2))
par(mfrow=c(2,2))
plot(1:7,Mouse100n.rsquared,type = "b",lwd=1,xlab="Capture occasions - mice",ylab="100TN R-squared",
     cex.lab=2,cex.axis=1.5,ylim=c(0.25,1),bty="l",yaxt="n",pch=19)
text(x=1,y=1,labels="a",cex=1.5)
axis(side=2,at=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),labels=c('',"0.4",'','0.6','','0.8','','1.0'),cex.axis=1.5,las=2)

plot(1:7,Vole100n.rsquared,type = "b",lwd=1,xlab="Capture occasions - voles",ylab="100TN R-squared",
     cex.lab=2,cex.axis=1.5,ylim=c(0.25,1),bty="l",yaxt="n",pch=19)
text(x=1,y=1,labels="b",cex=1.5)
axis(side=2,at=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),labels=c('',"0.4",'','0.6','','0.8','','1.0'),cex.axis=1.5,las=2)
plot(1:7,Chip100n.rsquared,type = "b",lwd=1,xlab="Capture occasions - chipmunks",ylab="100TN R-squared",
     cex.lab=2,cex.axis=1.5,ylim=c(0.25,1),bty="l",yaxt="n",pch=19)
text(x=1,y=1,labels="c",cex=1.5)
axis(side=2,at=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),labels=c('',"0.4",'','0.6','','0.8','','1.0'),cex.axis=1.5,las=2)
plot(1:7,Total100n.rsquared,type = "b",lwd=1,xlab="Capture occasions - total",ylab="100TN R-squared",
     cex.lab=2,cex.axis=1.5,ylim=c(0.25,1),bty="l",yaxt="n",pch=19)
text(x=1,y=1,labels="d",cex=1.5)
axis(side=2,at=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),labels=c('',"0.4",'','0.6','','0.8','','1.0'),cex.axis=1.5,las=2)

####PROP DET PUB Figure####

par(mar=c(7,7,2,2))
par(mfrow=c(2,2))
plot(newsrdat$MouseProp2,newsrdat$mouse_den,pch=19,cex.lab=2,cex.axis=1.5,bty="l",yaxt = "n",
     xlab="",ylab="Mouse density (#/hectare)",ylim = c(min(newsrdat$mouse_low)-0.5,max(newsrdat$mouse_up)+0.5))
axis(side = 2,at=1:8,labels = c("1","2","3","4","5","6","7","8"),cex.axis=1.5,las=2)
arrows(x0 = newsrdat$MouseProp2,y0=newsrdat$mouse_low,x1=newsrdat$MouseProp2,y1=newsrdat$mouse_up,angle = 90,length = 0.05,code=3)
title(xlab="Proportion of cameras \n detecting mice",cex.lab=2,cex.axis=1.5,line=4.5)
abline(lm(newsrdat$mouse_den~newsrdat$MouseProp2))
text(x=0.42,y=7.5,labels = "a",cex=1.5)
r2<-round(summary(lm(newsrdat$mouse_den~newsrdat$MouseProp2))$r.squared,2)
mylabel<-bquote(italic(R)^2 == .(format(r2,digits=2)))
text(x=0.9,y=1.3,labels=mylabel,font=2,cex=1.5)

plot(newsrdat$VoleProp2,newsrdat$vole_den,pch=19,cex.lab=2,cex.axis=1.5,bty="l",yaxt = "n",
     xlab="",ylab="Vole density (#/hectare)",ylim = c(0,max(newsrdat$vole_up)+0.5))
axis(side = 2,at=0:6,labels = c("0","1","2","3","4","5","6"),cex.axis=1.5,las=2)
arrows(x0 = newsrdat$VoleProp2,y0=newsrdat$vole_low,x1=newsrdat$VoleProp2,y1=newsrdat$vole_up,angle = 90,length = 0.05,code=3)
title(xlab="Proportion of cameras \n detecting voles",cex.lab=2,cex.axis=1.5,line=4.5)
abline(lm(newsrdat$vole_den~newsrdat$VoleProp2))
text(x=0.01,y=6,labels = "b",cex=1.5)
r2<-round(summary(lm(newsrdat$vole_den~newsrdat$VoleProp2))$r.squared,2)
mylabel<-bquote(italic(R)^2 == .(format(r2,digits=2)))
text(x=0.35,y=0.1,labels=mylabel,font=2,cex=1.5)

plot(newsrdat$ChipProp2,newsrdat$chip_den,pch=19,cex.lab=2,cex.axis=1.5,bty="l",yaxt = "n",
     xlab="",ylab="Chipmunk density (#/hectare)",ylim = c(0,max(newsrdat$chip_up)+0.5))
axis(side = 2,at=0:4,labels = c("0","1","2","3","4"),cex.axis=1.5,las=2)
arrows(x0 = newsrdat$ChipProp2,y0=newsrdat$chip_low,x1=newsrdat$ChipProp2,y1=newsrdat$chip_up,angle = 90,length = 0.05,code=3)
title(xlab="Proportion of cameras \n detecting chipmunks",cex.lab=2,cex.axis=1.5,line=4.5)
abline(lm(newsrdat$chip_den~newsrdat$ChipProp2))
text(x=0.18,y=3.8,labels = "c",cex=1.5)
r2<-round(summary(lm(newsrdat$chip_den~newsrdat$ChipProp2))$r.squared,2)
mylabel<-bquote(italic(R)^2 == .(format(r2,digits=2)))
text(x=0.6,y=0.1,labels=mylabel,font=2,cex=1.5)

plot(newsrdat$TotProp2,newsrdat$total_den,pch=19,cex.lab=2,cex.axis=1.5,bty="l",yaxt = "n",
     xlab="",ylab="Total density (#/hectare)",ylim = c(min(newsrdat$total_low)-0.5,max(newsrdat$total_up)+0.5))
axis(side = 2,at=2:8,labels = c("2","3","4","5","6","7","8"),cex.axis=1.5,las=2)
arrows(x0 = newsrdat$TotProp2,y0=newsrdat$total_low,x1=newsrdat$TotProp2,y1=newsrdat$total_up,angle = 90,length = 0.05,code=3)
title(xlab="Proportion of cameras \n detecting small mammals",cex.lab=2,cex.axis=1.5,line=4.5)
abline(lm(newsrdat$total_den~newsrdat$TotProp2))
text(x=0.55,y=7.8,labels = "d",cex=1.5)
r2<-round(summary(lm(newsrdat$total_den~newsrdat$TotProp2))$r.squared,2)
mylabel<-bquote(italic(R)^2 == .(format(r2,digits=2)))
text(x=0.93,y=2.0,labels=mylabel,font=2,cex=1.5)



####Ind Det Pub Figures####

par(mar=c(7,7,2,4))
par(mfrow=c(2,2))
plot(newsrdat$MouseInd,newsrdat$mouse_den, ylab = "Mouse density (#/hectare)"  , xlab = "",yaxt="n",
     pty = "s",bty="l",cex.lab=2,cex.axis=1.5,pch=19,ylim=c(min(newsrdat$mouse_low)-0.5,max(newsrdat$mouse_up)+0.5))
axis(side = 2,at=1:8,labels = c("1","2","3","4","5","6","7","8"),cex.axis=1.5,las=2)
title(xlab="Independent photo capture events\n (per night)",cex.lab=2,cex.axis=1.5,line=4.5)
abline(lm(newsrdat$mouse_den~newsrdat$MouseInd))
arrows(x0 = newsrdat$MouseInd,y0=newsrdat$mouse_low,x1=newsrdat$MouseInd,y1=newsrdat$mouse_up,angle = 90,length = 0.05,code=3)
text(x=0.58,y=7,labels = "a",cex=1.5)
r2<-round(summary(lm(newsrdat$mouse_den~newsrdat$MouseInd))$r.squared,2)
mylabel<-bquote(italic(R)^2 == .(format(r2,digits=2)))
text(x=2.8,y=1.5,labels=mylabel,font=2,cex=1.5)

plot(newsrdat$VoleInd,newsrdat$vole_den, ylab = "Vole density (#/hectare)"  , xlab = "",yaxt="n",
     pty = "s",bty="l",cex.lab=2,cex.axis=1.5,pch=19,ylim=c(0,max(newsrdat$vole_up)+0.5))
title(xlab="Independent photo capture events\n (per night)",cex.lab=2,cex.axis=1.5,line=4.5)
axis(side = 2,at=0:6,labels = c("0","1","2","3","4","5","6"),cex.axis=1.5,las=2)
abline(lm(newsrdat$vole_den~newsrdat$VoleInd))
arrows(x0 = newsrdat$VoleInd,y0=newsrdat$vole_low,x1=newsrdat$VoleInd,y1=newsrdat$vole_up,angle = 90,length = 0.05,code=3)
text(x=0.0,y=6,labels = "b",cex=1.5)
r2<-round(summary(lm(newsrdat$vole_den~newsrdat$VoleInd))$r.squared,2)
mylabel<-bquote(italic(R)^2 == .(format(r2,digits=2)))
text(x=1.8,y=0.1,labels=mylabel,font=2,cex=1.5)

plot(newsrdat$ChipInd,newsrdat$chip_den, ylab = "Chipmunk density (#/hectare)"  , xlab = "",yaxt="n",
     pty = "s",bty="l",cex.lab=2,cex.axis=1.5,pch=19,ylim=c(0,max(newsrdat$chip_up)+0.5) )
title(xlab="Independent photo capture events\n (per night)",cex.lab=2,cex.axis=1.5,line=4.5)
abline(lm(newsrdat$chip_den~newsrdat$ChipInd))
axis(side = 2,at=0:4,labels = c("0","1","2","3","4"),cex.axis=1.5,las=2)
arrows(x0 = newsrdat$ChipInd,y0=newsrdat$chip_low,x1=newsrdat$ChipInd,y1=newsrdat$chip_up,angle = 90,length = 0.05,code=3)
text(x=0.18,y=3.8,labels = "c",cex=1.5)
r2<-round(summary(lm(newsrdat$chip_den~newsrdat$ChipInd))$r.squared,2)
mylabel<-bquote(italic(R)^2 == .(format(r2,digits=2)))
text(x=0.8,y=0.1,labels=mylabel,font=2,cex=1.5)


plot(newsrdat$TotInd,newsrdat$total_den, ylab = "Total density (#/hectare)"  , xlab = "",yaxt="n",
     pty = "s",bty="l",cex.lab=2,cex.axis=1.5,pch=19,ylim=c(min(newsrdat$total_low)-0.5,max(newsrdat$total_up)+0.5))
axis(side = 2,at=2:8,labels = c("2","3","4","5","6","7","8"),cex.axis=1.5,las=2)
arrows(x0 = newsrdat$TotInd,y0=newsrdat$total_low,x1=newsrdat$TotInd,y1=newsrdat$total_up,angle = 90,length = 0.05,code=3)
title(xlab="Independent photo capture events\n (per night)",cex.lab=2,cex.axis=1.5,line=4.5)
abline(lm(newsrdat$total_den~newsrdat$TotInd))
text(x=0.77,y=7.8,labels = "d",cex=1.5)
r2<-round(summary(lm(newsrdat$total_den~newsrdat$TotInd))$r.squared,2)
mylabel<-bquote(italic(R)^2 == .(format(r2,digits=2)))
text(x=2.25,y=2.0,labels=mylabel,font=2,cex=1.5)


