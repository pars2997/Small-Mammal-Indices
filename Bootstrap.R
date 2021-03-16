#Mouse prop detected bootstrap
head(Mouse)
allsites<-unique(Mouse$Site)
nbt=1000
site_prop<-vector()
mpropfinalres<-matrix(nrow=nbt,ncol=6)
for(j in 1:nbt){
  for (i in 1:length(unique(Mouse$Site))){
    site_temp<-allsites[i]
    tempdat<-Mouse[Mouse$Site==site_temp,]
    x<-sample(1:nrow(tempdat),nrow(tempdat),replace = TRUE)
    # x<-1:nrow(tempdat)
    propdet<-sum(tempdat$rowsum[x])/(nrow(tempdat)*days2[days2$Site==site_temp,"Nights"])
    site_prop[i]<-propdet
  }
  prop_dat<-data.frame(cbind(allsites,site_prop))
  prop_dat$site_prop<-as.numeric(prop_dat$site_prop)
  merge_dat<-merge(densitydat,prop_dat,by.x="Site",by.y="allsites")
  mod<-summary(lm(sqrt(merge_dat$mouse_den)~sqrt(merge_dat$site_prop)))
  res<-c(round(mod$coefficients[1],3),round(mod$coefficients[3],3),round(mod$coefficients[2],3)
         ,round(mod$coefficients[4],3),round(mod$r.squared,3),round(mod$coefficients[8],3))
  mpropfinalres[j,]<-res
}


#Vole prop detected bootstrap
allsites<-unique(Vole$Site)
site_prop<-vector()
vpropfinalres<-matrix(nrow=nbt,ncol=6)
for(j in 1:nbt){
  for (i in 1:length(unique(Vole$Site))){
    site_temp<-allsites[i]
    tempdat<-Vole[Vole$Site==site_temp,]
    x<-sample(1:nrow(tempdat),nrow(tempdat),replace = TRUE)
    # x<-1:nrow(tempdat)
    propdet<-sum(tempdat$rowsum[x])/(nrow(tempdat)*days2[days2$Site==site_temp,"Nights"])
    site_prop[i]<-propdet
  }
  prop_dat<-data.frame(cbind(allsites,site_prop))
  prop_dat$site_prop<-as.numeric(prop_dat$site_prop)
  merge_dat<-merge(densitydat,prop_dat,by.x="Site",by.y="allsites")
  mod<-summary(lm(sqrt(merge_dat$vole_den)~sqrt(merge_dat$site_prop)))
  res<-c(round(mod$coefficients[1],3),round(mod$coefficients[3],3),round(mod$coefficients[2],3)
         ,round(mod$coefficients[4],3),round(mod$r.squared,3),round(mod$coefficients[8],3))
  vpropfinalres[j,]<-res
}

#Chip prop detected bootstrap
allsites<-unique(Chip$Site)
site_prop<-vector()
cpropfinalres<-matrix(nrow=nbt,ncol=6)
for(j in 1:nbt){
  for (i in 1:length(unique(Chip$Site))){
    site_temp<-allsites[i]
    tempdat<-Chip[Chip$Site==site_temp,]
    x<-sample(1:nrow(tempdat),nrow(tempdat),replace = TRUE)
    # x<-1:nrow(tempdat)
    propdet<-sum(tempdat$rowsum[x])/(nrow(tempdat)*days2[days2$Site==site_temp,"Nights"])
    site_prop[i]<-propdet
  }
  prop_dat<-data.frame(cbind(allsites,site_prop))
  prop_dat$site_prop<-as.numeric(prop_dat$site_prop)
  merge_dat<-merge(densitydat,prop_dat,by.x="Site",by.y="allsites")
  mod<-summary(lm(sqrt(merge_dat$chip_den)~sqrt(merge_dat$site_prop)))
  res<-c(round(mod$coefficients[1],3),round(mod$coefficients[3],3),round(mod$coefficients[2],3)
         ,round(mod$coefficients[4],3),round(mod$r.squared,3),round(mod$coefficients[8],3))
  cpropfinalres[j,]<-res
}


head(totalrowsum) 
allsites<-unique(totalrowsum$Mouse.Site)
site_prop<-vector()
tpropfinalres<-matrix(nrow=nbt,ncol=6)
for(j in 1:nbt){
  for (i in 1:length(allsites)){
    site_temp<-allsites[i]
    tempdat<-totalrowsum[totalrowsum$Mouse.Site==site_temp,]
    x<-sample(1:nrow(tempdat),nrow(tempdat),replace = TRUE)
    # x<-1:nrow(tempdat)
    propdet<-sum(tempdat$totalrowsum[x])/(nrow(tempdat)*days2[days2$Site==site_temp,"Nights"])
    site_prop[i]<-propdet
  }
  prop_dat<-data.frame(cbind(allsites,site_prop))
  prop_dat$site_prop<-as.numeric(prop_dat$site_prop)
  merge_dat<-merge(densitydat,prop_dat,by.x="Site",by.y="allsites")
  mod<-summary(lm(sqrt(merge_dat$total_den)~sqrt(merge_dat$site_prop)))
  res<-c(round(mod$coefficients[1],3),round(mod$coefficients[3],3),round(mod$coefficients[2],3)
         ,round(mod$coefficients[4],3),round(mod$r.squared,3),round(mod$coefficients[8],3))
  tpropfinalres[j,]<-res
}

#Mouse ind det
camerasx<-names(tapply(mouseinddet$independentMouse5,mouseinddet$Station,sum))
countsx<-tapply(mouseinddet$independentMouse5,mouseinddet$Station,sum)
sitesx<-mouseinddet$Folder[which(!duplicated(mouseinddet$Station))]
mouseindet_table <- data.frame(sitesx,camerasx,countsx)
allsites<-unique(mouseindet_table$sitesx)
site_ind<-vector()
mindfinalres<-matrix(nrow=nbt,ncol=6)
for(j in 1:nbt){
  for (i in 1:length(allsites)){
    site_temp<-allsites[i]
    tempdat<-mouseindet_table[mouseindet_table$sitesx==site_temp,]
    x<-sample(1:nrow(tempdat),nrow(tempdat),replace = TRUE)
    # x<-1:nrow(tempdat)
    inddet<-sum(tempdat$countsx[x])/days[days$Site==site_temp,"Nights"]
    site_ind[i]<-inddet
  }
  ind_dat<-data.frame(allsites,site_ind)
  ind_dat$site_ind<-as.numeric(ind_dat$site_ind)
  merge_dat<-merge(densitydat,ind_dat,by.x="Site",by.y="allsites")
  mod<-summary(lm(sqrt(merge_dat$mouse_den)~sqrt(merge_dat$site_ind)))
  res<-c(round(mod$coefficients[1],3),round(mod$coefficients[3],3),round(mod$coefficients[2],3)
         ,round(mod$coefficients[4],3),round(mod$r.squared,3),round(mod$coefficients[8],3))
  mindfinalres[j,]<-res
}

#vole ind det
camerasx<-names(tapply(voleinddet$allind,voleinddet$Station,sum))
countsx<-tapply(voleinddet$allind,voleinddet$Station,sum)
sitesx<-voleinddet$Folder[which(!duplicated(voleinddet$Station))]
voleindet_table <- data.frame(sitesx,camerasx,countsx)
allsites<-unique(mouseindet_table$sitesx)
site_ind<-vector()
vindfinalres<-matrix(nrow=nbt,ncol=6)
for(j in 1:nbt){
  for (i in 1:length(allsites)){
    site_temp<-allsites[i]
    tempdat<-voleindet_table[voleindet_table$sitesx==site_temp,]
    x<-sample(1:nrow(tempdat),nrow(tempdat),replace = TRUE)
    # x<-1:nrow(tempdat)
    inddet<-sum(tempdat$countsx[x])/days[days$Site==site_temp,"Nights"]
    site_ind[i]<-inddet
  }
  site_ind[is.na(site_ind)]<-0
  ind_dat<-data.frame(allsites,site_ind)
  ind_dat$site_ind<-as.numeric(ind_dat$site_ind)
  merge_dat<-merge(densitydat,ind_dat,by.x="Site",by.y="allsites")
  mod<-summary(lm(sqrt(merge_dat$vole_den)~sqrt(merge_dat$site_ind)))
  res<-c(round(mod$coefficients[1],3),round(mod$coefficients[3],3),round(mod$coefficients[2],3)
         ,round(mod$coefficients[4],3),round(mod$r.squared,3),round(mod$coefficients[8],3))
  vindfinalres[j,]<-res
}

#chip ind det
camerasx<-names(tapply(chipindet$independentChip5,chipindet$Station,sum))
countsx<-tapply(chipindet$independentChip5,chipindet$Station,sum)
sitesx<-chipindet$Folder[which(!duplicated(chipindet$Station))]
chipindet_table <- data.frame(sitesx,camerasx,countsx)
allsites<-unique(mouseindet_table$sitesx)
site_ind<-vector()
cindfinalres<-matrix(nrow=nbt,ncol=6)
for(j in 1:nbt){
  for (i in 1:length(allsites)){
    site_temp<-allsites[i]
    tempdat<-chipindet_table[chipindet_table$sitesx==site_temp,]
    x<-sample(1:nrow(tempdat),nrow(tempdat),replace = TRUE)
    # x<-1:nrow(tempdat)
    inddet<-sum(tempdat$countsx[x])/days[days$Site==site_temp,"Nights"]
    site_ind[i]<-inddet
  }
  ind_dat<-data.frame(allsites,site_ind)
  ind_dat$site_ind<-as.numeric(ind_dat$site_ind)
  merge_dat<-merge(densitydat,ind_dat,by.x="Site",by.y="allsites")
  mod<-summary(lm(sqrt(merge_dat$chip_den)~sqrt(merge_dat$site_ind)))
  res<-c(round(mod$coefficients[1],3),round(mod$coefficients[3],3),round(mod$coefficients[2],3)
         ,round(mod$coefficients[4],3),round(mod$r.squared,3),round(mod$coefficients[8],3))
  cindfinalres[j,]<-res
}


#Total Independent Detections
camerasx<-names(tapply(mouseinddet$independentMouse20,mouseinddet$Station,sum))
countsx<-tapply(mouseinddet$independentMouse20,mouseinddet$Station,sum)
sitesx<-mouseinddet$Folder[which(!duplicated(mouseinddet$Station))]
mouseindet_table <- data.frame(sitesx,camerasx,countsx)

camerasx<-names(tapply(voleinddet$independentvole20,voleinddet$Station,sum))
countsx<-tapply(voleinddet$independentvole20,voleinddet$Station,sum)
sitesx<-voleinddet$Folder[which(!duplicated(voleinddet$Station))]
voleindet_table <- data.frame(sitesx,camerasx,countsx)

camerasx<-names(tapply(chipindet$independentChip20,chipindet$Station,sum))
countsx<-tapply(chipindet$independentChip20,chipindet$Station,sum)
sitesx<-chipindet$Folder[which(!duplicated(chipindet$Station))]
chipindet_table <- data.frame(sitesx,camerasx,countsx)
head(mouseindet_table)

mousevole<-merge(mouseindet_table,voleindet_table,by = "camerasx",all = TRUE)
all<-merge(mousevole,chipindet_table,by="camerasx",all=TRUE)
all$sitesx.x<-as.character(all$sitesx.x)
all$sitesx<-as.character(all$sitesx)
all$sitesx.y<-as.character(all$sitesx.y)
head(all)

tempsites<-vector()
for(i in 1:nrow(all)){
  if(!is.na(all$sitesx.x[i])){
    tempsites[i]<-all$sitesx.x[i]
  } else if(!is.na(all$sitesx.y[i])){
    tempsites[i]<-all$sitesx.y[i]
  } else{
    tempsites[i]<-all$sitesx[i]
  }
}
all$sites<-tempsites

all[is.na(all)]<-0
all$totalcount<-all$countsx+all$countsx.x+all$countsx.y
all<-all[,c("camerasx","sites","totalcount")]

allsites<-unique(all$sites)
site_ind<-vector()
tindfinalres<-matrix(nrow=nbt,ncol=6)
for(j in 1:nbt){
  for (i in 1:length(allsites)){
    site_temp<-allsites[i]
    tempdat<-all[all$sites==site_temp,]
    x<-sample(1:nrow(tempdat),nrow(tempdat),replace = TRUE)
    # x<-1:nrow(tempdat)
    inddet<-sum(tempdat$totalcount[x])/days[days$Site==site_temp,"Nights"]
    site_ind[i]<-inddet
  }
  ind_dat<-data.frame(allsites,site_ind)
  ind_dat$site_ind<-as.numeric(ind_dat$site_ind)
  merge_dat<-merge(densitydat,ind_dat,by.x="Site",by.y="allsites")
  mod<-summary(lm(sqrt(merge_dat$total_den)~sqrt(merge_dat$site_ind)))
  res<-c(round(mod$coefficients[1],3),round(mod$coefficients[3],3),round(mod$coefficients[2],3)
         ,round(mod$coefficients[4],3),round(mod$r.squared,3),round(mod$coefficients[8],3))
  tindfinalres[j,]<-res
}

mpropfinalres<-data.frame(mpropfinalres)
colnames(mpropfinalres)<-c("int","int_se","slope","slope_se","r2","p")
head(mpropfinalres)

vpropfinalres<-data.frame(vpropfinalres)
colnames(vpropfinalres)<-c("int","int_se","slope","slope_se","r2","p")
head(vpropfinalres)

cpropfinalres<-data.frame(cpropfinalres)
colnames(cpropfinalres)<-c("int","int_se","slope","slope_se","r2","p")
head(cpropfinalres)

tpropfinalres<-data.frame(tpropfinalres)
colnames(tpropfinalres)<-c("int","int_se","slope","slope_se","r2","p")
head(tpropfinalres)

mindfinalres<-data.frame(mindfinalres)
colnames(mindfinalres)<-c("int","int_se","slope","slope_se","r2","p")
head(mindfinalres)

vindfinalres<-data.frame(vindfinalres)
colnames(vindfinalres)<-c("int","int_se","slope","slope_se","r2","p")
head(vindfinalres)

cindfinalres<-data.frame(cindfinalres)
colnames(cindfinalres)<-c("int","int_se","slope","slope_se","r2","p")
head(cindfinalres)

tindfinalres<-data.frame(tindfinalres)
colnames(tindfinalres)<-c("int","int_se","slope","slope_se","r2","p")
head(tindfinalres)

resultstab<-matrix(nrow=8,ncol=9)

resultstab[1,]<-c(mean(mpropfinalres$int),
                  quantile(mpropfinalres$int,probs = c(0.025,0.975)),
                  mean(mpropfinalres$slope),
                  quantile(mpropfinalres$slope,probs = c(0.025,0.975)),
                  mean(mpropfinalres$r2),
                  quantile(mpropfinalres$r2,probs = c(0.025,0.975)))
resultstab[2,]<-c(mean(vpropfinalres$int),
                  quantile(vpropfinalres$int,probs = c(0.025,0.975)),
                  mean(vpropfinalres$slope),
                  quantile(vpropfinalres$slope,probs = c(0.025,0.975)),
                  mean(vpropfinalres$r2),
                  quantile(vpropfinalres$r2,probs = c(0.025,0.975)))
resultstab[3,]<-c(mean(cpropfinalres$int),
                  quantile(cpropfinalres$int,probs = c(0.025,0.975)),
                  mean(cpropfinalres$slope),
                  quantile(cpropfinalres$slope,probs = c(0.025,0.975)),
                  mean(cpropfinalres$r2),
                  quantile(cpropfinalres$r2,probs = c(0.025,0.975)))
resultstab[4,]<-c(mean(tpropfinalres$int),
                  quantile(tpropfinalres$int,probs = c(0.025,0.975)),
                  mean(tpropfinalres$slope),
                  quantile(tpropfinalres$slope,probs = c(0.025,0.975)),
                  mean(tpropfinalres$r2),
                  quantile(tpropfinalres$r2,probs = c(0.025,0.975)))

resultstab[5,]<-c(mean(mindfinalres$int),
                  quantile(mindfinalres$int,probs = c(0.025,0.975)),
                  mean(mindfinalres$slope),
                  quantile(mindfinalres$slope,probs = c(0.025,0.975)),
                  mean(mindfinalres$r2),
                  quantile(mindfinalres$r2,probs = c(0.025,0.975)))
resultstab[6,]<-c(mean(vindfinalres$int),
                  quantile(vindfinalres$int,probs = c(0.025,0.975)),
                  mean(vindfinalres$slope),
                  quantile(vindfinalres$slope,probs = c(0.025,0.975)),
                  mean(vindfinalres$r2),
                  quantile(vindfinalres$r2,probs = c(0.025,0.975)))
resultstab[7,]<-c(mean(cindfinalres$int),
                  quantile(cindfinalres$int,probs = c(0.025,0.975)),
                  mean(cindfinalres$slope),
                  quantile(cindfinalres$slope,probs = c(0.025,0.975)),
                  mean(cindfinalres$r2),
                  quantile(cindfinalres$r2,probs = c(0.025,0.975)))
resultstab[8,]<-c(mean(tindfinalres$int),
                  quantile(tindfinalres$int,probs = c(0.025,0.975)),
                  mean(tindfinalres$slope),
                  quantile(tindfinalres$slope,probs = c(0.025,0.975)),
                  mean(tindfinalres$r2),
                  quantile(tindfinalres$r2,probs = c(0.025,0.975)))
resultstab<-data.frame(resultstab)
resultstab$mod<-c("mprop","vprop","cprop","tprop","mind","vind","cind","tind")
colnames(resultstab)<-c("Intercept","Upper.CI","Lower.CI","Slope","Upper.CI","Lower.CI","R2","Upper.CI","Lower.CI","Mod")
resultstab
write.csv(resultstab,file="Bootstrapresults.csv")
