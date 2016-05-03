################################################################################
###                 Code to reproduce all results from 
# Estimation of carbon stocks in Colombian mangroves at national level  #
# by Bolivar and Sierra
# Submitted to Ocean and Coastal Management
################################################################################


#Set working directory. Set this to the directory where you downloded the supplementary material
setwd("~/TEE/Manuscripts/Mangroves/")
setwd("~/SOIL-R/Manuscripts/Mangroves/")
setwd("C:/Users/PROJECT2/AGBMangrove/")

##Estimation of C stocks in AGB from available data

#Mangrove area with uncertainty

#Area Caribbean in ha
InvemarCaribbean=69894.02
Giri1Caribbean=47064.78
Giri2Caribbean=13846.07
x=c(InvemarCaribbean,Giri1Caribbean,Giri2Caribbean)
meanAreaCaribbean=mean(x)
sdAreaCaribbean=sd(x)
seAreaCaribbean=sdAreaCaribbean/sqrt(length(x))

#Area Pacific in ha
InvemarPacific=239239.20
Giri1Pacific=165563.6
Giri2Pacific=144882.74
y=c(InvemarPacific, Giri1Pacific,Giri2Pacific)
meanAreaPacific=mean(y)
sdAreaPacific=sd(y)
seAreaPacific=sdAreaPacific/sqrt(length(y))

#Area Total Colombia in ha
InvemarTotal=300133.22
Giri1Total=212628.4
Giri2Total=158728.21
FAOTotal=371250
HutchisonTotal=410152
z=c(InvemarTotal,Giri1Total,Giri2Total,FAOTotal,HutchisonTotal)
meanAreaTotal=mean(z)
sdAreaTotal=sd(z)
seAreaTotal=sdAreaTotal/sqrt(length(z))

n=1000 #Number of random deviates to sample from a given probability distribution

areaCaribbean=rnorm(n,mean=meanAreaCaribbean,sd=seAreaCaribbean) #Create n random deviates form a Normal distribution
# hist(areaCaribbean)
# abline(v=meanAreaCaribbean)

areaPacific=rnorm(n,mean=meanAreaPacific,sd=seAreaPacific)
# hist(areaPacific)
# abline(v=meanAreaPacific)

areaTotal=rnorm(n,mean=meanAreaTotal,sd=seAreaTotal)
# hist(areaTotal)
# abline(v=meanAreaTotal)

#AGB density Caribbean
AGBC=plots[plots[,5]=="A",3]
AGBC=AGBC[!is.na(AGBC)]
meanAGBCaribbean=mean(AGBC)
sdAGBCaribbean=sd(AGBC)
nCaribbean=length(AGBC)
seAGBCaribbean=sdAGBCaribbean/sqrt(nCaribbean)

#AGB density Pacific
AGBP=plots[plots[,5]=="P",3]
AGBP=AGBP[!is.na(AGBP)]
meanAGBPacific=mean(AGBP)
sdAGBPacific=sd(AGBP)
nPacific=length(AGBP)
seAGBPacific=sdAGBPacific/sqrt(nPacific)

#Mean comparison AGB Caribbean vs. AGB Pacific
t.test(plots[plots[,5]=="A",3],y=plots[plots[,5]=="P",3],na.rm=TRUE,conf.level = 0.95)

#AGB Total Colombia
AGBT=plots[,3]
AGBT=AGBT[!is.na(AGBT)]
meanAGBTotal=mean(AGBT)
sdAGBTotal=sd(AGBT)
nTotal=length(AGBT)
seAGBTotal=sdAGBTotal/sqrt(nTotal)

#n random deviates for AGB in the Caribbean
rAGBC=rnorm(n,mean=meanAGBCaribbean,sd=seAGBCaribbean)

totAGBC=rAGBC*areaCaribbean/1000000 #Multiply by area and express in Tg C (divide by 10^6)
# hist(totAGBC,xlab="Total aboveground biomass (Tg DW)")
# hist(totAGBC*0.5,xlab="Total aboveground biomass (Tg C)")

quantile(totAGBC*0.5,probs=c(0.1,0.9))
mean(totAGBC*0.5)
sd(totAGBC*0.5)

#n random deviates for AGB in the Pacific
rAGBP=rnorm(n,mean=meanAGBPacific,sd=seAGBPacific)

totAGBP=rAGBP*areaPacific/1000000 #Multiply by area and express in Tg C (divide by 10^6)
# hist(totAGBP,xlab="Total aboveground biomass (Tg DW)")
# hist(totAGBP*0.5,xlab="Total aboveground biomass (Tg C)")

quantile(totAGBP*0.5,probs=c(0.1,0.9)) 
mean(totAGBP*0.5)
sd(totAGBP*0.5)


#n random deviates for AGB in Colombia
rAGBT=rnorm(n,mean=meanAGBTotal,sd=seAGBTotal)

totAGBT=rAGBT*areaTotal/1000000 #Multiply by area and express in Tg C (divide by 10^6)
# hist(totAGBT,xlab="Total aboveground biomass (Tg DW)")
# abline(v=mean(totAGBT,na.rm=TRUE))
# hist(totAGBT*0.5,xlab="Total aboveground biomass (Tg C)")

quantile(totAGBT*0.5,probs=c(0.1,0.9)) 
mean(totAGBT*0.5)
sd(totAGBT*0.5)


#Figure 1
#pdf(file="Manuscript/Figures/Cdensity_and_TotalCstock.pdf")
par(mfrow=c(2,1), mar=c(4,4.5,1,1))
boxplot(AGBC*0.5,AGBP*0.5,AGBT*0.5,names=c("Caribbean","Pacific","Colombia"),ylab=expression(paste("AGB density (Mg C h", a^-1,")")))

boxplot(totAGBC*0.5,totAGBP*0.5,totAGBT*0.5,names=c("Caribbean","Pacific","Colombia"),ylab="Total AGB (Tg C)")
par(mfrow=c(1,1))
#dev.off()

##################################################################################################
#Sensitivity analysis of uncertainty

unlevels=c(0,0.05,0.1,0.15,0.2,0.3,0.5,1)

#Area Random Matrix (random deviates for each uncertainty level) for Caribbean
AreaRMC=matrix(0,nrow=n,ncol=length(unlevels))

for(i in 1:length(unlevels)){
  AreaRMC[,i]=rnorm(n,mean=meanAreaCaribbean,sd=meanAreaCaribbean*unlevels[i])
}

#Area Random Matrix (random deviates for each uncertainty level) for Pacific
AreaRMP=matrix(0,nrow=n,ncol=length(unlevels))

for(i in 1:length(unlevels)){
  AreaRMP[,i]=rnorm(n,mean=meanAreaPacific,sd=meanAreaPacific*unlevels[i])
}

#Area Random Matrix (random deviates for each uncertainty level) for Colombia
AreaRMT=matrix(0,nrow=n,ncol=length(unlevels))

for(i in 1:length(unlevels)){
  AreaRMT[,i]=rnorm(n,mean=meanAreaTotal,sd=meanAreaTotal*unlevels[i])
}

# Area Caribbean
sdAreaCaribbean_0=meanAreaCaribbean*0 #uncertainty=0%
areaCaribbean_0=rnorm(n,mean=meanAreaCaribbean,sd=sdAreaCaribbean_0)
# 
sdAreaCaribbean_5=meanAreaCaribbean*0.05 #uncertainty=5%
areaCaribbean_5=rnorm(n,mean=meanAreaCaribbean,sd=sdAreaCaribbean_5)


#Biomass Caribbean
#AGB Random Matrix (random deviates for each uncertainty level) for Caribbean
agbRMC=matrix(0,nrow=n,ncol=length(unlevels))

for(i in 1:length(unlevels)){
  agbRMC[,i]=rnorm(n,mean=meanAGBCaribbean,sd=meanAGBCaribbean*unlevels[i])
}

#AGB Random Matrix (random deviates for each uncertainty level) for Pacific
agbRMP=matrix(0,nrow=n,ncol=length(unlevels))

for(i in 1:length(unlevels)){
  agbRMP[,i]=rnorm(n,mean=meanAGBPacific,sd=meanAGBPacific*unlevels[i])
}

#AGB Random Matrix (random deviates for each uncertainty level) for Colombia
agbRMT=matrix(0,nrow=n,ncol=length(unlevels))

for(i in 1:length(unlevels)){
  agbRMT[,i]=rnorm(n,mean=meanAGBTotal,sd=meanAGBTotal*unlevels[i])
}


sdAGBCaribbean_0=meanAGBCaribbean*0 #uncertainty=0%
AGBC_0=rnorm(n,mean=meanAGBCaribbean,sd=sdAGBCaribbean_0)
# 
sdAGBCaribbean_5=meanAGBCaribbean*0.05 #uncertainty=5%
AGBC_5=rnorm(n,mean=meanAGBCaribbean,sd=sdAGBCaribbean_5)

#Interactions Area vs. Biomass 

# Function to calculate coefficient of variation
CV=function(x){
  sd(x)/mean(x)
}


###### Caribbean coast
#Matrix to fill up with combinations of uncertainties for area and AGB density 
sensUnC=matrix(1,nrow=length(unlevels),ncol=length(unlevels))

#For loop to fill up the matrix. Columns correspond to uncertainties related to areas and rows to uncertainties related to AGB density
for(j in 1:length(unlevels)){
  tmp=AreaRMC[,j]*agbRMC
  sensUnC[,j]=apply(tmp,MARGIN = 2,FUN=CV)
}

round(sensUnC,2) # rounded to 2 decimal places


###### Pacific coast
#Matrix to fill up with combinations of uncertainties for area and AGB density 
sensUnP=matrix(1,nrow=length(unlevels),ncol=length(unlevels))

#For loop to fill up the matrix. Columns correspond to uncertainties related to areas and rows to uncertainties related to AGB density
for(j in 1:length(unlevels)){
  tmp=AreaRMP[,j]*agbRMP
  sensUnP[,j]=apply(tmp,MARGIN = 2,FUN=CV)
}

round(sensUnP,2) # rounded to 2 decimal places


###### Colombia
#Matrix to fill up with combinations of uncertainties for area and AGB density 
sensUnT=matrix(1,nrow=length(unlevels),ncol=length(unlevels))

#For loop to fill up the matrix. Columns correspond to uncertainties related to areas and rows to uncertainties related to AGB density
for(j in 1:length(unlevels)){
  tmp=AreaRMT[,j]*agbRMT
  sensUnT[,j]=apply(tmp,MARGIN = 2,FUN=CV)
}

round(sensUnT,2) # rounded to 2 decimal places


### Simulation number of necessary estimations to achive an specific level of uncertainty (10%)
##Caribbean
AGBSamples=1:200
seAGBCarSimul=sdAGBCaribbean/sqrt(AGBSamples)
CV_AGB_Caribbean=seAGBCarSimul/meanAGBCaribbean


##Pacific
seAGBPacSimul=sdAGBPacific/sqrt(AGBSamples)
CV_AGB_Pacific=seAGBPacSimul/meanAGBPacific


##Colombia
seAGBColSimul=sdAGBTotal/sqrt(AGBSamples)
CV_AGB_Total=seAGBColSimul/meanAGBTotal


##Figure 2
#pdf(file="Manuscript/Figures/AGBsamplingEffort.pdf")
par(mar=c(4,4,2,1))
plot(AGBSamples,CV_AGB_Caribbean,type="l",xlab="Number of sampling units",ylab="Coefficient of variation of AGB density",ylim=c(0,0.6),xaxs="i",yaxs="i")
abline(h=0.05,lty=2)
abline(h=0.1,lty=2)
lines(AGBSamples,CV_AGB_Pacific,type="l", lty=1, lwd = 1, col=2)
lines(AGBSamples,CV_AGB_Total,type="l", lty=1, lwd = 1, col=3)
legend("topright",c("Caribbean","Pacific", "Colombia"),lty=1,col=c(1,2,3),bty="n")
#dev.off()

##number of total samples necessary

#Caribbean
min(AGBSamples[which(round(CV_AGB_Caribbean,2)==0.05)])
min(AGBSamples[which(round(CV_AGB_Caribbean,2)==0.1)])

#Pacific
min(AGBSamples[which(round(CV_AGB_Pacific,2)==0.05)])
min(AGBSamples[which(round(CV_AGB_Pacific,2)==0.1)])

#Colombia
min(AGBSamples[which(round(CV_AGB_Colombia,2)==0.05)])
min(AGBSamples[which(round(CV_AGB_Colombia,2)==0.1)])

################################################################################

##Extract data from WorldClim for AGB data available for Colombia

# Load required packages
install.packages("raster")
library(raster)

#Extract data from Worldclim
w = getData('worldclim', var='bio', res=0.5, lon=-75, lat=10) #extract data from bioclimatic variables, with a res=0.5 (minutes of a degree). For res=0.5 it is necessary to provide a lon and lat for the tile which include the study area.

# Import data with coordinates, biomass data, and soil carbon for all available plots and sites
#plots<-read.table("All plots.txt",header=TRUE,dec=".")
plots<-read.table("C:/Users/PROJECT2/Documents/GitHub/ManVic/All plots.txt",header=TRUE,dec=".")

Tab=data.frame(X=-1*plots[,1], Y=plots[,2]) #Extract only the coordinates and transform relative to GMZ

# #Introduce coordinates and transform them to spatial object
coordinates(Tab)<-~X+Y
 
# #Extract climate data for specific coordinates
dat<-extract(w,Tab)
#dat

#####################################################################################
#Create function to estimate AGB density using Twilley et al. (1992) Model
TwilleyModel=function(lat){
  -7.291*lat+298.5
}

# Predict ABG density for all sites
AGB1=TwilleyModel(Tab$Y)

#Create function to estimate AGB density using Hutchinson et al. (2014) Model 
HutchisonModel=function(BIO10,BIO11,BIO16,BIO17){
  0.295*BIO10+0.658*BIO11+0.0234*BIO16+0.195*BIO17-120.3
}

#Predict AGB density for all sites with this model
AGB2=HutchisonModel(BIO10=dat[,10],BIO11=dat[,11],BIO16=dat[,16],BIO17=dat[,17])

############################################################################################
#Observed and predicted AGB density using previous models

#Figure 3
#pdf(file="Manuscript/Figures/GlobalModelsvs.pdf") #Uncomment to save figure as pdf
par(mfrow=c(2,1),mar=c(4,4.5,1,1))
plot(plots$AGB,AGB1,xlim=c(0,300),ylim=c(0,300),pch=20,xlab=expression(paste("Observed AGB density (Mg h",a^-1,")")),
     ylab=expression(paste("Predicted AGB density (Mg h",a^-1,")")))
points(plots$AGB,AGB2,pch=20,col=2)
abline(0,1)
abline(v=mean(plots$AGB,na.rm=TRUE),lty=2)
legend("bottomright",c("Twilley model","Hutchinson model"),pch=20,col=c(1,2),bty="n")

boxplot(plots$AGB,AGB1,AGB2,names=c("Observed","Twilley","Hutchinson"),ylab=expression(paste("AGB density (Mg h", a^-1, ")")))
par(mfrow=c(1,1))
#dev.off()

# #############################################################################################
# #Estimation of Soil OC using Jardine and Siikam??ki (2014) Model (Parametric for Csoil 1 m) (mg/cm3)
# JardineModel=function(lat,BIO1,BIO11,BIO12,BIO15){
#   0.150*abs(lat)+12.210*(BIO1/10)-0.389*(BIO1/10)^2+5.399*(BIO11/10)-0.006*BIO12-0.101*BIO15-141.921
# }
# 
# JardineModel(lat=Tab$Y, BIO1=dat[,1],BIO11=dat[,11],BIO12=dat[,12],BIO15=dat[,15])
# 
# BGB1=JardineModel(lat=Tab$Y, BIO1=dat[,1],BIO11=dat[,11],BIO12=dat[,12],BIO15=dat[,15])
# 
# ###############################
# #JardineModel (Parametric for Csoil 1 m) with original data (test)
# SA<-read.table("~/TEE/Manuscripts/Mangroves/Jardine_SA.txt",header=T,dec=".")
# SA
# 
# JardineModel(lat=SA$Y, BIO1=SA$BIO1,BIO11=SA$BIO11,BIO12=SA$BIO12,BIO15=SA$BIO15)
# 
# BGB2=JardineModel(lat=SA$Y, BIO1=SA$BIO1,BIO11=SA$BIO11,BIO12=SA$BIO12,BIO15=SA$BIO15)
# 
# ################################################################################################
# #Graphic omparison between estimations made with Jardine model
# boxplot(plots$Csoil,BGB1*10,BGB2*10,names=c("Colombia-Plots","Jardine-Colombia","Jardine-Paper"),
#         ylab="Soil Carbon (Mg C ha-1)",border=1:3)
# abline(h=mean(plots$Csoil,na.rm=TRUE))
# abline(h=mean(BGB1*10,na.rm=TRUE),col=2)
# abline(h=mean(BGB2*10,na.rm=TRUE),col=3)
# 
# plot(plots$Csoil,BGB1*10,pch=19,xlim=c(0,2000),ylim=c(0,2000),xlab="Observations",ylab="Predictions",
#      main="Soil C to 1 m")
# abline(1,1)

###################################################################################################

##Extract data from MODIS images

#Install and load required package
install.packages("MODISTools")
library (MODISTools)

AGBC[!is.na(AGBC)]

#Format the data
#modis.subset <- read.table("coordMODIS.txt",header=TRUE,dec=".")
modis.subset=data.frame(long=-1*plots[!is.na(plots[,3]),1],lat=plots[!is.na(plots[,3]),2])

names(modis.subset) 
is.numeric(modis.subset$lat)
is.numeric(modis.subset$long)

#Time-series of MODIS data
modis.subset$start.date <- rep(2011, nrow(modis.subset))
modis.subset$end.date <- rep(2013, nrow(modis.subset))

#Specifing a subset request
GetProducts()

#Listing available bands for MOD13Q1 product
GetBands(Product = "MOD13Q1")

#Checking the time-series of MODIS data we want is available for this data product
GetDates(Product = "MOD13Q1", Lat = modis.subset$lat[1], Long = modis.subset$long[1])

#Downloading information EVI
#using sourrounding pixels
MODISSubsets(LoadDat = modis.subset, Products = "MOD13Q1", Bands = c("250m_16_days_EVI", "250m_16_days_pixel_reliability"), Size = c(1,1))
subset.string <- read.csv(list.files(pattern = ".asc")[1], header = FALSE, as.is = TRUE)
subset.string[1, ]

#only for the focal pixel
#MODISSubsets(LoadDat = modis.subset, Products = "MOD13Q1", Bands = c("250m_16_days_EVI", "250m_16_days_pixel_reliability"), Size = c(0,0))
#subset.string <- read.csv(list.files(pattern = ".asc")[1], header = FALSE, as.is = TRUE)
#subset.string[1, ]

#Downloading information NDVI
##using sourrounding pixels
#MODISSubsets(LoadDat = modis.subset, Products = "MOD13Q1", Bands = c("250m_16_days_NDVI", "250m_16_days_pixel_reliability"), Size = c(1,1))
#subset.string <- read.csv(list.files(pattern = ".asc")[1], header = FALSE, as.is = TRUE)
#subset.string[1, ]

#only for the focal pixel
#MODISSubsets(LoadDat = modis.subset, Products = "MOD13Q1", Bands = c("250m_16_days_NDVI", "250m_16_days_pixel_reliability"), Size = c(0,0))
#subset.string <- read.csv(list.files(pattern = ".asc")[1], header = FALSE, as.is = TRUE)
#subset.string[1, ]

#Finding average each pixel over time, to produce one tile of mean EVI pixels at each subset location
MODISSummaries(LoadDat = modis.subset, Product = "MOD13Q1", Bands = "250m_16_days_EVI", ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001, QualityScreen = TRUE, QualityBand = "250m_16_days_pixel_reliability", QualityThreshold = 0)

#MODISSummaries(LoadDat = modis.subset, Product = "MOD13Q1", Bands = "250m_16_days_NDVI", ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001, QualityScreen = TRUE, QualityBand = "250m_16_days_pixel_reliability", QualityThreshold = 0)

#Calculating EVI mean for each coordinate (mean of 81 pixels)
#Mod1=read.csv("MODIS_Data_MOD13Q1_2016-02-09_h17-m3-s20.csv") 
#head(Mod1)

#meanMod1=data.frame(Mod1[,1:2],meanEVI=apply(Mod1[,6:86],1,mean,na.rm=TRUE))
#meanMod1
#boxplot(meanMod1[,3])
#dim(meanMod1)

#Calculating NDVI mean for each coordinate (mean of 81 pixels)
#Mod2=read.csv("MODIS_Data_MOD13Q1_2015-06-23_h12-m0-s60.csv")

#meanMod2=data.frame(Mod1[,1:2],meanNDVI=apply(Mod1[,6:86],1,mean,na.rm=TRUE))
#meanMod2

# Import EVI data Victor OJO ACA ME TIENEN QUE QUEDAR 46 DATOS
NewEVI<-read.table("C:/Users/PROJECT2/Documents/GitHub/ManVic/MeanEVIPlots.txt",header=TRUE,dec=".")
head(NewEVI)
NewEVI46=NewEVI[!is.na(NewEVI[,4]),]
dim(NewEVI46)


######################################################################################
#Extract data from Worldclim only for 46 plots with AGB data
w = getData('worldclim', var='bio', res=0.5, lon=-75, lat=10) #extract data from bioclimatic variables, with a res=0.5 (minutes of a degree). For res=0.5 it is necessary to provide a lon and lat for the tile which include the study area.

# Select from plots locations with AGB data
plots46=plots[!is.na(plots[,3]),]
dim(plots46)
head(plots46)

Tab46=data.frame(X=-1*plots46[,1], Y=plots46[,2]) #Extract only the coordinates and transform relative to GMZ

# #Introduce coordinates and transform them to spatial object
coordinates(Tab46)<-~X+Y

# #Extract climate data for specific coordinates
dat46<-extract(w,Tab46)
#dat46
dim(dat46)

#########################################################################################
#Conforming an only data base with information of bioclimatic variables, EVI and AGB data for Colombia
DataSet=cbind(plots46,EVI=meanMod1[,3],dat46) #plots46 (data base with AGB data for Colombian mangroves), dat46 (data base with bioclimatic variables downloaded from WorldClim)
dim(DataSet)
head(DataSet)

DataSetVic=cbind(plots46,EVI=NewEVI[,4],dat46) #plots46 (data base with AGB data for Colombian mangroves), dat46 (data base with bioclimatic variables downloaded from WorldClim)

##############################################################################

##Regression analysis

#Install and load required package
install.packages("lmtest")
library(lmtest)

#Import table with data
# DataSet<-read.table("~/TEE/Manuscripts/Mangroves/DataSetAGB.txt",header=T,dec=".")
# DataSet

summary (DataSet)

#Confirm data are numeric
#names(DataSet) 
#is.numeric(DataSet$X)
#is.numeric(DataSet$Y)
#is.numeric(DataSet$AGB)
#is.numeric(DataSet$EVI)
#is.numeric(DataSet$bio1_23)

#Twilley AGB model parametrization
Twilleymodel=lm(AGB~Y, data=DataSet)
Twilleymodel
summary(Twilleymodel)
AIC(Twilleymodel)

#Hutchison AGB model parametrization
Hutchisonmodel=lm(AGB~bio10_23+bio11_23+bio16_23+bio17_23, data=DataSet)
Hutchisonmodel
summary(Hutchisonmodel)

#New models

##Model1 (extreme climatic data + EVI)
Model1=lm(AGB~bio10_23+bio11_23+bio16_23+bio17_23+EVI, data=DataSet)
Model1
summary(Model1)
summary.aov(Model1)

#Model2 (mean climatic data)
Model2=lm(AGB~bio1_23+bio4_23+bio12_23+bio15_23,data=DataSet)
Model2
summary(Model2)
summary.aov(Model2)

#Model3 (mean climatic data + EVI)
Model3=lm(AGB~bio1_23+bio4_23+bio12_23+bio15_23+EVI,data=DataSet)
Model3
summary(Model3)
summary.aov(Model3)

#Model4 (extreme climatic data + EVI + latitude)
Model4=lm(AGB~Y+bio10_23+bio11_23+bio16_23+bio17_23+EVI, data=DataSet)
Model4
summary(Model4)
summary.aov(Model4)

#Model5 (extreme climatic data + latitude)
Model5=lm(AGB~bio10_23+bio11_23+bio16_23+bio17_23+Y, data=DataSet)
Model5
summary(Model5)
summary.aov(Model5)

#Model6 (mean climatic data + latitude)
Model6=lm(AGB~Y+bio1_23+bio4_23+bio12_23+bio15_23,data=DataSet)
Model6
summary(Model6)
summary.aov(Model6)

#Model7 (mean climatic data + EVI + latitude)
Model7=lm(AGB~Y+bio1_23+bio4_23+bio12_23+bio15_23+EVI,data=DataSet)
Model7
summary(Model7)
summary.aov(Model7)

#Model8 (jardine variables)
a=abs(DataSet$Y)
b=(DataSet$bio1_23/10)
c=(DataSet$bio1_23/10)^2
d=(DataSet$bio11_23/10)
e=(DataSet$bio12_23)
f=(DataSet$bio15_23)
AGB=(DataSet$AGB)
Model8=lm(AGB~a+b+c+d+e+f)
Model8
summary(Model8)
summary.aov(Model8)

#Model9 (jardine variables + EVI)
a=abs(DataSet$Y)
b=(DataSet$bio1_23/10)
c=(DataSet$bio1_23/10)^2
d=(DataSet$bio11_23/10)
e=(DataSet$bio12_23)
f=(DataSet$bio15_23)
g=(DataSet$EVI)
AGB=(DataSet$AGB)
Model9=lm(AGB~a+b+c+d+e+f+g)
Model9
summary(Model9)
summary.aov(Model9)

#Model10 (precipitation and temperature associated to driest quarter)
Model10=lm(AGB~bio9_23+bio17_23,data=DataSet)
Model10
summary(Model10)
summary.aov(Model10)

#Model11 (precipitation and temperature associated to driest quarter + latitude)
Model11=lm(AGB~Y+bio9_23+bio17_23,data=DataSet)
Model11
summary(Model11)
summary.aov(Model11)

#Model12 (precipitation and temperature associated to driest quarter + latitude + EVI)
Model12=lm(AGB~Y+bio9_23+bio17_23+EVI,data=DataSet)
Model12
summary(Model12)
summary.aov(Model12)

#Model13 (temperature associated to driest quarter + latitude + EVI)
Model13=lm(AGB~Y+bio9_23+EVI,data=DataSet)
Model13
summary(Model13)
summary.aov(Model13)

#Model14 (temperature associated to driest quarter +  EVI)
Model14=lm(AGB~bio9_23+EVI,data=DataSet)
Model14
summary(Model14)
summary.aov(Model14)

#Model15 (temperature associated to driest quarter + latitude)
Model15=lm(AGB~bio9_23+Y,data=DataSet)
Model15
summary(Model15)
summary.aov(Model15)

#Model16 (temperature associated to driest quarter)
Model16=lm(AGB~bio9_23,data=DataSet)
Model16
summary(Model16)
summary.aov(Model16)

#Model17 
Model17=lm(AGB~bio11_23+bio16_23+bio9_23+EVI,data=DataSet)
Model17
summary(Model17)
summary.aov(Model17)
plot(Model17)

##T test (Obs vs. Est.)
exp(fitted(Model17))
t.test(exp(fitted(Model17)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model17))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model17) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model17)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model17)
significants<-cook>1
significants

#Model18 (Model1 only with significant variables + EVI)
Model18=lm(AGB~bio11_23+bio16_23+EVI,data=DataSet)
Model18
summary(Model18)
summary.aov(Model18)

#Model19 (Hutchison parametrized only with significant variables)
Model19=lm(AGB~bio11_23+bio16_23+bio17_23,data=DataSet)
Model19
summary(Model19)
summary.aov(Model19)

#Model20 (Model17 + annual mean Temperature)
Model20=lm(AGB~bio11_23+bio16_23+bio9_23+bio1_23+EVI,data=DataSet)
Model20
summary(Model20)
summary.aov(Model20)

#Model21 (Model17 + annual mean Temperature)
a=(DataSet$bio11_23/10)
b=(DataSet$bio9_23/10)
c=(DataSet$bio1_23/10)
d=(DataSet$bio1_23/10)^2
Model21=lm(AGB~a+bio16_23+b+c+d+EVI,data=DataSet)
Model21
summary(Model21)
summary.aov(Model21)
plot(Model21)

##T test (Obs vs. Est.)
exp(fitted(Model21))
t.test(exp(fitted(Model21)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model21))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model21) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model21)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model21)
significants<-cook>1
significants

#Model22 (all variables Model1^2)
a=(DataSet$bio10_23^2)
b=(DataSet$bio11_23^2)
c=(DataSet$bio16_23^2)
d=(DataSet$bio17_23^2)
e=(DataSet$EVI^2)
Model22=lm(AGB~a+b+c+d+e, data=DataSet)
Model22
summary(Model22)
summary.aov(Model22)

#Model23 (lnModel1 and lnAGB)
a=log(DataSet$AGB)
b=log(DataSet$bio10_23)
c=log(DataSet$bio11_23)
d=log(DataSet$bio16_23)
e=log(DataSet$bio17_23)
f=log(DataSet$EVI)
Model23=lm(a~b+c+d+e+f, data=DataSet)
Model23
summary(Model23)
summary.aov(Model23)
par(mfrow = c(2,2))
plot(Model23)
summary.aov(Model23)

#Model24 (lnModel1)
a=log(DataSet$bio10_23)
b=log(DataSet$bio11_23)
c=log(DataSet$bio16_23)
d=log(DataSet$bio17_23)
e=log(DataSet$EVI)
Model24=lm(AGB~a+b+c+d+e, data=DataSet)
Model24
summary(Model24)
summary.aov(Model24)

#Model25 (lnModel1 and lnAGB)
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$bio16_23)
d=log(DataSet$EVI)
Model25=lm(a~b+c+d, data=DataSet)
Model25
summary(Model25)
summary.aov(Model25)
par(mfrow = c(2,2))
plot(Model25)

##T test (Obs vs. Est.)
exp(fitted(Model25))
t.test(exp(fitted(Model25)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model25))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model25) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model25)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model25)
significants<-cook>1
significants

#Model26 (lnModel1+ abs(lat) and lnAGB)
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$bio16_23)
d=log(DataSet$EVI)
e=abs(DataSet$Y)
Model26=lm(a~b+c+d+e, data=DataSet)
Model26
summary(Model26)
summary.aov(Model26)
par(mfrow = c(2,2))
plot(Model26)

##T test (Obs vs. Est.)
exp(fitted(Model26))
t.test(exp(fitted(Model26)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model26))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model26) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model26)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model26)
significants<-cook>1
significants

#Model27 (lnModel1 and lnAGB and lnbio11_23)
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$bio16_23)
d=log(DataSet$EVI)
e=abs(DataSet$Y)
f=log(DataSet$bio11_23)
Model27=lm(a~b+c+d+e+f, data=DataSet)
Model27
summary(Model27)
summary.aov(Model27)
par(mfrow = c(2,2))
plot(Model27)

##T test (Obs vs. Est.)
exp(fitted(Model27))
t.test(exp(fitted(Model27)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model27))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model27) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model27)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model27)
significants<-cook>1
significants

#Model28 (Recip X)
a=1/(DataSet$bio9_23)
b=1/(DataSet$bio16_23)
c=1/(DataSet$EVI)
d=1/abs(DataSet$Y)
Model28=lm(AGB~a+b+c+d, data=DataSet)
Model28
summary(Model28)
summary.aov(Model28)

#Model29 (Recip Y)
a=1/(DataSet$AGB)
b=abs(DataSet$Y)
Model29=lm(a~bio9_23+bio16_23+EVI+b, data=DataSet)
Model29
summary(Model29)
summary.aov(Model29)

#Model30 (Recip doble)
a=1/(DataSet$AGB)
b=1/(DataSet$bio9_23)
c=1/(DataSet$bio16_23)
d=1/(DataSet$EVI)
e=1/abs(DataSet$Y)
Model30=lm(a~b+c+d+e, data=DataSet)
Model30
summary(Model30)
summary.aov(Model30)

#Model31 (Multip)
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$bio16_23)
d=log(DataSet$EVI)
e=log (abs(DataSet$Y))
Model31=lm(a~b+c+d+e, data=DataSet)
Model31
summary(Model31)
summary.aov(Model31)
par(mfrow = c(2,2))
plot(Model31)

##T test (Obs vs. Est.) 
exp(fitted(Model31))
t.test(exp(fitted(Model31)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model31))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model31) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model31)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model31)
significants<-cook>1
significants

#Model32 (Exponential)
a=log(DataSet$AGB)
b=(DataSet$bio9_23)
c=(DataSet$bio16_23)
d=(DataSet$EVI)
e=abs(DataSet$Y)
Model32=lm(a~b+c+d+e, data=DataSet)
Model32
summary(Model32)
summary.aov(Model32)
plot(Model32)

##T test (Obs vs. Est.) 
exp(fitted(Model32))
t.test(exp(fitted(Model32)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model32))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model32) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model32)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model32)
significants<-cook>1
significants

#Model33 (Logarithmic)
a=(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$bio16_23)
d=log(DataSet$EVI)
e=log (abs(DataSet$Y))
Model33=lm(a~b+c+d+e, data=DataSet)
Model33
summary(Model33)
summary.aov(Model33)
plot(Model33)

##T test (Obs vs. Est.) 
exp(fitted(Model33))
t.test(exp(fitted(Model33)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model33))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model33) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model33)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model33)
significants<-cook>1
significants

#Model34 (Square root X)
a=(DataSet$AGB)
b=sqrt(DataSet$bio9_23)
c=sqrt(DataSet$bio16_23)
d=sqrt(DataSet$EVI)
e=sqrt (abs(DataSet$Y))
Model34=lm(a~b+c+d+e, data=DataSet)
Model34
summary(Model34)
summary.aov(Model34)

#Model35 (Square root Y)
a=sqrt(DataSet$AGB)
b=(DataSet$bio9_23)
c=(DataSet$bio16_23)
d=(DataSet$EVI)
e=(abs(DataSet$Y))
Model35=lm(a~b+c+d+e, data=DataSet)
Model35
summary(Model35)
summary.aov(Model35)
plot(Model35)

##T test (Obs vs. Est.) 
exp(fitted(Model35))
t.test(exp(fitted(Model35)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model35))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model35) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model35)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model35)
significants<-cook>1
significants

#Model36 (Curve S)
a=log(DataSet$AGB)
b=1/(DataSet$bio9_23)
c=1/(DataSet$bio16_23)
d=1/(DataSet$EVI)
e=1/(abs(DataSet$Y))
Model36=lm(a~b+c+d+e, data=DataSet)
Model36
summary(Model36)
summary.aov(Model36)
par(mfrow = c(2,2))
plot(Model36)

#Model37 (Simple with BIO9,EVI,lat)
a=abs(DataSet$Y)
Model37=lm(AGB~bio9_23+EVI+a, data=DataSet)
Model37
summary(Model37)
summary.aov(Model37)
plot(Model37)
confint(Model37, level = 0.95)

##T test (Obs vs. Est.) 
exp(fitted(Model37))
t.test(exp(fitted(Model37)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model37))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model37) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model37)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model37)
significants<-cook>1
significants

#Model38 (Mult with BIO9,EVI,lat)
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$EVI)
d=log(abs(DataSet$Y))
Model38=lm(a~b+c+d, data=DataSet)
Model38
summary(Model38)
summary.aov(Model38)
par(mfrow = c(2,2))
plot(Model38)
confint(Model38, level = 0.95)

##T test (Obs vs. Est.) 
exp(fitted(Model38))
t.test(exp(fitted(Model38)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model38))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model38) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model38)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model38)
significants<-cook>1
significants

#Model39 (Exp BIO9,EVI,lat)
a=log(DataSet$AGB)
b=(DataSet$bio9_23)
c=(DataSet$EVI)
d=abs(DataSet$Y)
Model39=lm(a~b+c+d, data=DataSet)
Model39
summary(Model39)
summary.aov(Model39)
par(mfrow = c(2,2))
plot(Model39)

##T test (Obs vs. Est.) 
exp(fitted(Model39))
t.test(exp(fitted(Model39)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model39))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model39) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model39)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model39)
significants<-cook>1
significants

#Model40 (Log with BIO9,EVI,lat)
a=(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$EVI)
d=log(abs(DataSet$Y))
Model40=lm(a~b+c+d, data=DataSet)
Model40
summary(Model40)
summary.aov(Model40)

#Model41 (Square root X with BIO9,EVI,lat)
a=(DataSet$AGB)
b=sqrt(DataSet$bio9_23)
c=sqrt(DataSet$EVI)
d=sqrt (abs(DataSet$Y))
Model41=lm(a~b+c+d, data=DataSet)
Model41
summary(Model41)
summary.aov(Model41)

#Model42 (Square root y with BIO9,EVI,lat)
a=sqrt(DataSet$AGB)
b=(DataSet$bio9_23)
c=(DataSet$EVI)
d=(abs(DataSet$Y))
Model42=lm(a~b+c+d, data=DataSet)
Model42
summary(Model42)
summary.aov(Model42)
plot(Model42)

##T test (Obs vs. Est.) 
exp(fitted(Model42))
t.test(exp(fitted(Model42)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model42))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model42) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model42)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model42)
significants<-cook>1
significants

#Model43 (Curve S with BIO9,EVI,lat)
a=log(DataSet$AGB)
b=1/(DataSet$bio9_23)
c=1/(DataSet$EVI)
d=1/(abs(DataSet$Y))
Model43=lm(a~b+c+d, data=DataSet)
Model43
summary(Model43)
summary.aov(Model43)
par(mfrow = c(2,2))
plot(Model43)

##T test (Obs vs. Est.) 
exp(fitted(Model43))
t.test(exp(fitted(Model43)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model43))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model43) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model43)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model43)
significants<-cook>1
significants

#Model44 (Curve S with BIO9,EVI)
a=log(DataSet$AGB)
b=1/(DataSet$bio9_23)
c=1/(DataSet$EVI)
Model44=lm(a~b+c, data=DataSet)
Model44
summary(Model44)
summary.aov(Model44)
par(mfrow = c(2,2))
plot(Model44)
confint(Model44, level = 0.95)

##T test (Obs vs. Est.) 
exp(fitted(Model44))
t.test(exp(fitted(Model44)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model44))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model44) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model44)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model44)
significants<-cook>1
significants

#Model45 (Mult with BIO9,EVI)
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$EVI)
Model45=lm(a~b+c, data=DataSet)
Model45
summary(Model45)
summary.aov(Model45)
par(mfrow = c(2,2))
plot(Model45)
confint(Model45, level = 0.95)

##T test (Obs vs. Est.) 
exp(fitted(Model45))
t.test(exp(fitted(Model45)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model45))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model45) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model45)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model45)
significants<-cook>1
significants

#Model46 (Exp BIO9,EVI)
a=log(DataSet$AGB)
b=(DataSet$bio9_23)
c=(DataSet$EVI)
Model46=lm(a~b+c, data=DataSet)
Model46
summary(Model46)
summary.aov(Model46)
par(mfrow = c(2,2))
plot(Model46)
confint(Model46, level = 0.95)

##T test (Obs vs. Est.) 
exp(fitted(Model46))
t.test(exp(fitted(Model46)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model46))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model46) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model46)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model46)
significants<-cook>1
significants

#Model47 (Square root y with BIO9,EVI)
a=sqrt(DataSet$AGB)
b=(DataSet$bio9_23)
c=(DataSet$EVI)
Model47=lm(a~b+c, data=DataSet)
Model47
summary(Model47)
summary.aov(Model47)
confint(Model47, level = 0.95)

##T test (Obs vs. Est.) 
exp(fitted(Model46))
t.test(exp(fitted(Model47)), exp(a))

##Shapiro-Wilk Test (Normality)
shapiro.test(rstandard(Model47))

##Durbin-Watson Test (Autocorrelation of residuals)
dwtest(Model47) 

##Goldfeld-Quandt Test (Homocedasticity)
gqtest(Model47)

## Cook's distance (detection of influential variables)
cook<-cooks.distance(Model47)
significants<-cook>1
significants

#Model48 (Curve S with BIO9, BIO16, EVI)
a=log(DataSet$AGB)
b=1/(DataSet$bio9_23)
c=1/(DataSet$EVI)
e=1/(DataSet$bio16_23)
Model48=lm(a~b+c+e, data=DataSet)
Model48
summary(Model48)
summary.aov(Model48)
par(mfrow = c(2,2))
plot(Model48)
confint(Model48, level = 0.95)


#Akaike's Criterion
AIC(Model1,Model2,Model3,Model4,Model5,Model6,Model7,Model8,Model9,Model10,Model11,Model12,Model13,Model14,Model15,Model16,Model17,Model18,Model19,Model20,Model21,Model22,Model23,Model24,Model25,Model26,Model27,Model28,Model29,Model30,Model31,Model32,Model33,Model34,Model35,Model36,Model37,Model38,Model39,Model40,Model41,Model42,Model43,Model44,Model45,Model46,Model47,Model48)
BIC(Model1,Model2,Model3,Model4,Model5,Model6,Model7,Model8,Model9,Model10,Model11,Model12,Model13,Model14,Model15,Model16,Model17,Model18,Model19,Model20,Model21,Model22,Model23,Model24,Model25,Model26,Model27,Model28,Model29,Model30,Model31,Model32,Model33,Model34,Model35,Model36,Model37,Model38,Model39,Model40,Model41,Model42,Model43,Model44,Model45,Model46,Model47,Model48)

#Comparison with Akaike's criterion
install.packages("AICcmodavg")
library(AICcmodavg)

Cand.mod <- list()
Cand.mod[[1]] <- Model25
Cand.mod[[2]] <- Model44
Cand.mod[[3]] <- Model45
Cand.mod[[4]] <- Model46
Cand.mod

NamesModels <- c("Model25", "Model44", "Model45", "Model46")

#Selection of models based in AICc ##Delta_AICc es el incremento desde el AICc mas bajo (modelos equivalentes en rango de D=2)
aictab(cand.set = Cand.mod, modnames = NamesModels)

#Calculate "evidence ratios" 
evidence(aictab(cand.set = Cand.mod, modnames = NamesModels))

#Subgroup of most relevant models
confset(cand.set = Cand.mod, modnames = NamesModels, second.ord = TRUE, method = "raw")
confset(cand.set = Cand.mod, modnames = NamesModels, second.ord = TRUE, method = "ordinal")

######################################################################################################

##Fitting Generalized Linear Models

#Install and load required packages
install.packages("Matrix")
library(Matrix)

install.packages("lme4")
library(lme4)

#SectorII corresponds to classification by mangrove type for Cispata and Malaga data

#Model25

##Fit the not multilevel Model25
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$bio16_23)
d=log(DataSet$EVI)
Model25=glm(a~b+c+d, data=DataSet)
Model25
summary(Model25)

##Fit a varying intercept model with Sector Model25
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$bio16_23)
d=log(DataSet$EVI)
Model25Sector=glm(a~b+c+d+SectorII, data=DataSet)
Model25Sector
summary(Model25Sector)

anova(Model25, Model25Sector, test="F")

##Fit a varying intercept model with UAC Model25
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$bio16_23)
d=log(DataSet$EVI)
Model25UAC=glm(a~b+c+d+UAC, data=DataSet)
Model25UAC
summary(Model25UAC)

anova(Model25, Model25UAC, test="F")

##Fit the interaction between UAC and Sector Model25
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$bio16_23)
d=log(DataSet$EVI)
Model25UAC_Sector=glm(a~b+c+d+UAC:SectorII, data=DataSet)
Model25UAC_Sector
summary(Model25UAC_Sector)

anova(Model25, Model25UAC_Sector, test="F")


#Model44

##Fit the not multilevel Model44
a=log(DataSet$AGB)
b=1/(DataSet$bio9_23)
c=1/(DataSet$EVI)
Model44=glm(a~b+c, data=DataSet)
Model44
summary(Model44)

##Fit a varying intercept model with Sector Model44
a=log(DataSet$AGB)
b=1/(DataSet$bio9_23)
c=1/(DataSet$EVI)
Model44Sector=glm(a~b+c+SectorII, data=DataSet)
Model44Sector
summary(Model44Sector)

anova(Model44, Model44Sector, test="F")

##Fit a varying intercept model with UAC Model44
a=log(DataSet$AGB)
b=1/(DataSet$bio9_23)
c=1/(DataSet$EVI)
Model44UAC=glm(a~b+c+UAC, data=DataSet)
Model44UAC
summary(Model44UAC)

anova(Model44, Model44UAC, test="F")

##Fit the interaction between UAC and Sector Model44
a=log(DataSet$AGB)
b=1/(DataSet$bio9_23)
c=1/(DataSet$EVI)
Model44UAC_Sector=glm(a~b+c+UAC:SectorII, data=DataSet)
Model44UAC_Sector
summary(Model44UAC_Sector)

anova(Model44, Model44UAC_Sector, test="F")

#Model45

##Fit the not multilevel Model45
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$EVI)
Model45=glm(a~b+c, data=DataSet)
Model45
summary(Model45)

##Fit a varying intercept model with Sector Model45
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$EVI)
Model45Sector=glm(a~b+c+SectorII, data=DataSet)
Model45Sector
summary(Model45Sector)

anova(Model45, Model45Sector, test="F")

##Fit a varying intercept model with UAC Model45
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$EVI)
Model45UAC=glm(a~b+c+UAC, data=DataSet)
Model45UAC
summary(Model45UAC)

anova(Model45, Model45UAC, test="F")

##Fit the interaction between UAC and Sector Model45
a=log(DataSet$AGB)
b=log(DataSet$bio9_23)
c=log(DataSet$EVI)
Model45UAC_Sector=glm(a~b+c+UAC:SectorII, data=DataSet)
Model45UAC_Sector
summary(Model45UAC_Sector)

anova(Model45, Model45UAC_Sector, test="F")

#Model46

##Fit the not multilevel Model46
a=log(DataSet$AGB)
b=(DataSet$bio9_23)
c=(DataSet$EVI)
Model46=glm(a~b+c, data=DataSet)
Model46
summary(Model46)

##Fit a varying intercept model with Sector Model46
a=log(DataSet$AGB)
b=(DataSet$bio9_23)
c=(DataSet$EVI)
Model46Sector=glm(a~b+c+SectorII, data=DataSet)
Model46Sector
summary(Model46Sector)

anova(Model46, Model46Sector, test="F")

##Fit a varying intercept model with UAC Model46
a=log(DataSet$AGB)
b=(DataSet$bio9_23)
c=(DataSet$EVI)
Model46UAC=glm(a~b+c+UAC, data=DataSet)
Model46UAC
summary(Model46UAC)

anova(Model46, Model46UAC, test="F")

##Fit the interaction between UAC and Sector Model46
a=log(DataSet$AGB)
b=(DataSet$bio9_23)
c=(DataSet$EVI)
Model46UAC_Sector=glm(a~b+c+UAC:SectorII, data=DataSet)
Model46UAC_Sector
summary(Model46UAC_Sector)

anova(Model46, Model46UAC_Sector, test="F")


#############################################################################################

##Coordinates of mangrove area in Colombia - Source: Global Mangrove Forest Distribution, 2000 (Giri et al. 2013)

#Install and load required packages
library(raster)
library(rgdal)
install.packages("maps")
library(maps)
install.packages("sp")
library(sp)
install.packages("mapdata")
library(mapdata) 

#Process tiff images and extract coordinates
## Image W80N10
img= readGDAL("~/TEE/Manuscripts/Mangroves/NASA Mangrove TIF_Colombia/W80N10.tif")
img2=raster(img)
cells=which(img2@data@values==1) #points with mangroves=1
xy=xyFromCell(img2,cells)
write.csv(xy,"~/TEE/Manuscripts/Mangroves/NASA Mangrove TIF_Colombia/W80N10xy.csv",row.names =FALSE)

## Image W80N20
img= readGDAL("~/TEE/Manuscripts/Mangroves/NASA Mangrove TIF_Colombia/W80N20.tif")
img2=raster(img)
cells=which(img2@data@values==1) #points with mangroves=1
xy=xyFromCell(img2,cells)
write.csv(xy,"~/TEE/Manuscripts/Mangroves/NASA Mangrove TIF_Colombia/W80N20xy.csv",row.names =FALSE)

#Graphic the points the download points 
W80N10xy=read.csv("NASA Mangrove TIF_Colombia/W80N10xy.csv")
W80N20xy=read.csv("NASA Mangrove TIF_Colombia/W80N20xy.csv")

xyall=rbind(W80N10xy, W80N20xy)

map('worldHires','Colombia')
points(xyall,pch=".", col=3)

###As is possible to observe in the last graphic, there are some points which don't belong to Colombia. Usign ArcGis were identified the country asociated to each point. From this info were extracted only the points for Colombia
#get points for Colombia W80N20
W80N20=read.table("W80N20xy.csvcountry.txt",sep=",",header=TRUE)
ColW80N20=W80N20[W80N20[,3]==197,1:2]
write.csv(ColW80N20,"~/TEE/Manuscripts/Mangroves/ColW80N20.csv",row.names=FALSE)


#get points for Colombia W80N10
W80N10=read.table("W80N10xy.csvcountry.txt",sep=",",header=TRUE)
ColW80N10=W80N10[W80N10[,3]==197,1:2]
write.csv(ColW80N10,"~/TEE/Manuscripts/Mangroves/ColW80N10.csv",row.names=FALSE)

# pdf(file="Manuscript/Figures/map.pdf")
# map('worldHires','Colombia')
# points(ColW80N20,pch=".", col=3)
# points(ColW80N10,pch=".", col=3)
# 
# sitesAGBd=plots[!is.na(plots[,3]),1:2]
# sitesAGBd=data.frame(X=-1*sitesAGBd[,1],Y=sitesAGBd[,2])
# coordinates(sitesAGBd)<-~X+Y
# points(sitesAGBd,col=2,pch="*") #Coordinates of sites with AGB density data
# dev.off()

#combinate both date base in one
Colall=rbind(ColW80N10, ColW80N20)
write.csv(Colall,"~/TEE/Manuscripts/Mangroves/Colall.csv",row.names=FALSE)
#############################################################################################

##Worldclim data for Colall coordinates

#Extract data from Worldclim
library(raster)
c= getData('worldclim', var='bio', res=0.5, lon=-75, lat=10)

#Import table with coordinates
ColTab<-read.csv("~/TEE/Manuscripts/Mangroves/Colall.csv")
ColTab
dim(ColTab)

#To confirm data are numeric
names(ColTab) 
is.numeric(ColTab$long)
is.numeric(ColTab$lat)

#Introduce coordinates
coordinates(ColTab)<-~long+lat

#Extract climate data for specific coordinates
Coldat<-extract(w,ColTab)
Coldat
################################################################################################

##Extract data from MODIS images for Colall coordinates ACA CAMBIAR TODO ESTO POR EL SCRIPT Mangrove_Grid_Colombia

#Install and load required package
install.packages("MODISTools")
library (MODISTools)

#Format the data
modis.subsetCol <- read.csv("~/TEE/Manuscripts/Mangroves/Colall.csv")
modis.subsetCol

names(modis.subsetCol) 
is.numeric(modis.subsetCol$lat)
is.numeric(modis.subsetCol$long)

#Time-series of MODIS data
modis.subsetCol$start.date <- rep(2011, nrow(modis.subsetCol))
modis.subsetCol$end.date <- rep(2013, nrow(modis.subsetCol))

#Specifing a subset request
GetProducts()

#Listing available bands for MOD13Q1 product
GetBands(Product = "MOD13Q1")

#Checking the time-series of MODIS data we want is available for this data product
GetDates(Product = "MOD13Q1", Lat = modis.subsetCol$lat[81], Long = modis.subsetCol$long[81])

#Downloading information EVI
modis.subsetCol <- read.csv("~/TEE/Manuscripts/Mangroves/Colall.csv")
print(tail(modis.subsetCol))

f=function(i){
  if(i==7|i==9){
    stop()	
  }else{
    i^2
  }
}

Dir="../EVI_ASC_Files"
B1="250m_16_days_EVI"
B2="250m_16_days_pixel_reliability"
Pr= "MOD13Q1"
qth=1 # was set to 0 before (check for a reasonable value) 
#nmax=20
nmax=nrow(modis.subsetCol)
#fs=function(i){try(f(i))}
fs=function(i){
  ss=modis.subsetCol[i:i,]
  try(
    #Downloading information EVI
    MODISSubsets(LoadDat = ss, Products = Pr, Bands = c(B1, B2), Size = c(1,1),SaveDir=Dir)
  )
} ##Function allows to download the information without stops when the server found some error in some data. Finally the function list the files with errors.
results=lapply(1:nmax,fs)
print(results)
cls=lapply(results,class)
errorIndices=c()
for (i in 1:length(cls)){if(cls[[i]]=="try-error"){errorIndices=c(errorIndices,i)}}
print(errorIndices)

#Finding average each pixel over time, to produce one tile of mean EVI, NDVI and FPAR pixels at each subset location
MODISSummaries(LoadDat = modis.subsetCol, Product = "MOD13Q1", Bands = "250m_16_days_EVI", ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001, QualityScreen = TRUE, QualityBand = "250m_16_days_pixel_reliability", QualityThreshold = 0)

#Calculating EVI mean for each coordinate (mean of 81 pixels)
Mod1=read.csv("~/TEE/Manuscripts/Mangroves/MODIS_Data_MOD13Q1_2015-06-18_h10-m2-s54.csv") ####CAMBIAR

meanMod1=data.frame(Mod1[,1:2],meanEVI=apply(Mod1[,6:86],1,mean,na.rm=TRUE))
meanMod1

#Conforming an only data base with information of bioclimatic variables, EVI and NDVI and AGB data for Colombia
AllColDataset=cbind(plots,EVI=meanMod1[,3],dat) #plots (data base with AGB data for Colombian mangroves), dat (data base with bioclimatic variables downloaded from WorldClim)
plot(AllColDataset)
write.csv(AllColDataset,"~/TEE/Manuscripts/Mangroves/AllColDataset.csv")

##############################################################################################################

#Weighted predictions (using Model44 and Model 45: best models) ACA CAMBIAR USANDO TODOS LOS MODELOS

#Conforming an only data base with information of bioclimatic variables, EVI and NDVI and AGB data for Colombia
AllColDataset=cbind(plots,EVI=meanMod1[,3],dat) #plots (data base with AGB data for Colombian mangroves), dat (data base with bioclimatic variables downloaded from WorldClim)
plot(AllColDataset)
write.csv(AllColDataset,"~/TEE/Manuscripts/Mangroves/AllColDataset.csv")

modavgpred(cand.set = Cand.mod, modnames = NamesModels, newdata = AllColDataset) 
###No se si asi este bien o deba limitar la funcion a los modelos elegidos
#Chosen.mod <- list()
#Chosen.mod[[1]] <- Model44
#Chosen.mod[[2]] <- Model45
#Chosen.mod

#NamesChosenModels <- c("Model44", "Model45")

#modavgpred(cand.set = Chosen.mod, modnames = NamesChosenModels, newdata = AllColDataset) 

#########################################################################################################################

#Area from Global Mangrove Forests Distribution, 2000 (Giri et al., 2013)
#resolution: 30x30 m

#Caribbean
dim(ColW80N20)
522942*900
470647800/10000

#Pacific
dim(ColW80N10)
1839596*900
1655636400/10000

#Total Country
47064.78+165563.6

################################################################################################

##Citation packages

x<-citation(package="AICcmodavg", lib.loc=NULL, auto=NULL)
toBibtex(x)

y<-citation(package="lme4", lib.loc=NULL, auto=NULL)
toBibtex(y)
