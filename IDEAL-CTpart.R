library(stringr)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(agricolae)
#library(XLConnect)
library(readxl)
library(data.table)
library(ggpmisc)
library(psych)
library("PerformanceAnalytics")
library(MASS)
library(plotly)
library(extrafont)
library(scales)
library(ggrepel)
theme_set(theme_bw())
library(plotly)
library(htmlwidgets)
theme(text=element_text(size=10,  family="Arial"))
setwd("D:/1.study/PHD/UCPRC/Project/SCB/SCB_Report/IDEAL-CT")#To be updated with current directory
#rm(list=ls())



# Read IDEAL-CT and 4PB excel data ----------------------------------------
#wb=loadWorkbook("AlldataIDealCT.xlsx",create=FALSE)
ExcelsheetFile="AlldataIDealCT.xlsx"
data=read_excel(ExcelsheetFile,sheet='SCBVS4PBfilteredNEW')#4PB data
##Select the columns and rows
#data=data[data$Strainlevel==3,]#select the rows with multiple fatigue strain levels
data1=data[1:(nrow(data)-1),c(1,4:11,13,34:38,41,43:45,50:52)]
data1$Mean_strengthPSI=data1$Mean_strengthPSI*0.00689#PSI to MPa

##Change the column names
colnames(data1)=c('MixLabel','FI','Sasc','Gf','Spp','Strength','KIC','AreaBefore','AreaAfter','FIasc','MIXID','strainlevel','E50',
                  'loga','b','StrainNf1M','Nf200ustrain','Nf400ustrain','Nf600ustrain','Intercept','slope','StrainNfQM')
##FI,Spp,Sasc,FIasc  VS E50 as no units for these SCB parameters
data1$MIXID=as.factor(data1$MIXID)

##COVplots for SCB (all materials and materials has IDEALCT,both I-FIT and IDEAL-CT tested materials)

##read IDEAL-CT data
dataIDT=read_excel(ExcelsheetFile,sheet='SCBVSIDT')
dataIDT=data.frame(dataIDT)
dataIDT1=dataIDT[!is.na(dataIDT$MIX.ID),]
dataIDT1=dataIDT1[!is.na(dataIDT1$mean_Strength_modified),]
dataIDT1$MIXID=as.factor(dataIDT1$MIXID)
#colnames(dataIDT1)[1]="MIX.ID"

##################################################
##COV plot
dataCOV=dataIDT1[,c(1,3,14:16,18,19)]
#dataCOV=dataCOV[c(1:13),]
#dataCOV[nrow(dataCOV),2]="0% RAP with AR binder"
datacov=dataCOV[,-c(1,2)]
for (i in c(1:ncol(datacov))){
  datacov[,i]=as.numeric(datacov[,i])
}

datacov["mean",]=colSums(datacov[c(1:9),])/dim(datacov)[1]
datacov["sd",]=apply(datacov[c(1:9),],2,sd)
datacov2=t(datacov)
datacov2=as.data.frame(datacov2)
setDT(datacov2,keep.rownames="Parameters")
datacov2$Parameters=c("Gf","m75","L75","Strength","CTindex")
datacov3=datacov2
#x=c("FI","Spp","FIasc","Sasc","Gf","Strength","KIC")
#datacov3=datacov2%>%mutate(Parameters=factor(Parameters,levels=x))%>%
#  arrange(Parameters)

COVplot=ggplot(datacov3,aes(x=reorder(datacov3$Parameters,-datacov3$mean),y=datacov3$mean))+
  geom_bar(stat="identity",fill="#009E73",size=0.3,width=0.8,colour="black")+
  geom_text(aes(label=round(datacov3$mean,2)), vjust=-0.2,size=3)+
  xlab("Parameters")+
  ylab("Coefficient of variation (%)")+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+ylim(c(0,100))+
  theme(axis.text=element_text(size=10),
        axis.ticks.length=unit(-1.0,"mm"),
        axis.ticks=element_line(size=.3))
#geom_errorbar(aes(ymin=ParaCOV2$mean-ParaCOV2$sd,ymax=ParaCOV2$mean+ParaCOV2$sd))

COVplot
# win.metafile(filename='COVplot2.wmf',height=2.5,width=4,,family="Arial",pointsize=10)
# tiff("Figure 2(b).tiff", units="in", width=4, height=2.5, res=300,family="Arial",pointsize=10)
# plot(COVplot)
# dev.off()
png("Figure7-1COVforIDEALCT(addmix).png",
    height=2.5, width = 4, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(COVplot)
dev.off()


#Merge IDEALCT and 4PB ###################

tt=merge(dataIDT1[c(1:11)],data1,by.x="MIX.ID",by.y="MixLabel")

#correlation analysis#############
IDTSCB=tt[c(1,3,5:ncol(tt))]
colnames(IDTSCB)[c(1,2,3,4,5,6,7,8,9)]=c('MIX.ID','MixType','IDT_Wf','IDT_Gf','IDT_m75',
                                         'IDT_L75','IDT_PeakLoad','IDT_Strength','IDT_CTindex')
IDTSCB$IDT_Strength=IDTSCB$IDT_Strength/1e6#change the unit to MPa
IDTSCB$MixType=as.factor(IDTSCB$MixType)
IDTSCB=droplevels(IDTSCB)
IDTSCB$MixType=as.character(IDTSCB$MixType)
IDTSCB$MixType[IDTSCB$MixType=="15%RAP with neat binder"]="15% RAP with neat binder"
IDTSCB$MixType[IDTSCB$MixType=="15%RAP with PG+X binder"]="15% RAP with AR binder"
IDTSCB$MixType[IDTSCB$MixType=="15%RAP with PM binder"]= "15% RAP with PM binder"
IDTSCB$MixType[IDTSCB$MixType=="25%RAP with neat binder"]= "25% RAP with neat binder"
IDTSCB$MixType[IDTSCB$MixType=="25%RAP with PM binder"]="25% RAP with PM binder"
IDTSCB$MixType[IDTSCB$MixType=="20%RAP3%RAS with neat binder"]="20% RAP + 3% RAS with neat binder"
IDTSCB$MixType[IDTSCB$MixType=="40%RAP with neat binder"]="40% RAP with neat binder"
IDTSCB$MixType[IDTSCB$MixType=="40%RAP with RA"]="40% RAP with RA"
IDTSCB$MixType[IDTSCB$MixType=="50%RAP with RA"]="50% RAP with RA"
IDTSCB$MixType[IDTSCB$MixType=="0%RAP with AR binder"]="0% RAP with AR binder"
IDTSCB$MixType[IDTSCB$MixType=="0%RAP with neat binder"]="0% RAP with neat binder"
IDTSCB$MixType[IDTSCB$MixType=="25%RAP with RA"]="25% RAP with RA"
IDTSCB$MixType[IDTSCB$MixType=="0%RAP with  AR binder"]="0% RAP with AR binder"
IDTSCB=IDTSCB[!is.na(IDTSCB$FI),]

IDTSCB$MixType=as.factor(IDTSCB$MixType)
IDTSCB2=IDTSCB
#png(height=800, width=1000, file="Fig7-2CorrelationMatrixSCBvsIDEAL(addmix).png", type = "cairo")
chart.Correlation(IDTSCB[c(4:6,8:15,18)],histogram=FALSE,pch="+")#saved as emf with size (650X312)
#dev.off()
my.formula=IDTSCB2$IDT_CTindex~IDTSCB2$FI
Figure=ggplot(data=IDTSCB2,aes(FI,IDT_CTindex))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#dd1c77','#980043',
                              '#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("FI")+
  ylab("CTindex")+#scale_y_continuous(breaks=seq(0, 12), limits=c(0,12))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = y~x,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure
png("Figure7-3CTindexVsFI(addMixes)2.png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

CTindexFIplotly=ggplotly(Figure,tooltip=c("x","y","mixid"))
widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}
widget_file_size(CTindexFIplotly)
saveWidget(CTindexFIplotly,"Figure7-3CTindexVsFI(addMixes).html")

#*strength vs strength ############
my.formula=IDTSCB2$IDT_Strength~IDTSCB2$Strength
FigureStrength=ggplot(IDTSCB2,aes(Strength*145.038,IDT_Strength*145.038))+
  geom_point(aes(color=MixType,shape=MixType, mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#dd1c77','#980043',
                              '#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("Strength from I-FIT (psi)")+
  ylab("Strength from IDEAL-CT (psi)")+#scale_y_continuous(breaks=seq(0, 12), limits=c(0,12))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = y~x,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
FigureStrength
png("Figure7-3(2)IDT_strengthVsSCB_strength(addmixes)2.png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrength)
dev.off()

Strengthplotly=ggplotly(FigureStrength,tooltip=c("x","y","mixid"))
widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}
widget_file_size(Strengthplotly)
saveWidget(Strengthplotly,"StrengthVSstrength(addMixes).html")


#unit-SI
my.formula=IDTSCB$IDT_Strength~IDTSCB$Strength
Figure=ggplot(IDTSCB,aes(Strength,IDT_Strength))+
  geom_point(aes(color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,16,23,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IFIT_Strength (MPa)")+
  ylab("IDT_Strength (MPa)")+#scale_y_continuous(breaks=seq(0, 12), limits=c(0,12))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = my.formula,aes(label = paste(stat(eq.label),
                                                      stat(rr.label), sep = "*\", \"*")),
               color="black",
               parse = TRUE,size=4)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure
win.metafile(filename="StrengthVSIDT_Strength.wmf",height=2.5,width=5.5,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#Correlation between IDT and 4PB#####
IDTSCB3=IDTSCB[!is.na(IDTSCB$IDT_Strength),]
IDTSCB3=IDTSCB3[!is.na(IDTSCB3$E50),]
IDTSCB3=IDTSCB3[IDTSCB3$strainlevel>1,]

#png(height=800, width=1000, file="Fig7-4CorrelationMatrix4pbvsIDEAL(addmix).png", type = "cairo")
#win.metafile(filename="Fig7-4CorrelationMatrix4pbvsIDEAL(addmix).wmf",height=10,width=12,family="Arial",pointsize=6)
chart.Correlation(IDTSCB2[c(4:6,8:9,21,24)],histogram=FALSE,pch="+")#saved as emf with size (550X)

#dev.off()
IDTSCB3=droplevels(IDTSCB3)

#**dense graded conventional asphalt mixtures (25%RAP or less)----
IDTSCB_Dense=IDTSCB3
IDTSCB_Dense$Conventional="No"
IDTSCB_Dense$Conventional[IDTSCB_Dense$MixType=="0% RAP with neat binder"]="Yes"
IDTSCB_Dense$Conventional[IDTSCB_Dense$MixType=="15% RAP with neat binder"]="Yes"
IDTSCB_Dense$Conventional[IDTSCB_Dense$MixType=="25% RAP with neat binder"]="Yes"
IDTSCB_Dense$Conventional[IDTSCB_Dense$MixType=="25% RAP with RA"]="Yes"
IDTSCB_Dense$Conventional=as.factor(IDTSCB_Dense$Conventional)
#IDTSCB_Dense=IDTSCB_Dense[IDTSCB_Dense$Conventional==1,]
#UNIT: SI
my.formula=IDTSCB3$E50~IDTSCB3$IDT_Strength
Figure=ggplot(IDTSCB3,aes(IDT_Strength,E50))+
  geom_point(aes(color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0',
                              '#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IDT_Strength (MPa)")+
  ylab("E50 (MPa)")+
  scale_y_continuous(breaks=seq(2000, 10000,1000), label=comma,limits=c(2000,10000))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = y~x,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure

png("Figure7-5StrengthVsE50(addmixes).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#Take out non Caltrans mixes
my.formula=IDTSCB32$E50~IDTSCB32$IDT_Strength
Figure=ggplot(IDTSCB32,aes(IDT_Strength,E50))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0',
                              '#980043','#980043','#238443'))+###color represents RAP content
  xlab("IDT_Strength (MPa)")+
  ylab("E50 (MPa)")+
  scale_y_continuous(breaks=seq(2000, 10000,1000), label=comma,limits=c(2000,10000))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = y~x,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure
png("Figure7-5StrengthVsE50(addmixes)-CaltransMIX.png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#***add conventional mixes regression----
#SI unit

FigureStrengthE50Conventional=ggplot(data=IDTSCB_Dense,
                                     aes(IDT_Strength,E50))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('gray78','#000000','#df65b0','gray78','gray78',
                              '#980043','#980043','gray78','gray78','gray78'))+###color represents RAP content
  xlab("IDT_Strength (MPa)")+
  ylab("E50 (MPa)")+
  #xlim(c(100,300))+
  #ylim(c(250,1250))+
  # geom_smooth(aes(IDT_Strength*145.038,E50*0.145038,color="No"), data = IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",], 
  #             method = "lm", formula=y~x,alpha=0.1,fill="#d2006c") + 
  geom_smooth(aes(IDT_Strength,E50),
              data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="Yes",],method = "lm", color="#0193d3", 
              formula=y~x, alpha=0.1) +
  # scale_color_manual(name="Conventional mixes?", values=c("No"="#d2006c","Yes"="#0193d3"))+
  # scale_y_continuous(breaks=seq(1600, 8000,800), label=comma,limits=c(1600,8000))+#+
  #  geom_smooth(method='lm',formula=y~x,alpha=0.3)+#,linetype="dashed",color="black")+ 
  #  geom_smooth(method="lm",formula=my.formula2,linetype="dotted",color="red")+
  stat_poly_eq(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="Yes",],formula = y~x,aes(label = paste(..rr.label..)),
               color="black",
               parse = TRUE,size=3)+
  #stat_poly_eq(formula = my.formula2,aes(label = paste(..rr.label..)),color="blue",
  #             parse = TRUE,size=3)
  theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

FigureStrengthE50Conventional

png("Figure7-5StrengthVsE50(conventionalMix).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthE50Conventional)
dev.off()


FigureStrengthE50UNConventional=ggplot(data=IDTSCB_Dense,
                                       aes(IDT_Strength,E50))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','gray87','gray87','#df65b0','#dd1c77',
                              'gray87','gray87','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IDT_Strength (MPa)")+
  ylab("E50 (MPa)")+
  #xlim(c(100,300))+
  #ylim(c(250,1250))+
  geom_smooth(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",],aes(IDT_Strength,E50),method = "lm", color="#d2006c", 
              formula=y~x, alpha=0.1) +
  stat_poly_eq(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",],formula = y~x,aes(label = paste(..rr.label..)),color="black",
               parse = TRUE,size=3)+
  theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

FigureStrengthE50UNConventional

png("Figure7-5StrengthVsE50(unconventionalMix).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthE50UNConventional)
dev.off()

#TAKE out nonCaltrans
IDTSCB_Dense2=IDTSCB_Dense[!IDTSCB_Dense$MIX.ID %in% c("4.64-MIXA-0H","4.64-MIXA-6H","4.64-MIXB-0H",
                                                         "4.64-MIXB-16H","4.64-MIXC-0H","4.64-MIXC-5H",
                                                         "4.64-MIXD-0H","4.64-MIXD-16H"),]
IDTSCB_Dense2=droplevels(IDTSCB_Dense2)
FigureStrengthE50UNConventional=ggplot(data=IDTSCB_Dense2,
                                       aes(IDT_Strength,E50))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','gray87','gray87','#df65b0',
                              'gray87','gray87','#238443'))+###color represents RAP content
  xlab("IDT_Strength (MPa)")+
  ylab("E50 (MPa)")+
  #xlim(c(100,300))+
  #ylim(c(250,1250))+
  geom_smooth(data=IDTSCB_Dense2[IDTSCB_Dense2$Conventional=="No",],aes(IDT_Strength,E50),method = "lm", color="#d2006c", 
              formula=y~x, alpha=0.1) +
  stat_poly_eq(data=IDTSCB_Dense2[IDTSCB_Dense2$Conventional=="No",],formula = y~x,aes(label = paste(..rr.label..)),color="black",
               parse = TRUE,size=3)+
  theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

FigureStrengthE50UNConventional

png("Figure7-5StrengthVsE50(unconventionalMix)-CaltransMIX.png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthE50UNConventional)
dev.off()

#US unit
FigureStrengthE50Conventional=ggplot(data=IDTSCB_Dense,
                         aes(IDT_Strength*145.038,E50*0.145038))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('gray78','#000000','#df65b0','gray78','gray78',
                              '#980043','#980043','gray78','gray78','gray78'))+###color represents RAP content
  xlab("IDT_Strength (psi)")+
  ylab("E50 (ksi)")+
  xlim(c(100,300))+
  ylim(c(250,1250))+
  # geom_smooth(aes(IDT_Strength*145.038,E50*0.145038,color="No"), data = IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",], 
  #             method = "lm", formula=y~x,alpha=0.1,fill="#d2006c") + 
  geom_smooth(aes(IDT_Strength*145.038,E50*0.145038),
              data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="Yes",],method = "lm", color="#0193d3", 
              formula=y~x, alpha=0.1) +
 # scale_color_manual(name="Conventional mixes?", values=c("No"="#d2006c","Yes"="#0193d3"))+
  # scale_y_continuous(breaks=seq(1600, 8000,800), label=comma,limits=c(1600,8000))+#+
#  geom_smooth(method='lm',formula=y~x,alpha=0.3)+#,linetype="dashed",color="black")+ 
#  geom_smooth(method="lm",formula=my.formula2,linetype="dotted",color="red")+
  stat_poly_eq(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="Yes",],formula = y~x,aes(label = paste(..rr.label..)),color="black",
               parse = TRUE,size=3)+
  #stat_poly_eq(formula = my.formula2,aes(label = paste(..rr.label..)),color="blue",
  #             parse = TRUE,size=3)
  theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

FigureStrengthE50Conventional

png("Figure7-5StrengthVsE50-usunit(conventionalMix).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthE50Conventional)
dev.off()


FigureStrengthE50UNConventional=ggplot(data=IDTSCB_Dense,
                                     aes(IDT_Strength*145.038,E50*0.145038))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','gray87','gray87','#df65b0','#dd1c77',
                              'gray87','gray87','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IDT_Strength (psi)")+
  ylab("E50 (ksi)")+
  xlim(c(100,300))+
  ylim(c(250,1250))+
  geom_smooth(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",],aes(IDT_Strength*145.038,E50*0.145038),method = "lm", color="#d2006c", 
              formula=y~x, alpha=0.1) +
  stat_poly_eq(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",],formula = y~x,aes(label = paste(..rr.label..)),color="black",
               parse = TRUE,size=3)+
  theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

FigureStrengthE50UNConventional

png("Figure7-5StrengthVsE50-usunit(unconventionalMix).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthE50UNConventional)
dev.off()

#UNIT: US
FigureStrengthE50=ggplot(IDTSCB3,aes(IDT_Strength*145.038,E50*0.145038))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77',
                              '#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IDT_Strength (psi)")+
  ylab("E50 (ksi)")+
 # scale_y_continuous(breaks=seq(1600, 8000,800), label=comma,limits=c(1600,8000))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = y~x,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

FigureStrengthE50

png("Figure7-5StrengthVsE50-usunit(addmixes).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthE50)
dev.off()

StrengthE50plotly=ggplotly(FigureStrengthE50,tooltip=c("x","y","mixid"))
secret_graph = api_create(StrengthE50plotly, filename = "secret-StrengthE50", sharing = "secret")
secret_graph
widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}
widget_file_size(StrengthE50plotly)
saveWidget(StrengthE50plotly,"Figure7-5StrengthVsE50-usunit(addmixes).html")


# Mixes tested for both SCB and IDEAL-CT ----------------------------------
my.formula=IDTSCB3$E50~IDTSCB3$Strength
Figure=ggplot(IDTSCB3,aes(Strength,E50))+
  geom_point(aes(color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#dd1c77','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IFIT_Strength (MPa)")+
  ylab("E50 (MPa)")+
  scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = my.formula,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure
win.metafile(filename="IFIT_StrengthVSE50.wmf",height=2.5,width=5.5,family="Arial",pointsize=10)
#tiff("Figure 6.tiff", units="in", width=5.5, height=2.5, res=300,family="Arial",pointsize=10)

plot(Figure)
dev.off()


my.formula=IDTSCB3$StrainNf1M~IDTSCB3$Strength
Figure=ggplot(IDTSCB3,aes(Strength,StrainNf1M))+
  geom_point(aes(color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#dd1c77','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IFIT_Strength (MPa)")+
  ylab("StrainNf1M")+
  #scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = my.formula,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line=(size=0.6))

Figure
win.metafile(filename="IFIT_StrengthVSStrainNf1M.wmf",height=2.5,width=5.5,family="Arial",pointsize=10)
#tiff("Figure 6.tiff", units="in", width=5.5, height=2.5, res=300,family="Arial",pointsize=10)
plot(Figure)
dev.off()


#IDT vs fatigue strain value ####
#*IDT vs StrainNf1M#####
#IDTSCB3$IDT_Strength=IDTSCB3$IDT_Strength*145.038#convert MPa to psi
for (i in c(3:9)){
my.formula=(IDTSCB3$StrainNf1M)~(IDTSCB3[,i])
Figure=ggplot(IDTSCB3,aes(IDTSCB3[,i],StrainNf1M))+
  geom_point(aes(color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab(colnames(IDTSCB2)[i])+
  ylab("StrainNf1M")+
  #scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  geom_smooth(method='lm',formula=y ~ x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = y ~ x,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

png(filename=paste(colnames(IDTSCB3)[i],"VS(strainNf1m)(addmix).png"),#Figure7-6 and Figure 7-7
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()
}

#*IDT vs StrainNf0.25M###### 
for (i in c(3:9)){
  my.formula=(IDTSCB3$StrainNfQM)~IDTSCB3[,i]
  Figure=ggplot(IDTSCB3,aes(IDTSCB3[,i],StrainNfQM))+
    geom_point(aes(color=MixType,shape=MixType),size=2.5,stroke=0.5)+
    scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
    scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
    xlab(colnames(IDTSCB2)[i])+
    ylab("StrainNf0.25M")+
    #scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
    geom_smooth(method='lm',formula=y ~ x,linetype="dashed",color="black")+    
    stat_poly_eq(formula = y ~ x,aes(label = paste(..rr.label..)), color="black",
                 parse = TRUE,size=3)+theme_bw()+
    font("xy.text",size=10,color="black")+
    font("xlab",size=10,color="black")+
    font("ylab",size=10,color="black")+
    theme(legend.text=element_text(size=10))+
    theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
  Figure
  
  png(filename=paste(colnames(IDTSCB3)[i],"VS(strainNf0.25m)(addmix).png"),#Figure7-6 and Figure 7-7
      height=3, width = 5.8, units = 'in', 
      type="windows", res=400,family="Arial",pointsize=10)
  plot(Figure)
  dev.off()
}

##StrainNf1M VS IDT_Strength (to be updated with testing data)(TO RUN)#####
IDTSCB3=droplevels(IDTSCB3)
IDTSCB4=IDTSCB3#new us units
IDTSCB4$IDT_Strength=IDTSCB4$IDT_Strength*145.038#MPa to psi
IDTSCB4$E50=IDTSCB4$E50*0.145038#MPa to ksi

my.formula=(IDTSCB4$StrainNf1M)~IDTSCB4$IDT_Strength
Figure=ggplot(IDTSCB4,aes(IDT_Strength,StrainNf1M))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77',
                              '#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IDT_Strength (psi)")+
  ylab("StrainNf1M")+
  #scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = y~x,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure

png(filename="IDT_strengthVS(strainNf1m)-usunit(addmix).png",#Figure7-6 and Figure 7-7
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#SI unit
my.formula=(IDTSCB3$StrainNf1M)~IDTSCB3$IDT_Strength
Figure=ggplot(IDTSCB3,aes(IDT_Strength,StrainNf1M))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77',
                              '#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IDT_Strength (MPa)")+
  ylab("StrainNf1M")+
  #scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = y~x,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure

png(filename="IDT_strengthVS(strainNf1m)(addmix).png",#Figure7-6 and Figure 7-7
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#Take out nonCaltran mixes
IDTSCB32=IDTSCB3[!IDTSCB3$MIX.ID %in% c("4.64-MIXA-0H","4.64-MIXA-6H","4.64-MIXB-0H",
                                          "4.64-MIXB-16H","4.64-MIXC-0H","4.64-MIXC-5H",
                                          "4.64-MIXD-0H","4.64-MIXD-16H"),]
IDTSCB32=droplevels(IDTSCB32)

my.formula=(IDTSCB32$StrainNf1M)~IDTSCB32$IDT_Strength
Figure=ggplot(IDTSCB32,aes(IDT_Strength,StrainNf1M))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0',
                              '#980043','#980043','#238443'))+###color represents RAP content
  xlab("IDT_Strength (MPa)")+
  ylab("StrainNf1M")+
  #scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = y~x,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure

png(filename="IDT_strengthVS(strainNf1m)(addmix)-CaltransMIX.png",#Figure7-6 and Figure 7-7
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()


#***add conventional mixes regression-IDT_Strenght vs StrainNf1M---
#*SI unit
FigureStrengthStrainConventional=ggplot(data=IDTSCB_Dense,
                                        aes(IDT_Strength,StrainNf1M))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('gray78','#000000','#df65b0','gray78','gray78',
                              '#980043','#980043','gray78','gray78','gray78'))+###color represents RAP content
  xlab("IDT_Strength (MPa)")+
  ylab("StrainNf1M")+
  #xlim(c(100,300))+
  #ylim(c(250,1250))+
  # geom_smooth(aes(IDT_Strength*145.038,E50*0.145038,color="No"), data = IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",], 
  #             method = "lm", formula=y~x,alpha=0.1,fill="#d2006c") + 
  geom_smooth(aes(IDT_Strength,StrainNf1M),
              data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="Yes",],method = "lm", color="#0193d3", 
              formula=y~x, alpha=0.1) +
  # scale_color_manual(name="Conventional mixes?", values=c("No"="#d2006c","Yes"="#0193d3"))+
  # scale_y_continuous(breaks=seq(1600, 8000,800), label=comma,limits=c(1600,8000))+#+
  #  geom_smooth(method='lm',formula=y~x,alpha=0.3)+#,linetype="dashed",color="black")+ 
  #  geom_smooth(method="lm",formula=my.formula2,linetype="dotted",color="red")+
  stat_poly_eq(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="Yes",],formula = y~x,aes(label = paste(..rr.label..)),color="black",
               parse = TRUE,size=3)+
  #stat_poly_eq(formula = my.formula2,aes(label = paste(..rr.label..)),color="blue",
  #             parse = TRUE,size=3)
  theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

FigureStrengthStrainConventional

png("Figure7-5StrengthVsStrainNf1M(conventionalMix).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthStrainConventional)
dev.off()


FigureStrengthStrainUNConventional=ggplot(data=IDTSCB_Dense,
                                          aes(IDT_Strength,StrainNf1M))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','gray87','gray87','#df65b0','#dd1c77',
                              'gray87','gray87','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IDT_Strength (MPa)")+
  ylab("StrainNf1M")+
  
  geom_smooth(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",],aes(IDT_Strength,StrainNf1M),method = "lm", color="#d2006c", 
              formula=y~x, alpha=0.1) +
  stat_poly_eq(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",],formula = y~x,aes(label = paste(..rr.label..)),color="black",
               parse = TRUE,size=3)+
  theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

FigureStrengthStrainUNConventional

png("Figure7-5StrengthVsStrainNf1M(unconventionalMix).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthStrainUNConventional)
dev.off()

#US unit
FigureStrengthStrainConventional=ggplot(data=IDTSCB_Dense,
                                     aes(IDT_Strength*145.038,StrainNf1M))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('gray78','#000000','#df65b0','gray78','gray78',
                              '#980043','#980043','gray78','gray78','gray78'))+###color represents RAP content
  xlab("IDT_Strength (psi)")+
  ylab("StrainNf1M")+
  #xlim(c(100,300))+
  #ylim(c(250,1250))+
  # geom_smooth(aes(IDT_Strength*145.038,E50*0.145038,color="No"), data = IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",], 
  #             method = "lm", formula=y~x,alpha=0.1,fill="#d2006c") + 
  geom_smooth(aes(IDT_Strength*145.038,StrainNf1M),
              data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="Yes",],method = "lm", color="#0193d3", 
              formula=y~x, alpha=0.1) +
  # scale_color_manual(name="Conventional mixes?", values=c("No"="#d2006c","Yes"="#0193d3"))+
  # scale_y_continuous(breaks=seq(1600, 8000,800), label=comma,limits=c(1600,8000))+#+
  #  geom_smooth(method='lm',formula=y~x,alpha=0.3)+#,linetype="dashed",color="black")+ 
  #  geom_smooth(method="lm",formula=my.formula2,linetype="dotted",color="red")+
  stat_poly_eq(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="Yes",],formula = y~x,aes(label = paste(..rr.label..)),color="black",
               parse = TRUE,size=3)+
  #stat_poly_eq(formula = my.formula2,aes(label = paste(..rr.label..)),color="blue",
  #             parse = TRUE,size=3)
  theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

FigureStrengthStrainConventional

png("Figure7-5StrengthVsStrainNf1M-usunit(conventionalMix).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthStrainConventional)
dev.off()


FigureStrengthStrainUNConventional=ggplot(data=IDTSCB_Dense,
                                       aes(IDT_Strength*145.038,StrainNf1M))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','gray87','gray87','#df65b0','#dd1c77',
                              'gray87','gray87','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IDT_Strength (psi)")+
  ylab("StrainNf1M")+

  geom_smooth(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",],aes(IDT_Strength*145.038,StrainNf1M),method = "lm", color="#d2006c", 
              formula=y~x, alpha=0.1) +
  stat_poly_eq(data=IDTSCB_Dense[IDTSCB_Dense$Conventional=="No",],formula = y~x,aes(label = paste(..rr.label..)),color="black",
               parse = TRUE,size=3)+
  theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

FigureStrengthStrainUNConventional

png("Figure7-5StrengthVsStrain-usunit(unconventionalMix).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthStrainUNConventional)
dev.off()


#*IDT_Strength vs strainNfQM-----
my.formula=(IDTSCB4$StrainNfQM)~IDTSCB4$IDT_Strength
Figure=ggplot(IDTSCB4,aes(IDT_Strength,StrainNfQM))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77',
                              '#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IDT_Strength (psi)")+
  ylab("StrainNf0.25M")+
  #scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+    
  stat_poly_eq(formula = y~x,aes(label = paste(..rr.label..)), color="black",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure

png(filename="IDT_strengthVS(strainNf0.25m)-usunit(addmix).png",#Figure7-6 and Figure 7-7
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

# win.metafile(filename=paste(colnames(IDTSCB2)[i],"VS(strainNf1m).wmf"),height=2.5,width=5.5,family="Arial",pointsize=10)
# tiff(filename=paste(colnames(IDTSCB2)[i],"VS(strainNf1m).tiff"), units="in", width=5.5, 
#      height=2.5, res=300,family="Arial",pointsize=10)
# plot(Figure)
# dev.off()


#**ln(E50^2(strainNf1M)) VS lnKIC OR lnStrength----
IDTSCB1=IDTSCB
IDTSCB1$E502Strain=(IDTSCB1$E50)^2*IDTSCB1$StrainNf1M
my.formula1=log((IDTSCB1$E502Strain))~log((IDTSCB1$IDT_Strength)) #y~x
Allfit=lm(log((IDTSCB1$E502Strain))~log(IDTSCB1$IDT_Strength))
summary(Allfit)

anova(Allfit)
Figure2=ggplot(IDTSCB1,aes(IDT_Strength,E502Strain))+
  geom_point(aes(color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#dd1c77','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IDT_Strength(MPa)")+
  ylab("StrainNf1M(E50)^2")+#scale_y_continuous(breaks=seq(0, 12), limits=c(0,12))+#+
  geom_smooth(method='nls',formula=y~a*x^b,method.args=list(start= c(a = 1700000000,b=1)),se=FALSE,linetype="dashed",color="black")+    
  stat_poly_eq(formula = my.formula1,aes(label = paste(..rr.label..)), color="black",label.x="center",geom="label",
               parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure2
win.metafile(filename="StrainNf1mE50^2VSIDT_Strength.wmf",height=2.5,width=5.5,family="Arial",pointsize=10)
plot(Figure2)
dev.off()


#*To predict the IDT strength for all mixtures from SCB strength----
data11=data1
tt1=tt[,c("MIX.ID","mean_Strength")]
tt1$mean_Strength=tt1$mean_Strength/1e6
colnames(tt1)[1]="MixLabel"
IDTSCBall=merge(data11,tt1,by="MixLabel",all=TRUE)
IDTSCBall$MIXID=as.character((IDTSCBall$MIXID))
IDTSCBall[12,11]="15%RAP with PM binder"
IDTSCBall$MIXID=as.factor((IDTSCBall$MIXID))
IDTSCBall$mean_Strength[is.na(IDTSCBall$mean_Strength)]=1.01+1.3*IDTSCBall$Strength[is.na(IDTSCBall$mean_Strength)]
Figure=ggplot(IDTSCBall)+
  geom_point(aes(mean_Strength,Strength,color=MIXID,shape=MIXID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,23,16,17,16,16,23,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#d63b9a','#d63b9a','#d63b9a','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+
  xlab("IDT_Strength(MPa)")+
  ylab("I-FIT_Strength(MPa)")+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure
win.metafile(filename="IDT_StrengthVSI-FIT_Strength.wmf",height=3,width=6,family="Arial",pointsize=10)
plot(Figure)
dev.off()


#*model for criteria based on IDEAL strength (only IDEAL strength)----- 
IDTSCB2=IDTSCB[-c(1,4,7,9),]#remove one strain level 4PB

##StrainNf1M VS Strength equation from E50
IDTSCB2$StrainNf1Mmodel=109920*(IDTSCB2$IDT_Strength*3706-1145)^(-0.7)
IDTSCB2$StrainNf1MmodelConserv=109920*((IDTSCB2$IDT_Strength+0.084)*3706-1145)^(-0.71)
Figure=ggplot(IDTSCB2)+
  geom_point(aes(log(mean_Strength),log(StrainNf1M), color=MIXID,shape=MIXID),size=2.5,stroke=0.5)+
  geom_line(aes(log(mean_Strength),log(StrainNf1Mmodel)))+
  geom_line(aes(log(mean_Strength),log(StrainNf1MmodelConserv)),linetype="dashed",color="red")+
  # stat_summary(geom="line",fun.y = mean)+
  # stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,23,16,17,16,16,23,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#d63b9a','#d63b9a','#d63b9a','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  annotate(geom="text",x=0.3,y=5.86,label="pass/fail line",color="red")+
  annotate(geom="text",x=0.6,y=7,label='bold("PASS")',color="blue",parse=TRUE)+
  annotate(geom="text",x=0.2,y=5,label='bold("FAIL")',color="red",parse=TRUE)+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  xlab("ln(IDT_Strength)")+
  ylab("ln(StrainNf1M)")+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure
win.metafile(filename="ln(IDT_Strength)VSln(StrainNf1Mmodel).wmf",height=3,width=6,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#*model for criteria based on all IDEAL strength (predicted from SCB)-----
IDTSCBall2=IDTSCBall[-c(1,4,7,9),]#remove one strain level 4PB
##StrainNf1M VS Strength equation from E50
IDTSCBall2$StrainNf1Mmodel=109920*(IDTSCBall2$mean_Strength*3706-1145)^(-0.7)
IDTSCBall2$StrainNf1MmodelConserv=109920*((IDTSCBall2$mean_Strength+0.084)*3706-1145)^(-0.6728)
Figure=ggplot(IDTSCBall2)+
  geom_point(aes(log(mean_Strength),log(StrainNf1M), color=MIXID,shape=MIXID),size=2.5,stroke=0.5)+
  geom_line(aes(log(mean_Strength),log(StrainNf1Mmodel)))+
  geom_line(aes(log(mean_Strength),log(StrainNf1MmodelConserv)),linetype="dashed",color="red")+
  # stat_summary(geom="line",fun.y = mean)+
  # stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,23,16,17,16,16,23,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#d63b9a','#d63b9a','#d63b9a','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  annotate(geom="text",x=0.3,y=5.86,label="pass/fail line",color="red")+
  annotate(geom="text",x=0.6,y=7,label='bold("PASS")',color="blue",parse=TRUE)+
  annotate(geom="text",x=0.2,y=5,label='bold("FAIL")',color="red",parse=TRUE)+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  xlab("ln(IDT_Strength)")+
  ylab("ln(StrainNf1M)")+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure
win.metafile(filename="ln(IDT_Strength)VSln(StrainNf1Mmodel).wmf",height=3,width=6,family="Arial",pointsize=10)
plot(Figure)
dev.off()


#criterion model based on confidence interval--------------
##confidence interval calculation for ln(strainNf1M)=ln(150241)-0.71*ln(3706.4*strength-1144.5);
##ln(strainNf1M)=STRAINNF
##ln(3706.4*strength-1144.5)=STRENGTH
##https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
SStot=sum((IDTSCB2$StrainNf1M-mean(IDTSCB2$StrainNf1M))^2)
SSres=sum((IDTSCB2$StrainNf1M-IDTSCB2$StrainNf1Mmodel)^2)
Rsquared=(1-(SSres/SStot))*100
Rsquared
STRENGTH_new=log(3706*IDTSCB2$IDT_Strength-1145)
Strain1m_new=11.61-0.71*STRENGTH_new
ggplot(IDTSCB2)+
  geom_point(aes(STRENGTH_new,log(StrainNf1M)))+
  geom_line(aes(STRENGTH_new,Strain1m_new))
se=sqrt(sum((log(IDTSCB2$StrainNf1M)-log(IDTSCB2$StrainNf1Mmodel))^2)/(9-2))*sqrt(1/9+
                                                                                 (IDTSCB2$IDT_Strength-
                                                                                    mean(IDTSCB2$IDT_Strength))^2/sum((IDTSCB2$IDT_Strength-
                                                                                                           mean(IDTSCB2$IDT_Strength))^2))
t.value=qt(0.975,9-2)

STRAINNF_new=11.61-0.71*STRENGTH_new
slope.upper=suppressWarnings(STRAINNF_new+t.value*se)
slope.lower=suppressWarnings((STRAINNF_new-t.value*se))
Figure=ggplot(IDTSCB2)+
  geom_point(aes(IDT_Strength,log(StrainNf1M), color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  geom_line(aes(IDT_Strength,(STRAINNF_new)))+
  geom_line(aes(IDT_Strength,slope.lower),linetype="dashed",color="red")+
  geom_line(aes(IDT_Strength,slope.upper),linetype="dashed",color="red")+
  # stat_summary(geom="line",fun.y = mean)+
  # stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,16,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#dd1c77','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("STRENGTH")+
  ylab("ln(StrainNf1M)")+
  annotate(geom="text",x=1.35,y=5.5,label='atop("pass/fail line","(95% CI low bound)")',color="red",parse=T,size=3)+
  annotate(geom="text",x=1.25,y=5.2,label='bold("Fail area")',parse=T, color="red")+
  annotate(geom="text",x=2,y=6.2,label='bold("Pass area")',parse=T,color="blue")+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure
png("Figure10-3strengthVsStrainNf1M.png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

win.metafile(filename="StrengthVSStrainNf1MCriterion_CI2.wmf",height=3,width=6,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#criterion model based on confidence interval of stiffness vs strength (Strength as the Y-axis)------
##confidence interval calculation 
model4=lm(IDT_Strength~E50,data=IDTSCB3)
conf_interval=predict(model4,data.frame((E50=IDTSCB3$E50)),interval="confidence",level=0.95)#95% CI
Figure=ggplot(IDTSCB3,aes(E50,IDT_Strength))+
  geom_point(aes(E50,IDT_Strength, color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  geom_smooth(method='lm',formula=y~x,linetype="solid",color="black")+    
  geom_line(aes(E50,conf_interval[,2]),linetype="dashed",color="red")+
  geom_line(aes(E50,conf_interval[,3]),linetype="dashed",color="red")+
  # stat_summary(geom="line",fun.y = mean)+
  # stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("E50 (MPa)")+
  ylab("IDT_Strength (MPa)")+
  annotate(geom="text",x=3800,y=1,label='atop("threshold line","(95% CI low bound)")',color="red",parse=T,size=3)+
  annotate(geom="text",x=3000,y=0.8,label='bold("Fail area")',parse=T, color="red")+
  annotate(geom="text",x=5500,y=2.2,label='bold("Pass area")',parse=T,color="blue")+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))+
  geom_line(aes(E50,slope.lower),linetype="dashed",color="red")+
  geom_line(aes(E50,slope.upper),linetype="dashed",color="red")+
  geom_line(aes(E50,Strengthmodel),linetype="dashed",color="red")
Figure
png("Figure10-1strengthVsstiffness.png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

win.metafile(filename="StrengthVSStiffnessCriterion_CI4.wmf",height=3,width=6,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#criterion model based on confidence interval of StrainNf1M vs IDT_Strength (IDT_Strength as the Y-axis)-------
model5=lm(log(IDT_Strength)~log(StrainNf1M),data=IDTSCB4)#IDTSCB3->IDTSCB4
my.formula=log(IDTSCB4$IDT_Strength)~log(IDTSCB4$StrainNf1M)
#ThresholdIDT_Strength=model5$coefficients[1]+model5$coefficients[2]*5.6
summary(model5)
#PROP1 <- predictNLS(model5, data.frame(StrainNf1M=IDTSCB2$StrainNf1M), interval = "confidence",alpha=0.05)
conf_interval2=predict(model5,data.frame((StrainNf1M=IDTSCB4$StrainNf1M)),interval="confidence",level=0.95)
#predict_atpoint=predict(model5,data.frame(StrainNf1M=269),interval="prediction",level=0.95)
Figure=ggplot(IDTSCB4,aes(log(StrainNf1M),log(IDT_Strength)))+
  geom_point(aes(log(StrainNf1M),log(IDT_Strength), color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  geom_smooth(method='lm',formula=y~x,se=TRUE,linetype="solid",color="black")+    
  geom_line(aes(log(StrainNf1M),conf_interval2[,2]),linetype="dashed",color="red")+
  geom_line(aes(log(StrainNf1M),conf_interval2[,3]),linetype="dashed",color="red")+
  #stat_poly_eq(formula = my.formula, 
  #             aes(label = paste0("atop(",..eq.label..,",", ..rr.label.., ")")), 
  #             parse = TRUE,label.x = 6,label.y=1) +  
  # stat_summary(geom="line",fun.y = mean)+
  # stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,16,17,23,16,16,23,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#df65b0','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f'
                              ,'#238443'))+###color represents RAP content
  xlab("ln(StrainNf1M)")+
  ylab("ln(IDT_Strength)(psi)")+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  #ylim(1200,12200)+
  annotate(geom="text",x=6.13,y=5.15,label='atop("threshold line","(95% CI up bound)")',color="red",parse=T,size=3)+
  annotate(geom="text",x=5.3,y=5.7,label='bold("Fail area")',parse=T, color="red")+
  annotate(geom="text",x=5.8,y=4.5,label='bold("Pass area")',parse=T,color="blue")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
  # geom_point(aes(x=5.6,y=ThresholdIDT_Strength),color="blue",cex=3,shape=19)+
  # geom_point(aes(x=5.6,y=predict_atpoint[2]),color="blue",cex=3,shape=3)+
  # geom_point(aes(x=5.6,y=predict_atpoint[3]),color="blue",cex=3,shape=3)+
  # geom_vline(xintercept=5.6,color="blue",linetype="dashed")+
  # annotate(geom="text",x=5.4,y=0.39,label='atop("threshold line","(95% CI low bound)")',color="red",parse=T,size=3)
Figure
png("Figure10-3strainNf1MVsStrengthCriterion_CIupbound(US).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#SI unit

#criterion model based on confidence interval of StrainNf1M vs IDT_Strength (IDT_Strength as the Y-axis)-------
model5=lm(log(IDT_Strength)~log(StrainNf1M),data=IDTSCB3)
my.formula=log(IDTSCB3$IDT_Strength)~log(IDTSCB3$StrainNf1M)
#ThresholdIDT_Strength=model5$coefficients[1]+model5$coefficients[2]*5.6
summary(model5)
#PROP1 <- predictNLS(model5, data.frame(StrainNf1M=IDTSCB2$StrainNf1M), interval = "confidence",alpha=0.05)
conf_interval2=predict(model5,data.frame((StrainNf1M=IDTSCB3$StrainNf1M)),interval="confidence",level=0.95)
#predict_atpoint=predict(model5,data.frame(StrainNf1M=269),interval="prediction",level=0.95)
Figure=ggplot(IDTSCB3,aes(log(StrainNf1M),log(IDT_Strength)))+
  geom_point(aes(log(StrainNf1M),log(IDT_Strength), color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  geom_smooth(method='lm',formula=y~x,se=TRUE,linetype="solid",color="black")+    
  geom_line(aes(log(StrainNf1M),conf_interval2[,2]),linetype="dashed",color="red")+
  geom_line(aes(log(StrainNf1M),conf_interval2[,3]),linetype="dashed",color="red")+
  #stat_poly_eq(formula = my.formula, 
  #             aes(label = paste0("atop(",..eq.label..,",", ..rr.label.., ")")), 
  #             parse = TRUE,label.x = 6,label.y=1) +  
  # stat_summary(geom="line",fun.y = mean)+
  # stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,16,17,23,16,16,23,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#df65b0','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f'
                              ,'#238443'))+###color represents RAP content
  xlab("ln(StrainNf1M)")+
  ylab("ln(IDT_Strength)(MPa)")+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  xlim(5,6.5)+
  annotate(geom="text",x=6.2,y=0.18,label='atop("threshold line","(95% CI up bound)")',color="red",parse=T,size=3)+
  annotate(geom="text",x=5.3,y=0.7,label='bold("Fail area")',parse=T, color="red")+
  annotate(geom="text",x=5.8,y=-0.3,label='bold("Pass area")',parse=T,color="blue")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
# geom_point(aes(x=5.6,y=ThresholdIDT_Strength),color="blue",cex=3,shape=19)+
# geom_point(aes(x=5.6,y=predict_atpoint[2]),color="blue",cex=3,shape=3)+
# geom_point(aes(x=5.6,y=predict_atpoint[3]),color="blue",cex=3,shape=3)+
# geom_vline(xintercept=5.6,color="blue",linetype="dashed")+
# annotate(geom="text",x=5.4,y=0.39,label='atop("threshold line","(95% CI low bound)")',color="red",parse=T,size=3)
Figure
png("Figure10-3strainNf1MVsStrengthCriterion_CIupbound(SI).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()


#predicted all IDEAL-CT strength vs StrainNf1M-------
Figure=ggplot(IDTSCBall2)+
  geom_point(aes(log(mean_Strength),log(StrainNf1M), color=MIXID,shape=MIXID),size=2.5,stroke=0.5)+
  geom_line(aes(log(mean_Strength),log(StrainNf1Mmodel)))+
  geom_line(aes(log(mean_Strength),log(StrainNf1MmodelConserv)),linetype="dashed",color="red")+
  # stat_summary(geom="line",fun.y = mean)+
  # stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,23,16,17,16,16,23,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#d63b9a','#d63b9a','#d63b9a','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  annotate(geom="text",x=0.3,y=5.86,label="pass/fail line",color="red")+
  annotate(geom="text",x=0.6,y=7,label='bold("PASS")',color="blue",parse=TRUE)+
  annotate(geom="text",x=0.2,y=5,label='bold("FAIL")',color="red",parse=TRUE)+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  xlab("ln(IDT_Strength)")+
  ylab("ln(StrainNf1M)")+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))+

  
Figure
win.metafile(filename="ln(IDT_Strength)VSln(StrainNf1Mmodel).wmf",height=3,width=6,family="Arial",pointsize=10)
plot(Figure)
dev.off()
##


#Example in the appendix-intermediate------------
#*E50------------
##criterion model stiffness vs strength (Strength as the Y-axis)
##both directly on regression curve calculation and CI
IDTSCB4=IDTSCB3#new us units
IDTSCB4$IDT_Strength=IDTSCB4$IDT_Strength*145.038#MPa to psi
IDTSCB4$E50=IDTSCB4$E50*0.145038#MPa to ksi
model4=lm(IDT_Strength~E50,data=IDTSCB4)
conf_interval=predict(model4,data.frame((E50=IDTSCB4$E50)),interval="confidence",level=0.95)#95% CI
E50require=782 #ksi for intermediate layer
Predict_atpoint=predict(model4,data.frame(E50=E50require),interval="confidence",level=0.95)
Figure=ggplot(data=IDTSCB4,aes(E50,IDT_Strength))+
  geom_point(aes(E50,IDT_Strength,color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  geom_point(aes(E50require,Predict_atpoint[1]),color="blue",size=1,stroke=1,shape=3)+
  geom_point(aes(E50require,Predict_atpoint[2]),color="blue",size=1,stroke=1,shape=3)+
  geom_point(aes(E50require,Predict_atpoint[3]),color="blue",size=1,stroke=1,shape=3)+
  geom_smooth(method='lm',formula=y~x,linetype="solid",color="black")+    
  geom_line(aes(E50,conf_interval[,2]),linetype="dashed",color="red")+
  geom_line(aes(E50,conf_interval[,3]),linetype="dashed",color="red")+
 #  #stat_summary(geom="line",fun.y = mean)+
#   #stat_summary(geom="ribbon",fun.data=mean_cl_boot,
#  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("E50 (ksi)")+
  ylab("IDT_Strength (psi)")+
 #  annotate(geom="text",x=600,y=100,label='atop("threshold line","(95% CI low bound)")',color="red",parse=T,size=3)+
 # annotate(geom="text",x=900,y=100,label='bold("Fail area")',parse=T, color="red")+
 #  annotate(geom="text",x=750,y=220,label='bold("Pass area")',parse=T,color="blue")+
 #  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
  geom_vline(xintercept=E50require,color="blue",linetype="dashed")+
  geom_hline(yintercept=Predict_atpoint[1],color="blue",linetype="dashed")+
  geom_label(x=E50require*0.65,y=Predict_atpoint[1]-0.1,label=paste0("Strength=",
                                                                     round(Predict_atpoint[1],2)," psi"),
             color="blue",size=2)+
  geom_label(x=E50require,y=Predict_atpoint[2]-8,label=paste0("Strength=",
                                                                round(Predict_atpoint[2],2)," psi"),
             color='blue',size=2)+
  geom_label(x=E50require,y=Predict_atpoint[3]+8,label=paste0("Strength=",
                                                                round(Predict_atpoint[3],2)," psi"),
             color='blue',size=2)

Figure
# png("Figure10-1Relationship between Strength from IDEAL-CT and E50 from 4PB(US).png",
#     height=3, width = 5.8, units = 'in', 
#     type="windows", res=400,family="Arial",pointsize=10)
# plot(Figure)
# dev.off()
png("Example-FigureXXIDT_strengthVsstiffness_inter-USunit(addmix_CI).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#SI unit
model4=lm(IDT_Strength~E50,data=IDTSCB3)
conf_interval=predict(model4,data.frame((E50=IDTSCB3$E50)),interval="confidence",level=0.95)#95% CI

Figure=ggplot(data=IDTSCB3,aes(E50,IDT_Strength))+
  geom_point(aes(E50,IDT_Strength,color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  geom_smooth(method='lm',formula=y~x,linetype="solid",color="black")+    
  geom_line(aes(E50,conf_interval[,2]),linetype="dashed",color="red")+
  geom_line(aes(E50,conf_interval[,3]),linetype="dashed",color="red")+
  #  #stat_summary(geom="line",fun.y = mean)+
  #   #stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("E50 (MPa)")+
  ylab("IDT_Strength (MPa)")+
  annotate(geom="text",x=3800,y=0.7,label='atop("threshold line","(95% CI low bound)")',color="red",parse=T,size=3)+
  annotate(geom="text",x=6000,y=0.75,label='bold("Fail area")',parse=T, color="red")+
  annotate(geom="text",x=4900,y=1.5,label='bold("Pass area")',parse=T,color="blue")+
  # annotate(geom="text",x=750,y=220,label='bold("Pass area")',parse=T,color="blue")+
  # # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))


Figure
png("Figure10-1Relationship between Strength from IDEAL-CT and E50 from 4PB(SI).png",
    height=3, width = 5.8, units = 'in',
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()



#*E50-surface-----------
IDTSCB4=IDTSCB3#new us units
IDTSCB4$IDT_Strength=IDTSCB4$IDT_Strength*145.038#MPa to psi
IDTSCB4$E50=IDTSCB4$E50*0.145038#MPa to ksi
model4=lm(IDT_Strength~E50,data=IDTSCB4)
conf_interval=predict(model4,data.frame((E50=IDTSCB4$E50)),interval="confidence",level=0.95)#95% CI
E50require=210 #ksi for intermediate layer
Predict_atpoint=predict(model4,data.frame(E50=E50require),interval="confidence",level=0.95)
Figure=ggplot(data=IDTSCB4,aes(E50,IDT_Strength))+
  geom_point(aes(E50,IDT_Strength,color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  geom_point(aes(E50require,Predict_atpoint[1]),color="blue",size=1,stroke=1,shape=3)+
  geom_point(aes(E50require,Predict_atpoint[2]),color="blue",size=1,stroke=1,shape=3)+
  geom_point(aes(E50require,Predict_atpoint[3]),color="blue",size=1,stroke=1,shape=3)+
  geom_smooth(method='lm',formula=y~x,linetype="solid",color="black")+    
  #geom_line(aes(E50,conf_interval[,2]),linetype="dashed",color="red")+
  #geom_line(aes(E50,conf_interval[,3]),linetype="dashed",color="red")+
  # stat_summary(geom="line",fun.y = mean)+
  # stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("E50 (ksi)")+
  ylab("IDT_Strength (psi)")+
  # annotate(geom="text",x=3800,y=1,label='atop("threshold line","(95% CI low bound)")',color="red",parse=T,size=3)+
  #annotate(geom="text",x=3000,y=0.8,label='bold("Fail area")',parse=T, color="red")+
  #annotate(geom="text",x=5500,y=2.2,label='bold("Pass area")',parse=T,color="blue")+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))+
  geom_vline(xintercept=E50require,color="blue",linetype="dashed")+
  geom_hline(yintercept=Predict_atpoint[1],color="blue",linetype="dashed")+
  geom_label(x=E50require*2,y=Predict_atpoint[1]-0.1,label=paste0("Strength=",
                                                                     round(Predict_atpoint[1],2)," psi"),
             color="blue",size=2)+
  geom_label(x=E50require*2,y=Predict_atpoint[2]-4,label=paste0("Strength=",
                                                              round(Predict_atpoint[2],2)," psi"),
             color='blue',size=2)+
  geom_label(x=E50require*2,y=Predict_atpoint[3]+4,label=paste0("Strength=",
                                                              round(Predict_atpoint[3],2)," psi"),
             color='blue',size=2)

Figure
png("Example-FigureXXIDT_strengthVsstiffness_surface-USunit(addmix_CI).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()



#*E50-non-PRS new pavement-----------
IDTSCB4=IDTSCB3#new us units
IDTSCB4$IDT_Strength=IDTSCB4$IDT_Strength*145.038#MPa to psi
IDTSCB4$E50=IDTSCB4$E50*0.145038#MPa to ksi
model4=lm(IDT_Strength~E50,data=IDTSCB4)
conf_interval=predict(model4,data.frame((E50=IDTSCB4$E50)),interval="confidence",level=0.95)#95% CI
E50require=547 #1036.4 ksi for non-PRS 7147.5 MPa,#598.23 KSI FOR NON-PRS RHMA
Predict_atpoint=predict(model4,data.frame(E50=E50require),interval="confidence",level=0.95)
Figure=ggplot(data=IDTSCB4,aes(E50,IDT_Strength))+
  geom_point(aes(E50,IDT_Strength,color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  geom_point(aes(E50require,Predict_atpoint[1]),color="blue",size=1,stroke=1,shape=3)+
  geom_point(aes(E50require,Predict_atpoint[2]),color="blue",size=1,stroke=1,shape=3)+
  geom_point(aes(E50require,Predict_atpoint[3]),color="blue",size=1,stroke=1,shape=3)+
  geom_smooth(method='lm',formula=y~x,linetype="solid",color="black")+    
  #geom_line(aes(E50,conf_interval[,2]),linetype="dashed",color="red")+
  #geom_line(aes(E50,conf_interval[,3]),linetype="dashed",color="red")+
  # stat_summary(geom="line",fun.y = mean)+
  # stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("E50 (ksi)")+
  ylab("IDT_Strength (psi)")+
  # annotate(geom="text",x=3800,y=1,label='atop("threshold line","(95% CI low bound)")',color="red",parse=T,size=3)+
  #annotate(geom="text",x=3000,y=0.8,label='bold("Fail area")',parse=T, color="red")+
  #annotate(geom="text",x=5500,y=2.2,label='bold("Pass area")',parse=T,color="blue")+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))+
  geom_vline(xintercept=E50require,color="blue",linetype="dashed")+
  geom_hline(yintercept=Predict_atpoint[1],color="blue",linetype="dashed")+
  geom_label(x=E50require*0.82,y=Predict_atpoint[1]-0.1,label=paste0("Strength=",
                                                                  round(Predict_atpoint[1],2)," psi"),
             color="blue",size=2)+
  geom_label(x=E50require*0.82,y=Predict_atpoint[2]-4,label=paste0("Strength=",
                                                                round(Predict_atpoint[2],2)," psi"),
             color='blue',size=2)+
  geom_label(x=E50require*0.82,y=Predict_atpoint[3]+4,label=paste0("Strength=",
                                                                round(Predict_atpoint[3],2)," psi"),
             color='blue',size=2)

Figure
png("Example-FigureXXIDT_strengthVsstiffness_nonPRS-USunit(addmix_CI).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()
##criterion model based on regression line of StrainNf1M vs IDT_Strength (IDT_Strength as the Y-axis)
#*StrainNf--------
data1us=data1
data1us=data1us[!is.na(data1us$MixLabel)&data1us$strainlevel>1,]
data1us$MIXID=as.character(data1us$MIXID)
data1us$MIXID[data1us$MIXID=="15%RAP with neat binder"]="15% RAP with neat binder"
data1us$MIXID[data1us$MIXID=="15%RAP with PG+X binder"]="15% RAP with AR binder"
data1us$MIXID[data1us$MIXID=="15%RAP with PM binder"]= "15% RAP with PM binder"
data1us$MIXID[data1us$MIXID=="25%RAP with neat binder"]= "25% RAP with neat binder"
data1us$MIXID[data1us$MIXID=="25%RAP with PM binder"]="25% RAP with PM binder"
data1us$MIXID[data1us$MIXID=="20%RAP3%RAS with neat binder"]="20% RAP + 3% RAS with neat binder"
data1us$MIXID[data1us$MIXID=="40%RAP with neat binder"]="40% RAP with neat binder"
data1us$MIXID[data1us$MIXID=="40%RAP with RA"]="40% RAP with RA"
data1us$MIXID[data1us$MIXID=="50%RAP with RA"]="50% RAP with RA"
data1us$MIXID[data1us$MIXID=="0%RAP with AR binder"]="0% RAP with AR binder"
data1us$MIXID[data1us$MIXID=="0%RAP with neat binder"]="0% RAP with neat binder"
data1us$MIXID[data1us$MIXID=="25%RAP with RA"]="25% RAP with RA"
data1us$MIXID[data1us$MIXID=="0%RAP with  AR binder"]="0% RAP with AR binder"
data1us$E50=data1us$E50*0.145038#MPa to ksi
data1us$MIXID=as.factor(data1us$MIXID)
#data1us=data1us[data1us$strainlevel>1,]
#**StrainNf1M vs E50 --------------
#my.formula=y ~ x #y~x
data1us_Dense=data1us
data1us_Dense$Conventional="No"
data1us_Dense$Conventional[data1us_Dense$MIXID=="0% RAP with neat binder"]="Yes"
data1us_Dense$Conventional[data1us_Dense$MIXID=="15% RAP with neat binder"]="Yes"
data1us_Dense$Conventional[data1us_Dense$MIXID=="25% RAP with neat binder"]="Yes"
data1us_Dense$Conventional[data1us_Dense$MIXID=="25% RAP with RA"]="Yes"
data1us_Dense$Conventional=as.factor(data1us_Dense$Conventional)
m=lm(log(StrainNf1M) ~ log(E50), data=data1us)
r2=round(summary(m)$r.squared,2)
Figure=ggplot(data=data1us,aes(x=E50,y=StrainNf1M))+
  geom_point(aes(color=MIXID,shape=MIXID,mixid=MixLabel),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('#000000','#000000','#d63b9a','#d63b9a','#d63b9a','#dd1c77','#980043','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  ylab('StrainNf1M ')+
  xlab('E50(ksi)')+
  guides(fill=guide_legend(title="MixType"))+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='nls',formula=y~a*x^b,method.args=list(start= c(a =2000,b=0)),se=FALSE,linetype="dashed",color="black")+    
  #stat_poly_eq(aes(label = paste(stat(rr.label))), color="black",label.x="center",geom="label",
  #                 formula=my.formula,
  #             parse = TRUE,size=3)+    
  geom_text(aes(x=800,y=800,label=paste("R^2==", r2,sep="")), parse=T)+
  theme(legend.text=element_text(size=10))+
  theme(text=element_text(size=10,  family="Arial",color="black"))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

Figure
png("Figure10-2E50VsStrainNf1M-USunit(addmix).png",#Figure 10-2
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()


Figure=ggplotly(Figure,tooltip=c("x","y","mixid"))
widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}
widget_file_size(Figure)
saveWidget(Figure,"Figure10-2E50VsStrainNf1M-USunit(addmix).html")


#**Add conventional mixes between E50 and StrainNf1M-----

mconvention=lm(log(StrainNf1M) ~ log(E50), data=data1us_Dense[data1us_Dense$Conventional=="Yes",])
r2convention=round(summary(mconvention)$r.squared,2)
Figureconvention=ggplot(data=data1us,aes(x=E50,y=StrainNf1M))+
  geom_point(aes(color=MIXID,shape=MIXID,mixid=MixLabel),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('gray87','#000000','gray87','#d63b9a',
                                             'gray87','gray87','#980043','gray87','#980043',
                                             'gray87','gray87','gray87'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  ylab('StrainNf1M ')+
  xlab('E50(ksi)')+
  guides(fill=guide_legend(title="MixType"))+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='nls',formula=y~a*x^b,data=data1us_Dense[data1us_Dense$Conventional=="Yes",],
              method.args=list(start= c(a =2000,b=0)),se=FALSE,linetype="dashed",color="blue")+    
  #stat_poly_eq(aes(label = paste(stat(rr.label))), color="black",label.x="center",geom="label",
  #                 formula=my.formula,
  #             parse = TRUE,size=3)+    
  geom_text(aes(x=800,y=800,label=paste("R^2==", r2convention,sep="")), parse=T)+
  theme(legend.text=element_text(size=10))+
  theme(text=element_text(size=10,  family="Arial",color="black"))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

Figureconvention
png("Figure10-2E50VsStrainNf1M-USunit(conventionalmix).png",#Figure 10-2
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figureconvention)
dev.off()


munconvention=lm(log(StrainNf1M) ~ log(E50), data=data1us_Dense[data1us_Dense$Conventional=="No",])
r2un=round(summary(munconvention)$r.squared,2)
Figureunconvention=ggplot(data=data1us_Dense,
                          aes(x=E50,y=StrainNf1M))+
  geom_point(aes(color=MIXID,shape=MIXID,mixid=MixLabel),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('#000000','gray87','#d63b9a','gray87',
                                             '#d63b9a','#dd1c77','gray87','#980043',
                                             'gray87','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  ylab('StrainNf1M ')+
  xlab('E50(ksi)')+
  guides(fill=guide_legend(title="MixType"))+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='nls',data=data1us_Dense[data1us_Dense$Conventional=="No",],
              formula=y~a*x^b,method.args=list(start= c(a =2000,b=0)),se=FALSE,
              linetype="dashed",color="#d2006c")+    
  #stat_poly_eq(aes(label = paste(stat(rr.label))), color="black",label.x="center",geom="label",
  #                 formula=my.formula,
  #             parse = TRUE,size=3)+    
  geom_text(aes(x=800,y=800,label=paste("R^2==", r2un,sep="")), parse=T)+
  theme(legend.text=element_text(size=10))+
  theme(text=element_text(size=10,  family="Arial",color="black"))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

Figureunconvention
png("Figure10-2E50VsStrainNf1M-USunit(unconventionalmix).png",#Figure 10-2
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figureunconvention)
dev.off()

##***SI unit----
data1SI=data1us
data1SI$E50=data1SI$E50/0.145038#ksi to MPa
m=lm(log(StrainNf1M) ~ log(E50), data=data1SI)
r2=round(summary(m)$r.squared,2)
Figure=ggplot(data=data1SI,aes(x=E50,y=StrainNf1M))+
  geom_point(aes(color=MIXID,shape=MIXID,mixid=MixLabel),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('#000000','#000000','#d63b9a','#d63b9a','#d63b9a','#dd1c77','#980043','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  ylab('StrainNf1M ')+
  xlab('E50(MPa)')+
  guides(fill=guide_legend(title="MixType"))+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='nls',formula=y~a*x^b,method.args=list(start= c(a =2000,b=0)),se=FALSE,linetype="dashed",color="black")+    
  #stat_poly_eq(aes(label = paste(stat(rr.label))), color="black",label.x="center",geom="label",
  #                 formula=my.formula,
  #             parse = TRUE,size=3)+    
  geom_text(aes(x=6000,y=800,label=paste("R^2==", r2,sep="")), parse=T)+
  theme(legend.text=element_text(size=10))+
  theme(text=element_text(size=10,  family="Arial",color="black"))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

Figure
png("Figure10-2E50VsStrainNf1M(addmix).png",#Figure 10-2
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

##****take out nonCaltrans mixes----
data1SI2=data1SI[!data1SI$MixLabel %in% c("4.64-MIXA-0H","4.64-MIXA-6H","4.64-MIXB-0H",
                                           "4.64-MIXB-16H","4.64-MIXC-0H","4.64-MIXC-5H",
                                           "4.64-MIXD-0H","4.64-MIXD-16H"),]
data1SI2=droplevels(data1SI2)
m=lm(log(StrainNf1M) ~ log(E50), data=data1SI2)
r2=round(summary(m)$r.squared,2)
Figure=ggplot(data=data1SI2,aes(x=E50,y=StrainNf1M))+
  geom_point(aes(color=MIXID,shape=MIXID,mixid=MixLabel),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,23,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('#000000','#000000','#d63b9a','#d63b9a','#d63b9a','#980043',
                                             '#980043','#980043','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  ylab('StrainNf1M ')+
  xlab('E50(MPa)')+
  guides(fill=guide_legend(title="MixType"))+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='nls',formula=y~a*x^b,method.args=list(start= c(a =2000,b=0)),se=FALSE,linetype="dashed",color="black")+    
  #stat_poly_eq(aes(label = paste(stat(rr.label))), color="black",label.x="center",geom="label",
  #                 formula=my.formula,
  #             parse = TRUE,size=3)+    
  geom_text(aes(x=6000,y=800,label=paste("R^2==", r2,sep="")), parse=T)+
  theme(legend.text=element_text(size=10))+
  theme(text=element_text(size=10,  family="Arial",color="black"))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

Figure
png("Figure10-2E50VsStrainNf1M(addmix)_CaltranMIX.png",#Figure 10-2
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#Add conventional mixes between E50 and StrainNf1M
data1SI_Dense=data1us_Dense
data1SI_Dense$E50=data1SI_Dense$E50/0.145038#ksi to MPa
mconvention=lm(log(StrainNf1M) ~ log(E50), data=data1SI_Dense[data1SI_Dense$Conventional=="Yes",])
r2convention=round(summary(mconvention)$r.squared,2)
Figureconvention=ggplot(data=data1SI,aes(x=E50,y=StrainNf1M))+
  geom_point(aes(color=MIXID,shape=MIXID,mixid=MixLabel),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('gray87','#000000','gray87','#d63b9a',
                                             'gray87','gray87','#980043','gray87','#980043',
                                             'gray87','gray87','gray87'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  ylab('StrainNf1M ')+
  xlab('E50(MPa)')+
  guides(fill=guide_legend(title="MixType"))+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='nls',formula=y~a*x^b,data=data1SI_Dense[data1SI_Dense$Conventional=="Yes",],
              method.args=list(start= c(a =2000,b=0)),se=FALSE,linetype="dashed",color="blue")+    
  #stat_poly_eq(aes(label = paste(stat(rr.label))), color="black",label.x="center",geom="label",
  #                 formula=my.formula,
  #             parse = TRUE,size=3)+    
  geom_text(aes(x=6000,y=800,label=paste("R^2==", r2convention,sep="")), parse=T)+
  theme(legend.text=element_text(size=10))+
  theme(text=element_text(size=10,  family="Arial",color="black"))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

Figureconvention
png("Figure10-2E50VsStrainNf1M(conventionalmix).png",#Figure 10-2
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figureconvention)
dev.off()


munconvention=lm(log(StrainNf1M) ~ log(E50), data=data1SI_Dense[data1SI_Dense$Conventional=="No",])
r2un=round(summary(munconvention)$r.squared,2)
Figureunconvention=ggplot(data=data1SI_Dense,
                          aes(x=E50,y=StrainNf1M))+
  geom_point(aes(color=MIXID,shape=MIXID,mixid=MixLabel),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('#000000','gray87','#d63b9a','gray87',
                                             '#d63b9a','#dd1c77','gray87','#980043',
                                             'gray87','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  ylab('StrainNf1M ')+
  xlab('E50(MPa)')+
  guides(fill=guide_legend(title="MixType"))+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='nls',data=data1SI_Dense[data1SI_Dense$Conventional=="No",],
              formula=y~a*x^b,method.args=list(start= c(a =2000,b=0)),se=FALSE,
              linetype="dashed",color="#d2006c")+    
  #stat_poly_eq(aes(label = paste(stat(rr.label))), color="black",label.x="center",geom="label",
  #                 formula=my.formula,
  #             parse = TRUE,size=3)+    
  geom_text(aes(x=6000,y=800,label=paste("R^2==", r2un,sep="")), parse=T)+
  theme(legend.text=element_text(size=10))+
  theme(text=element_text(size=10,  family="Arial",color="black"))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

Figureunconvention
png("Figure10-2E50VsStrainNf1M(unconventionalmix).png",#Figure 10-2
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figureunconvention)
dev.off()

##take out nonCaltrans mixes
data1SI_Dense2=data1SI_Dense[!data1SI_Dense$MixLabel %in% c("4.64-MIXA-0H","4.64-MIXA-6H","4.64-MIXB-0H",
                                          "4.64-MIXB-16H","4.64-MIXC-0H","4.64-MIXC-5H",
                                          "4.64-MIXD-0H","4.64-MIXD-16H"),]
data1SI_Dense2=droplevels(data1SI_Dense2)
munconvention=lm(log(StrainNf1M) ~ log(E50), data=data1SI_Dense2[data1SI_Dense2$Conventional=="No",])
r2un=round(summary(munconvention)$r.squared,2)
Figureunconvention=ggplot(data=data1SI_Dense2,
                          aes(x=E50,y=StrainNf1M))+
  geom_point(aes(color=MIXID,shape=MIXID,mixid=MixLabel),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,23,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('#000000','gray87','#d63b9a','gray87',
                                             '#d63b9a','gray87','#980043',
                                             'gray87','#54278f'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  ylab('StrainNf1M ')+
  xlab('E50(MPa)')+
  guides(fill=guide_legend(title="MixType"))+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='nls',data=data1SI_Dense[data1SI_Dense$Conventional=="No",],
              formula=y~a*x^b,method.args=list(start= c(a =2000,b=0)),se=FALSE,
              linetype="dashed",color="#d2006c")+    
  #stat_poly_eq(aes(label = paste(stat(rr.label))), color="black",label.x="center",geom="label",
  #                 formula=my.formula,
  #             parse = TRUE,size=3)+    
  geom_text(aes(x=6000,y=800,label=paste("R^2==", r2un,sep="")), parse=T)+
  theme(legend.text=element_text(size=10))+
  theme(text=element_text(size=10,  family="Arial",color="black"))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

Figureunconvention
png("Figure10-2E50VsStrainNf1M(unconventionalmix)-CaltranMIX.png",#Figure 10-2
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figureunconvention)
dev.off()


#**StrainNf0.25M vs E50 --------------
n=lm(log(StrainNfQM) ~ log(E50), data=data1us)
r2n=round(summary(n)$r.squared,2)
Figure=ggplot(data1us,aes(E50,StrainNfQM))+
  geom_point(aes(color=MIXID,shape=MIXID,mixid=MixLabel),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('#000000','#000000','#d63b9a','#d63b9a',
                              '#d63b9a','#dd1c77','#980043','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  ylab('StrainNf0.25M ')+
  xlab('E50(ksi)')+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='nls',formula=y~a*x^b,method.args=list(start= c(a =2000,b=0)),se=FALSE,linetype="dashed",color="black")+    
  # stat_poly_eq(formula = my.formula,aes(label = paste(..rr.label..)), color="black",label.x="center",geom="label",
  #              parse = TRUE,size=3)+    
  geom_text(aes(x=800,y=800,label=paste("R^2==", r2n,sep="")), parse=T)+
  theme(legend.text=element_text(size=10))+
  theme(text=element_text(size=10,  family="Arial",color="black"))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

Figure
png("FigureXXE50VsStrainNf0.25M-usunit(addmix).png",#Figure 10-2
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()
Figure=ggplotly(Figure,tooltip=c("x","y","mixid"))
widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}
widget_file_size(Figure)
saveWidget(Figure,"FigureXXE50VsStrainNf0.25M-usunit(addmix).html")

#*StrainCritera(maximum strength, minimumStrainNf1M)-----------
model5=lm(log(E50)~log(StrainNf1M),data=data1us)#StrainNf1m=220
StrainNf1Mrequire=220
Predict_atpoint2=predict(model5,data.frame(StrainNf1M=StrainNf1Mrequire),interval="confidence",level=0.95)
Figure=ggplot(data1us,aes(log(StrainNf1M),log(E50)))+
  geom_point(aes(color=MIXID,shape=MIXID),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('#000000','#000000','#d63b9a','#d63b9a','#d63b9a',
                              '#dd1c77','#980043','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  geom_point(aes(log(StrainNf1Mrequire),(Predict_atpoint2[1])),color="blue",size=6,stroke=2,shape=3)+
  geom_vline(xintercept=log(StrainNf1Mrequire),color="blue",linetype="dashed")+
  geom_hline(yintercept=(Predict_atpoint2[1]),color="blue",linetype="dashed")+
  geom_label(x=log(StrainNf1Mrequire)+0.5,y=(Predict_atpoint2[1]),label=paste0("E50=",round(exp(Predict_atpoint2[1]),2)," ksi"),color="blue",size=3)+
  ylab('ln(E50) (ksi)')+
  xlab('ln(StrainNf1M)')+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='lm',formula=y~x,se=FALSE,linetype="dashed",color="black")+  # stat_poly_eq(formula = my.formula,aes(label = paste(..rr.label..)), color="black",label.x="center",geom="label",
  #              parse = TRUE,size=3)+    
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
Figure
png("Example-FigureXXStrainNf1MVSstiffness_inter(USunit)(addmix).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()


E50require=exp(Predict_atpoint2[1]) #ksi for intermediate layer
Predict_atpoint=predict(model4,data.frame(E50=E50require),interval="confidence",level=0.95)
Figure=ggplot(data=IDTSCB4,aes(E50,IDT_Strength))+
  geom_point(aes(E50,IDT_Strength,color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  geom_point(aes(E50require,Predict_atpoint[1]),color="blue",size=6,stroke=2,shape=3)+
  geom_smooth(method='lm',formula=y~x,se=FALSE,linetype="solid",color="black")+    
  #geom_line(aes(E50,conf_interval[,2]),linetype="dashed",color="red")+
  #geom_line(aes(E50,conf_interval[,3]),linetype="dashed",color="red")+
  # stat_summary(geom="line",fun.y = mean)+
  # stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("E50 (ksi)")+
  ylab("IDT_Strength (psi)")+
  # annotate(geom="text",x=3800,y=1,label='atop("threshold line","(95% CI low bound)")',color="red",parse=T,size=3)+
  #annotate(geom="text",x=3000,y=0.8,label='bold("Fail area")',parse=T, color="red")+
  #annotate(geom="text",x=5500,y=2.2,label='bold("Pass area")',parse=T,color="blue")+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))+
  geom_vline(xintercept=E50require,color="blue",linetype="dashed")+
  geom_hline(yintercept=Predict_atpoint[1],color="blue",linetype="dashed")+
  geom_label(x=E50require*0.66,y=Predict_atpoint[1]-0.1,label=paste0("Strength=",round(Predict_atpoint[1],2)," psi"),color="blue",size=3)


Figure
png("Example-FigureXXIDT_strengthVsNf_inter-USunit(addmix).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(Figure)
dev.off()

#***directly between strength vs Nf1M intermediate layer--------
model6=lm(log(IDT_Strength)~log(StrainNf1M),data=IDTSCB4)#StrainNf1m=220
StrainNf1Mrequire=220
Predict_atpoint22=predict(model6,data.frame(StrainNf1M=StrainNf1Mrequire),interval="confidence",level=0.95)
FigureStrengthNf=ggplot(IDTSCB4,aes(log(StrainNf1M),log(IDT_Strength)))+
  geom_point(aes(color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('#000000','#000000','#d63b9a','#d63b9a','#d63b9a',
                                             '#dd1c77','#980043','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  geom_point(aes(log(StrainNf1Mrequire),(Predict_atpoint22[1])),color="blue",size=1,shape=3)+
  geom_point(aes(log(StrainNf1Mrequire),(Predict_atpoint22[2])),color="blue",size=1,shape=3)+
  geom_point(aes(log(StrainNf1Mrequire),(Predict_atpoint22[3])),color="blue",size=1,shape=3)+
  geom_vline(xintercept=log(StrainNf1Mrequire),color="blue",linetype="dashed")+
  geom_hline(yintercept=(Predict_atpoint22[1]),color="blue",linetype="dashed")+
  geom_label(x=log(StrainNf1Mrequire)+0.5,y=(Predict_atpoint22[1]),label=paste0("Strength=",round(exp(Predict_atpoint22[1]),2)," psi"),color="blue",size=2)+
  geom_label(x=log(StrainNf1Mrequire),y=(Predict_atpoint22[2]-0.05),label=paste0("Strength=",round(exp(Predict_atpoint22[2]),2)," psi"),color="blue",size=2)+
  geom_label(x=log(StrainNf1Mrequire),y=(Predict_atpoint22[3]+0.05),label=paste0("Strength=",round(exp(Predict_atpoint22[3]),2)," psi"),color="blue",size=2)+
  
  ylab('ln(IDT_Strength) (psi)')+
  xlab('ln(StrainNf1M)')+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+  # stat_poly_eq(formula = my.formula,aes(label = paste(..rr.label..)), color="black",label.x="center",geom="label",
  #              parse = TRUE,size=3)+    
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
FigureStrengthNf
png("Example-FigureXXStrainNf1MVSstiffness_inter(USunit)(addmix_CI).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthNf)
dev.off()


#***directly between strength vs Nf1M surface layer -------
model6=lm(log(IDT_Strength)~log(StrainNf1M),data=IDTSCB4)#StrainNf1m=220
StrainNf1Mrequire=495
Predict_atpoint22=predict(model6,data.frame(StrainNf1M=StrainNf1Mrequire),interval="confidence",level=0.95)
FigureStrengthNf=ggplot(IDTSCB4,aes(log(StrainNf1M),log(IDT_Strength)))+
  geom_point(aes(color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('#000000','#000000','#d63b9a','#d63b9a','#d63b9a',
                                             '#dd1c77','#980043','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  geom_point(aes(log(StrainNf1Mrequire),(Predict_atpoint22[1])),color="blue",size=1,shape=3)+
  geom_point(aes(log(StrainNf1Mrequire),(Predict_atpoint22[2])),color="blue",size=1,shape=3)+
  geom_point(aes(log(StrainNf1Mrequire),(Predict_atpoint22[3])),color="blue",size=1,shape=3)+
  geom_vline(xintercept=log(StrainNf1Mrequire),color="blue",linetype="dashed")+
  geom_hline(yintercept=(Predict_atpoint22[1]),color="blue",linetype="dashed")+
  geom_label(x=log(StrainNf1Mrequire)-0.3,y=(Predict_atpoint22[1]),label=paste0("Strength=",round(exp(Predict_atpoint22[1]),2)," psi"),color="blue",size=2)+
  geom_label(x=log(StrainNf1Mrequire),y=(Predict_atpoint22[2]-0.05),label=paste0("Strength=",round(exp(Predict_atpoint22[2]),2)," psi"),color="blue",size=2)+
  geom_label(x=log(StrainNf1Mrequire),y=(Predict_atpoint22[3]+0.05),label=paste0("Strength=",round(exp(Predict_atpoint22[3]),2)," psi"),color="blue",size=2)+
  
  ylab('ln(IDT_Strength) (psi)')+
  xlab('ln(StrainNf1M)')+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+  # stat_poly_eq(formula = my.formula,aes(label = paste(..rr.label..)), color="black",label.x="center",geom="label",
  #              parse = TRUE,size=3)+    
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
FigureStrengthNf
png("Example-FigureXXStrainNf1MVSstiffness_surface(USunit)(addmix_CI).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthNf)
dev.off()


#***directly between strength vs Nf1M nonRPS -------
m=lm(log(StrainNf1M) ~ log(E50), data=data1us)
StrainNf1Mrequire=exp(predict(m,data.frame(E50=(547)),interval="confidence",level=0.95))
model6=lm(log(IDT_Strength)~log(StrainNf1M),data=IDTSCB4)#StrainNf1m=220

Predict_atpoint22=predict(model6,data.frame(StrainNf1M=StrainNf1Mrequire[1]),interval="confidence",level=0.95)
FigureStrengthNf=ggplot(IDTSCB4,aes(log(StrainNf1M),log(IDT_Strength)))+
  geom_point(aes(color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  scale_shape_manual(name="MixType",values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(name="MixType",values=c('#000000','#000000','#d63b9a','#d63b9a','#d63b9a',
                                             '#dd1c77','#980043','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  geom_point(aes(log(StrainNf1Mrequire[1]),(Predict_atpoint22[1])),color="blue",size=1,shape=3)+
  geom_point(aes(log(StrainNf1Mrequire[1]),(Predict_atpoint22[2])),color="blue",size=1,shape=3)+
  geom_point(aes(log(StrainNf1Mrequire[1]),(Predict_atpoint22[3])),color="blue",size=1,shape=3)+
  geom_vline(xintercept=log(StrainNf1Mrequire[1]),color="blue",linetype="dashed")+
  geom_hline(yintercept=(Predict_atpoint22[1]),color="blue",linetype="dashed")+
  geom_label(x=log(StrainNf1Mrequire[1])-0.3,y=(Predict_atpoint22[1]),label=paste0("Strength=",round(exp(Predict_atpoint22[1]),2)," psi"),color="blue",size=2)+
  geom_label(x=log(StrainNf1Mrequire[1]),y=(Predict_atpoint22[2]-0.05),label=paste0("Strength=",round(exp(Predict_atpoint22[2]),2)," psi"),color="blue",size=2)+
  geom_label(x=log(StrainNf1Mrequire[1]),y=(Predict_atpoint22[3]+0.05),label=paste0("Strength=",round(exp(Predict_atpoint22[3]),2)," psi"),color="blue",size=2)+
  
  ylab('ln(IDT_Strength) (psi)')+
  xlab('ln(StrainNf1M)')+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black")+  # stat_poly_eq(formula = my.formula,aes(label = paste(..rr.label..)), color="black",label.x="center",geom="label",
  #              parse = TRUE,size=3)+    
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
FigureStrengthNf
png("Example-FigureXXStrainNf1MVSstiffness_nonPRS(USunit)(addmix_CI).png",
    height=3, width = 5.8, units = 'in', 
    type="windows", res=400,family="Arial",pointsize=10)
plot(FigureStrengthNf)
dev.off()





my.formula=log(IDTSCB3$IDT_Strength)~log(IDTSCB3$StrainNf1M)
#ThresholdIDT_Strength=model5$coefficients[1]+model5$coefficients[2]*5.6
summary(model5)
#PROP1 <- predictNLS(model5, data.frame(StrainNf1M=IDTSCB2$StrainNf1M), interval = "confidence",alpha=0.05)
conf_interval2=predict(model5,data.frame((StrainNf1M=IDTSCB3$StrainNf1M)),interval="confidence",level=0.95)
#predict_atpoint=predict(model5,data.frame(StrainNf1M=269),interval="prediction",level=0.95)
Figure=ggplot(IDTSCB3,aes(log(StrainNf1M),log(IDT_Strength)))+
  geom_point(aes(log(StrainNf1M),log(IDT_Strength), color=MixType,shape=MixType),size=2.5,stroke=0.5)+
  geom_smooth(method='lm',formula=y~x,se=FALSE,linetype="solid",color="black")+    
  geom_line(aes(log(StrainNf1M),conf_interval2[,2]),linetype="dashed",color="red")+
  geom_line(aes(log(StrainNf1M),conf_interval2[,3]),linetype="dashed",color="red")+
  #stat_poly_eq(formula = my.formula, 
  #             aes(label = paste0("atop(",..eq.label..,",", ..rr.label.., ")")), 
  #             parse = TRUE,label.x = 6,label.y=1) +  
  # stat_summary(geom="line",fun.y = mean)+
  # stat_summary(geom="ribbon",fun.data=mean_cl_boot,
  #              conf.int=0.95,alpha=0.0,linetype="dashed",color="red")+
  scale_shape_manual(values=c(15,16,17,23,16,16,23,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#df65b0','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f'
                              ,'#238443'))+###color represents RAP content
  xlab("ln(StrainNf1M) (MPa)")+
  ylab("ln(IDT_Strength)(MPa)")+
  # scale_y_continuous(breaks=seq(2000, 8000,1000), label=comma,limits=c(2000,8000))+#+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  #ylim(1200,12200)+
  annotate(geom="text",x=5.5,y=0.8,label='atop("threshold line","(95% CI up bound)")',color="red",parse=T,size=3)+
  annotate(geom="text",x=5.3,y=1,label='bold("Fail area")',parse=T, color="red")+
  annotate(geom="text",x=5.8,y=0.1,label='bold("Pass area")',parse=T,color="blue")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
# geom_point(aes(x=5.6,y=ThresholdIDT_Strength),color="blue",cex=3,shape=19)+
# geom_point(aes(x=5.6,y=predict_atpoint[2]),color="blue",cex=3,shape=3)+
# geom_point(aes(x=5.6,y=predict_atpoint[3]),color="blue",cex=3,shape=3)+
# geom_vline(xintercept=5.6,color="blue",linetype="dashed")+
# annotate(geom="text",x=5.4,y=0.39,label='atop("threshold line","(95% CI low bound)")',color="red",parse=T,size=3)
Figure


# *strength vs E50 --------------------------------------------------------

E50vsS=ggplot(IDTSCB4,aes(x=IDT_Strength,y=E50))+
  geom_point(aes(color=MixType,shape=MixType,mixid=MIX.ID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,16,23,16,16,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#df65b0','#df65b0','#dd1c77','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content
  xlab("IDT_Strength (psi)")+
  ylab("E50 (ksi)")+
  # scale_y_continuous(breaks=seq(1600, 8000,800), label=comma,limits=c(1600,8000))+#+
  geom_smooth(method='lm',formula=y~x,linetype="dashed",color="black",se=FALSE)+
 # stat_poly_eq(formula = y~x,aes(label = paste(..rr.label..)), color="black",
  #              parse = TRUE,size=3)+theme_bw()+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))
E50vsSplotly=ggplotly(E50vsS,tooltip=c("x","y","mixid"))
widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}
widget_file_size(E50vsSplotly)
saveWidget(E50vsSplotly,"E50vsIDT_Strength-usunit(addmix).html")

# StrainNf1M vs E50
my.formula=log(data1us$StrainNf1M)~log(data1us$E50) #y~x
StrainNf1MVS50=ggplot(data1us,aes(x=E50,y=StrainNf1M))+
  geom_point(aes(mixid=MixLabel,color=MIXID,shape=MIXID),size=2.5,stroke=0.5)+
  scale_shape_manual(values=c(15,16,15,16,23,16,16,23,25,16,25,25))+###shape represents binder 
  scale_color_manual(values=c('#000000','#000000','#d63b9a','#d63b9a','#d63b9a','#dd1c77','#980043','#980043','#980043','#54278f','#54278f','#238443'))+###color represents RAP content  xlab("IDT_Strength (MPa)")+  ylab("StrainNf1M")+
  ylab('StrainNf1M ')+
  xlab('E50(ksi)')+
  #scale_y_continuous(breaks=seq(0, 900,200), limits=c(0,900))+
  font("xy.text",size=10,color="black")+
  font("xlab",size=10,color="black")+
  font("ylab",size=10,color="black")+
  geom_smooth(method='nls',formula=y~a*x^b,method.args=list(start= c(a =2000,b=0)),se=FALSE,linetype="dashed",color="black")+    
  stat_poly_eq(formula = my.formula,aes(label = paste(..rr.label..)), color="black",label.x="center",geom="label",
               parse = TRUE,size=3)+    
  theme(legend.text=element_text(size=10))+
  theme(axis.ticks.length=unit(-1, "mm"),axis.ticks=element_line(size=0.6))

StrainNf1MvsE50plotly=ggplotly(StrainNf1MVS50,tooltip=c("x","y","mixid"))

widget_file_size(StrainNf1MvsE50plotly)
saveWidget(StrainNf1MvsE50plotly,"E50vsStrainNf1M-usunit(addmix).html")
