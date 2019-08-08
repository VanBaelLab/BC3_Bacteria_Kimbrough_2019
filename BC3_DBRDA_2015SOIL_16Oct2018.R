#DB-RDA on rarified OTU table  16 October 2018
#2015 soil ONLY

#load vegan
library(vegan)


#load commuity data matrix as object. 
BC3_OTU<-read.csv("BC3_OTU_SOIL_2015.csv")

otu.spp<-names(BC3_OTU[grep("derep_", names(BC3_OTU))])
bcr<-BC3_OTU[,otu.spp]

# Make Bray Curtis dissimilarity matrix 
BC3BrayCurtisMatrix<-vegdist(bcr, method="bray", binary=FALSE)
#load environmental data as an object
BC3_Enviro<- read.csv("BC3_ENVIRO_SOIL_2015.csv")

#dbrda with all environmental data
#FullModel
#dbRDA_all_<-capscale(BC3BrayCurtisMatrix ~ Type+Year+Site+Lat+Long+Transect+Plot+Degradation+HistTidal+WoodyCov_z+ForestCover_z+TreeDensity_2014_z+PlotDensity_z+ForestHeight_z+WaterLevel_z+MeanSal_6mo_z+MeanTemp_6mo_z+MeanCond_z, data=BC3_Enviro)

dbRDA_all_<-capscale(BC3BrayCurtisMatrix ~ WoodyCov_z+TreeDensity_2014_z+WaterLevel_z+MeanSal_6mo_z, data=BC3_Enviro)

#Null Model
#generate one model with NOTHING to explain the unifrac dm matrix
dbRDAnull_BC3<-capscale(BC3BrayCurtisMatrix~1,data=BC3_Enviro)
dbRDAnull_BC3

#use forward selection to choose which elements of the full model explain a significant amount of variaiton in the unifrac dm by comparing it to the null
#I have played around with changing the pstep # 
dbRDA_forsel<-ordistep(dbRDAnull_BC3,scope=formula(dbRDA_all_),direction="forward",Pin=.1,trace=T,pstep=200000)
dbRDA_forsel

plot(dbRDA_forsel)
#this makes a very ugly plot

anova(dbRDA_forsel, by="margin")
#This ANOVA shows the significance per variable that contributed to the model? 


