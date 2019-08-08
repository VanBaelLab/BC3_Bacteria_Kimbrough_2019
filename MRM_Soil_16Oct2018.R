#MRM on BC3 SOIL data using weighted rooted unifrac matrix and new USGS Data August2018
#read in data (BUT THIS IS OLD??)
Soil.d<-read.csv(file = "MRM_Soil/BC3_Bacteria_PlotAggregate_soil.csv")

Long=Soil.d$Long
Lat=Soil.d$Lat

#load geosphere
library(geosphere)
library(ecodist)
#install.packages("ecodist")
library(ecodist)
#install.packages("geosphere")

#calculate physical distance, make sure that the order
#is Longitude first, them Latitude and in degrees
#default is meters
geodist.sl<-distm(cbind(Long, Lat), fun = distGeo) 
geodist.sl2<-as.dist(log(geodist.sl/1000),diag = FALSE, upper = FALSE) 




#spp.dist.sl_SoilBact<-bcdist(spp.mat, rmzero = FALSE)

#1. read in the unifrac
x <- read.table("MRM_Soil/weighted_rootedunifrac_Soil_PlotAggregate_otu.txt")
UnifracMatrixSoil <- as.dist(as(x, "matrix"))

#include other distance matrices to account for soil and 
#other environmental properties

sal.dist<-dist(Soil.d$MeanSal_6mo, method="euclidean")
h20.dist<-dist(Soil.d$WaterLevel, method="euclidean")
TreeDensity<-dist(Soil.d$TreeDensity, method="euclidean")


#based on multiple regression on matrices
MRM(UnifracMatrixSoil~geodist.sl2 +sal.dist+h20.dist, nperm = 1000)

#make plots
plot(x=geodist.sl2, y=UnifracMatrixSoil, pch=2)
abline(lm(UnifracMatrixSoil~geodist.sl2), col="black") 

#make ggplot-----


#convert dist matrices to pairwise lists

gdist.list <- data.frame(t(combn(rownames(x),2)), as.numeric(geodist.sl2))
names(gdist.list) <- c("c1", "c2", "geo_distance")

UFdist.list <- data.frame(t(combn(rownames(x),2)), as.numeric(UnifracMatrixSoil))
names(UFdist.list) <- c("c1", "c2", "UF_distance")

which(UFdist.list$c1!=gdist.list$c1)
which(UFdist.list$c2!=gdist.list$c2)

dist.df <- data.frame("plot1" = UFdist.list$c1, "plot2" = UFdist.list$c2, "geo_dist" = gdist.list$geo_distance, "UF_dist" = UFdist.list$UF_distance)

PW.comps <- with(dist.df, ifelse((plot1=="BC" | plot1=="BM" | plot1=="BT") & (plot2=="FL" | plot2=="TI" | plot2=="JL"), "Terr-Barr",
                   ifelse((plot1=="BC" | plot1=="BM" | plot1=="BT") & (plot2=="SL" | plot2=="SM" | plot2=="SU"), "Terr-Sav",
                          ifelse((plot1=="BC" | plot1=="BM" | plot1=="BT") & (plot2=="WL" | plot2=="WM" | plot2=="WU"), "Terr-Wac", 
                                 ifelse((plot1=="FL" | plot1=="TI" | plot1=="JL") & (plot2=="SL" | plot2=="SM" | plot2=="SU"), "Barr-Sav", 
                                        ifelse((plot1=="FL" | plot1=="TI" | plot1=="JL") & (plot2=="WL" | plot2=="WM" | plot2=="WU"), "Barr-Wac", 
                                               ifelse((plot1=="SL" | plot1=="SM" | plot1=="SU") & (plot2=="WL" | plot2=="WM" | plot2=="WU"), "Sav-Wac", 
                                                      ifelse((plot1=="FL" | plot1=="TI" | plot1=="JL") & (plot2=="BC" | plot2=="BM" | plot2=="BT"), "Terr-Barr",
                                                             ifelse((plot1=="SL" | plot1=="SM" | plot1=="SU") & (plot2=="BC" | plot2=="BM" | plot2=="BT"), "Terr-Barr",
                                                                    ifelse((plot1=="JL" | plot1=="TI" | plot1=="FL") & (plot2=="JL" | plot2=="TI" | plot2=="FL"), "Barr-Barr",
                                                                           ifelse((plot1=="SU" | plot1=="SL" | plot1=="SM") & (plot2=="SU" | plot2=="SL" | plot2=="SM"), "Sav-Sav",
                                                                                  ifelse((plot1=="BC" | plot1=="BM" | plot1=="BT") & (plot2=="BC" | plot2=="BM" | plot2=="BT"), "Terr-Terr",
                                                                                         ifelse((plot1=="WL" | plot1=="WM" | plot1=="WU") & (plot2=="WL" | plot2=="WM" | plot2=="WU"), "Wac-Wac", "Failed")))))))))))))

PW.comps
dist.df[PW.comps=="Failed",]

dist.df$PW.comps <- PW.comps

#make pairwise by state
PW.comps.state <- with(dist.df, ifelse((plot1=="BC" | plot1=="BM" | plot1=="BT") & (plot2=="FL" | plot2=="TI" | plot2=="JL"), "LA-LA",
                                       ifelse((plot1=="BC" | plot1=="BM" | plot1=="BT") & (plot2=="SL" | plot2=="SM" | plot2=="SU"), "Terr-Sav",
                                              ifelse((plot1=="BC" | plot1=="BM" | plot1=="BT") & (plot2=="WL" | plot2=="WM" | plot2=="WU"), "LA-SC", 
                                                     ifelse((plot1=="FL" | plot1=="TI" | plot1=="JL") & (plot2=="SL" | plot2=="SM" | plot2=="SU"), "LA-GA", 
                                                            ifelse((plot1=="FL" | plot1=="TI" | plot1=="JL") & (plot2=="WL" | plot2=="WM" | plot2=="WU"), "LA-SC", 
                                                                   ifelse((plot1=="SL" | plot1=="SM" | plot1=="SU") & (plot2=="WL" | plot2=="WM" | plot2=="WU"), "GA-SC", 
                                                                          ifelse((plot1=="FL" | plot1=="TI" | plot1=="JL") & (plot2=="BC" | plot2=="BM" | plot2=="BT"), "LA-LA",
                                                                                 ifelse((plot1=="SL" | plot1=="SM" | plot1=="SU") & (plot2=="BC" | plot2=="BM" | plot2=="BT"), "LA-LA",
                                                                                        ifelse((plot1=="JL" | plot1=="TI" | plot1=="FL") & (plot2=="JL" | plot2=="TI" | plot2=="FL"), "LA-LA",
                                                                                               ifelse((plot1=="SU" | plot1=="SL" | plot1=="SM") & (plot2=="SU" | plot2=="SL" | plot2=="SM"), "GA-GA",
                                                                                                      ifelse((plot1=="BC" | plot1=="BM" | plot1=="BT") & (plot2=="BC" | plot2=="BM" | plot2=="BT"), "LA-LA",
                                                                                                             ifelse((plot1=="WL" | plot1=="WM" | plot1=="WU") & (plot2=="WL" | plot2=="WM" | plot2=="WU"), "SC-SC", "Failed")))))))))))))

PW.comps.state

dist.df$PW.comps.state <- PW.comps.state

#calculate coefficients for abline
CF <- coef(lm(UnifracMatrixSoil~geodist.sl2))

library(ggplot2)

MRM_plot <- ggplot(data = dist.df, aes(x = geo_dist, y = UF_dist, shape = PW.comps.state)) +
  geom_point(size = (rel(4))) +
  geom_abline(slope = CF[2], intercept = CF[1]) +
  theme_bw() +
  theme(axis.title = element_text(size=14)) +
  xlab("Geographic Distance log(km)") +
  ylab("UniFrac Distance") +
  guides(shape=guide_legend(title=NULL))

MRM_plot


ggsave("MRM_Soil/MRM_plot_3Dec.png", height = 8, width = 8, units = "in")
