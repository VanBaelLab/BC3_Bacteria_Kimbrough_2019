# Oct 16 2018
#updated June 24, 2019 by Steve Formel

#ROOT DBRDA run removing site/trnsect, etc. from model selection


#DB-RDA on rarified OTU table and new environmental data: 16 Sept 2018



#load libraries and data and clean data--------
library(vegan)
library(ggplot2)
library(cowplot)

#load commuity data matrix as object. 
BC3_OTU<-read.csv("DBRDA_Roots/BC3_OTU_ROOT.csv", row.names = 1)


#fix enviro data

#steve remade this from BC3_Enviro.csv because something weird happened to it, not sure what.  Then he removed LA.R.25 because it had been removed in previous analyses according to his emails with Liz.

#These lines only needs to be run if the data needs to be remade

#BC3_Enviro <- read.csv("DBRDA_Roots/BC3_ENVIRO.csv", row.names = 1)
#BC3_Enviro_ROOT <- BC3_Enviro[BC3_Enviro$Type=="Root",]

which(row.names(BC3_Enviro_ROOT)=="LA.R.25")

#BC3_Enviro_ROOT <- BC3_Enviro_ROOT[-26,]

#write.csv(BC3_Enviro_ROOT, "DBRDA_Roots/BC3_Enviro_ROOT.csv")

#reload environmental data as an object

BC3_Enviro_ROOT <- read.csv("DBRDA_Roots/BC3_ENVIRO_ROOT.csv", row.names = 1)

#check rownames
which(rownames(BC3_OTU)!=rownames(BC3_Enviro_ROOT))

#Checked against old emails (from Oct 2-3 between Steve and Liz) these samples should be removed as well.

rownames(BC3_OTU)[8] #has no GA.R.40
rownames(BC3_Enviro_ROOT)[8]  #has no GA.R.39
rownames(BC3_OTU)[40]  #has no SC.R.48
rownames(BC3_Enviro_ROOT)[40]  #has no SC.R.47

BC3_Enviro_ROOT <- BC3_Enviro_ROOT[c(-8,-40),]
BC3_OTU <- BC3_OTU[c(-8,-40),]

#rename "Terrebone" to "Terrebonne" in transect factor
levels(BC3_Enviro_ROOT$Transect)[levels(BC3_Enviro_ROOT$Transect)=="Terrebone"] <- "Terrebonne"

#removed "derep" from OTU names
otu.spp<-names(BC3_OTU[grep("derep_", names(BC3_OTU))])
bcr<-BC3_OTU[,otu.spp]

#make matrix-------

# Make Bray Curtis dissimilarity matrix 
BC3BrayCurtisMatrix<-vegdist(bcr, method="bray", binary=FALSE)


#dbrda with all environmental data
#Lat+Long+Degradation+HistTidal+WoodyCov_z+ForestCover_z+TreeDensity_2014_z+PlotDensity_z+ForestHeight_z+...+MeanTemp_6mo_z+
#FullModel
dbRDA_all <- capscale(BC3BrayCurtisMatrix ~ WoodyCov_z+TreeDensity_2014_z+WaterLevel_z+MeanSal_6mo_z, data=BC3_Enviro_ROOT)

#Null Model
#generate one model with NOTHING to explain the braycurtis dm matrix
dbRDAnull_BC3 <- capscale(BC3BrayCurtisMatrix~1,data=BC3_Enviro_ROOT)

#use forward selection to choose which elements of the full model explain a significant amount of variaiton in the unifrac dm by comparing it to the null

dbRDA_forsel <- ordistep(dbRDAnull_BC3,scope=formula(dbRDA_all),direction="forward",Pin=.1,trace=T,pstep=500000)

dbRDA_forsel

plot(dbRDA_forsel)
#this makes a very ugly plot

anova(dbRDA_forsel, by="margin")


#make figure in ggplot----------

#any rid of NA values?

anyNA(BC3_Enviro_ROOT)
anyNA(BC3_OTU)

#make model sumary
B <- summary(dbRDA_forsel)

#plot

plot(dbRDA_forsel)
A.1 <- scores(dbRDA_forsel)
A.2 <- A.1$sites
A.3 <- cbind(A.2, BC3_Enviro_ROOT)

#reorder Transect levels to make plotting easier
A.3$Transect <- factor(A.3$Transect, levels = c("Barataria", "Terrebonne", "Savannah","Waccamaw"))

#scores for arows
A.4 <- data.frame(scores(dbRDA_forsel, display = "bp"))

# Define the arrow aesthetic mapping
arrow_map <- aes(xend = CAP1, yend = CAP2, x = 0, y = 0, shape = NULL, color = NULL, fill = NULL)
label_map <- aes(x = 1*CAP1, y = 1*CAP2, label = row.names(A.4), shape = NULL, color = NULL, fill = NULL)
arrowhead = arrow(length = unit(0.02, "npc"))

#subset A4 for labeling
A.4 <- A.4[sort(rownames(A.4)),]
A4.sub1 <- A.4[3:4,]
A4.sub2 <- A.4[1,]
A4.sub3 <- A.4[2,]

#make plot
p <- ggplot(data = A.3, aes(x = CAP1, y = CAP2))

p.dbrda.roots <- p +
  geom_point(data = A.3, alpha = 2/5, 
             aes(shape = Transect), size = rel(3), color = "black", stroke = 1) +
  theme_bw() + 
  geom_segment(arrow_map, size = rel(1), data = A.4, color = "black", arrow = arrowhead) +
  geom_text(label_map, size = rel(4), data = A4.sub1, show.legend = FALSE, hjust = 0, label = c("Water Level", "Woody Cover"), nudge_y = 0.05) +
  geom_text(label_map, size = rel(4), data = A4.sub2, show.legend = FALSE, hjust = 1, label = c("Mean Salinity"), nudge_y = 0.1) +
  geom_text(label_map, size = rel(4), data = A4.sub3, show.legend = FALSE, hjust = 1, label = c("Tree Density"), nudge_y = -0.1) +
  xlab(label = paste("CAP1 (", round(B$concont$importance[2,1]*100, digits = 1), "%)", sep="")) +
  ylab(label = paste("CAP2 (", round(B$concont$importance[2,2]*100, digits = 1), "%)", sep="")) +
  scale_shape_manual(name = "Transect/Gradient", labels = c("Barataria (LA)", "Terrebonne (LA)", "Savannah (GA)", "Waccamaw (SC)"), values = c(21,23,15,17))

 # ggtitle("dbRDA of Roots")
  
p.dbrda.roots

#save figure
#ggsave(filename = "DBRDA_Roots/dbRDA_roots.png", width = 6, height = 6, units = "in")
#ggsave(filename = "dbRDA_roots_NT.png", width = 6, height = 6, units = "in")

#final adjustment for manuscript on June 24, 2019
ggsave(filename = paste0("DBRDA_Roots/dbRDA_roots_NT_",Sys.Date(),".png"), width = 6, height = 6, units = "in")
