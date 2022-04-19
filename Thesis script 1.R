Thesis.data1<-read.csv("https://raw.githubusercontent.com/hrp38/Thesis-data/main/HP_MB_THESIS%20TOTAL%20DATA%20SET%20(2).csv")
#make subsets of each group so you can graph it easily
Def_Inoc<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Def" & Thesis.data1$MetaCode=="Inoc"))
Ade_Inoc<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Ade" & Thesis.data1$MetaCode=="Inoc"))
Exc_Inoc<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Exc" & Thesis.data1$MetaCode=="Inoc"))
Def_Unin<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Def" & Thesis.data1$MetaCode=="Unin"))
Ade_Unin<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Ade" & Thesis.data1$MetaCode=="Unin"))
Exc_Unin<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Exc" & Thesis.data1$MetaCode=="Unin"))

#Let's make subsets of the data for colonized
Def_Colonized<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Def" & Thesis.data1$Colonization_Root>0 & Thesis.data1$MetaCode=="Inoc"))
Ade_Colonized<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Ade" & Thesis.data1$Colonization_Root>0 & Thesis.data1$MetaCode=="Inoc"))
Exc_Colonized<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Exc" & Thesis.data1$Colonization_Root>0 & Thesis.data1$MetaCode=="Inoc"))

#Subset for inoculated but not detected
Def_Undetected<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Def" & Thesis.data1$Colonization_Root==0 & Thesis.data1$MetaCode=="Inoc"))
Ade_Undetected<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Ade" & Thesis.data1$Colonization_Root==0 & Thesis.data1$MetaCode=="Inoc"))
Exc_Undetected<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode=="Exc" & Thesis.data1$Colonization_Root==0 & Thesis.data1$MetaCode=="Inoc"))



#colonization averages vector
colon_averages<- c(mean(Def_Inoc$X.Colon_Root), mean(Ade_Inoc$X.Colon_Root), mean(Exc_Inoc$X.Colon_Root))
#Group order vector
Group<- c("Deficit", "Adequate", "Excess")
#barplot of water and metarhizium treatmentxcolonization at root level
barplot(colon_averages~Group, xlab="WaterTreatment", ylab="Percent Metarhizium Colonization", ylim = c(0,50), main= "Percent Colonization of Metarhizium by Water Stress Treatment")
#should try to do a plot like this but for the # of plants colonized per treatment? 
?anova
library(ggplot2)
library(tidyverse)
library(ggpubr)
#make a subset that's just inoculated plants
Inoc_Subj<-subset.data.frame(Thesis.data1, (Thesis.data1$MetaCode == "Inoc"))
#make a subset that's just deficit plants
Def_Plants<- subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode== "Def"))
#make a subset that's just adequate plants
Ade_Plants<- subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode== "Ade"))
#make a subset that's just excess plants
Exc_Plants<-subset.data.frame(Thesis.data1, (Thesis.data1$WaterCode== "Exc"))

#run anova to see if water treatment impacts colonization level
anova_colonization<- aov(dummy_colon~WaterTrt, data=Inoc_Subj)
anova_colonization<- aov(ArcSinColon_Root~WaterTrt, data=Inoc_Subj)
summary(anova_colonization)
TukeyHSD(anova_colonization)
View(tukey_anova_colonization)
slr_colonization<- lm(PropColonization_Root~WaterTrt, data=Thesis.data1)
summary(slr_colonization)
#like barely not significant? idk if this can be fixed or something or maybe try a diff test

#let's look at how water treatment impacted soil baiting
anova_baiting<- aov(SoilBaitProp~WaterTrt, data=Inoc_Subj)
summary(anova_baiting)
tukey_anova_baiting<-TukeyHSD(anova_baiting)
summary(tukey_anova_baiting)
slr_baiting<- lm(SoilBaitProp~WaterTrt, data=Thesis.data1)
summary(slr_baiting)
#significant!
#slr for how baiting impacts colonization level
bait_colon_slr_DEF<- lm(PropColonization_Root~SoilBaitProp, data=Def_Inoc)
summary(bait_colon_slr_DEF)
bait_colon_slr_ADE<- lm(PropColonization_Root~SoilBaitProp, data=Ade_Inoc)
summary(bait_colon_slr_ADE)
bait_colon_slr_EXC<- lm(PropColonization_Root~SoilBaitProp, data=Exc_Inoc)
summary(bait_colon_slr_EXC)
bait_colon_slr_ALL<- lm(PropColonization_Root~SoilBaitProp+WaterTrt, data=Inoc_Subj)
summary(bait_colon_slr_ALL)
#These are like crazy not significant...whats that mean tho??

#let's see how it impacts height
height_d<- aov(Height~PropColonization_Root, data=Def_Inoc)
summary(height_d)
#significant!
#let's look at interactions
height_int<-aov(Height~PropColonization_Root*WaterTrt, data=Thesis.data1)
summary(height_int)
Tukeyheightint<- TukeyHSD(height_int)
mlr_height<- lm(Height~PropColonization_Root+WaterTrt, data=Thesis.data1)
summary(mlr_height)
#now just colonization levels
height_justMeta<-aov(Height~PropColonization_Root, data=Thesis.data1)
summary(height_justMeta)

#let's graph it 
height_avg<-c(mean(Ade_Unin$Height), mean(Ade_Undetected$Height), mean(Ade_Colonized$Height), mean(Def_Unin$Height), mean(Def_Undetected$Height), mean(Def_Colonized$Height), mean(Exc_Unin$Height), mean(Exc_Undetected$Height), mean(Exc_Colonized$Height))
Group2<- c("Adequate and Uninoculated", "Adequate and Undetected", "Adequate and Detected", "Deficit and Uninoculated", "Deficit and Undetected", "Deficit and Detected", "Excess and Uninoculated", "Excess and Not Detected", "Excess and Detected")
barplot(height_avg~Group2, xlab="Water and Metarhizium Treatment", ylab="Height (cm)", ylim=c(0,140), main="Height")
View(height_avg)

#okay now biomass
biomass<- aov(Biomass~PropColonization_Root+WaterTrt, data=Thesis.data1)
summary(biomass)
#MLR
biomass_mlr<-lm(Biomass~PropColonization_Root+WaterTrt, data=Thesis.data1)
summary(biomass_mlr)
#now with blocking height
biomass_mlr_blocking<- lm(Biomass~PropColonization_Root+WaterTrt+Height, data=Thesis.data1)
summary(biomass_mlr_blocking)
#significant!
#look at interactions
biomass_int<- aov(Biomass~PropColonization_Root*WaterTrt, data=Thesis.data1)
summary(biomass_int)

#let's graph it
biomass_avg<- c(mean(Ade_Unin$Biomass), mean(Ade_Undetected$Biomass), mean(Ade_Colonized$Biomass), mean(Def_Unin$Biomass), mean(Def_Undetected$Biomass), mean(Def_Colonized$Biomass), mean(Exc_Unin$Biomass), mean(Exc_Undetected$Biomass), mean(Exc_Colonized$Biomass))
barplot(biomass_avg~Group2, xlab="Water and Metarhizium Treatment", ylab="Dry Biomass", ylim=c(0,20), main="Biomass")

#okay now chlorophyll content
chloro<- aov(Chl_content~PropColonization_Root+WaterTrt, data=Thesis.data1)
summary(chloro)
#not significant on either level

#okay now temperature
temp<- aov(Temp_avg~PropColonization_Root+WaterTrt, data=Thesis.data1)
summary(temp)
#not significant on either variable

#let's try the potassium just because
K_level<- aov(Potassium.ppm~PropColonization_Root+WaterTrt, data=Thesis.data1)
summary(K_level)
#in deficit plants?
K_level_def<- aov(Potassium.ppm~PropColonization_Root, data=Def_Plants)
summary(K_level_def)
#in control?
K_level_ade<- aov(Potassium.ppm~PropColonization_Root, data=Ade_Plants)
summary(K_level_ade)
#in excess?
k_level_exc<- aov(Potassium.ppm~PropColonization_Root, data=Exc_Plants)
summary(k_level_exc)

MLR_nutrientdata<- lm(dummy_colon ~ , data=Inoc_Subj)
summary(MLR_nutrientdata)

install.packages("fastDummies")
yes
library("fastDummies")
dummy_colon<- ifelse(Thesis.data1$PropColonization_Root==0, 0, 1)
View(dummy_colon)
dummy_baiting<- ifelse(Thesis.data1$SoilBaitProp==0, 0, 1)
slr_dummys<- lm(dummy_colon~dummy_baiting, data=Thesis.data1)
summary(slr_dummys) #WOOOHOO CRAZY SIGNIFICANT! TELL MARY AB THIS 
slr_colontowater<- lm(dummy_colon~WaterTrt, data=Inoc_Subj)
summary(slr_colontowater)
slr_baitingtowater<- lm(dummy_baiting~WaterTrt, data=Inoc_Subj)
summary(slr_baitingtowater)

View(Thesis.data1)
slr_potassium<- lm(Potassium.ppm~PT_K..+WaterTrt+dummy_colon, data=Thesis.data1)
summary(slr_potassium)
potassiumplot<- plot(Potassium.ppm~PT_K.., data=Thesis.data1, xlab="Soil Potassium", ylab="Plant Potassium", col=factor(WaterTrt))
mlr_phos<- lm(PT_P..~Phosphorus_ppm+WaterTrt+dummy_colon, data=Thesis.data1)
summary(mlr_phos)
phosplot<-plot(Phosphorus_ppm~PT_P.., data=Thesis.data1, xlab="Plant Phos", ylab="Soil Phos", col=factor(dummy_colon))

?na.action
na.action(na.omit(Inoc_Subj))
