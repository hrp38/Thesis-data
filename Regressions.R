Thesis.data1<-read.csv("https://raw.githubusercontent.com/hrp38/Thesis-data/main/HP_MB_THESIS%20TOTAL%20DATA%20SET%20(2).csv")

#root colonization
slr_colonization<- lm(PropColonization_Root~WaterTrt, data=Thesis.data1)
summary(slr_colonization)
slr_colonization_dummy <- lm(PropColonization_Root~WaterTrt, data=Inoc_Subj)
summary(slr_colonization_dummy)
colon_plot<- plot(PropColonization_Root~WaterTrt, data=Inoc_Subj)


#baiting
slr_baiting<- lm(SoilBaitProp~WaterCode, data=Thesis.data1)
summary(slr_baiting)
slr_baiting1<- lm(SoilBaitProp~WaterCode, data=Inoc_Subj)
summary(slr_baiting1)
baiting_plot<- plot(SoilBaitProp~WaterCode, data=Inoc_Subj)
abline(slr_baiting1, col="red")


#baiting~colonization
slr_colon_baiting<- lm(ArcSinColon_Root~ArcSinSoilBaitProp, data=Thesis.data1)
summary(slr_colon_baiting)
colon_bait_plot<-plot(PropColonization_Root~SoilBaitProp, data=Thesis.data1, xlab="Proportion Infected in Soil Baiting", ylab = "Proportion Root Colonization")
abline(slr_colon_baiting)

?abline
#height
mlr_height<- lm(Height~ArcSinColon_Root+WaterTrt+Round.number, data=Thesis.data1)
summary(mlr_height)
height_plot<- plot(Height~ArcSinColon_Root, xlab="Proportion Root Colonization", ylab= "Height (cm)", data= Thesis.data1, col=factor(WaterTrt))
abline(mlr_height)

#biomass
mlr_biomass<-lm(Biomass~PropColonization_Root+WaterTrt, data=Thesis.data1)
summary(mlr_biomass)
biomass_plot<- plot(Biomass~PropColonization_Root, xlab="Proportion Root Colonization", ylab= "Biomass (g)", data=Thesis.data1, col=factor(WaterTrt))
abline(mlr_biomass)

#biomass, with height as blocking
mlr_biomass_block<- lm(Biomass~PropColonization_Root+WaterTrt+Height, data=Thesis.data1)
summary(mlr_biomass_block)

#chlorophyll content
mlr_chlorophyll<- lm(Chl_content~PropColonization_Root+WaterTrt, data=Thesis.data1)
summary(mlr_chlorophyll)
chl_plot<- plot(Chl_content~PropColonization_Root, data=Thesis.data1)
abline(mlr_chlorophyll)


library(ggplot2)
library(dplyr)
summarydata<- Thesis.data1 %>%
  group_by(WaterTrt) %>%
  summary(mean_rootcol = mean(PropColonization_Root), sd_rootcol = sd(PropColonization_Root), count=n(), se_rootcol = (sd_rootcol)/(sqrt(count)))
View(summarydata)

ggplot() + geom_col(data = Thesis.data1, aes(x = WaterTrt, y = PropColonization_Root))


ggplot(Thesis.data1)+ geom_bar(aes(WaterTrt, PropColonization_Root, stat = "summary", fun.y= "mean"))
?aes
?stat
?sd()



