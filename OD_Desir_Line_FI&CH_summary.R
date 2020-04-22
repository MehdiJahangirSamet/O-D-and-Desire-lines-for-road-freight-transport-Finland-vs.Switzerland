#O-D flow presentations
rm(list=objects())
#install.packages("tidyr")
#install.packages("sf")
#install.packages("dplyr")
#install.packages("spDataLarge")
#install.packages("stplanr")      # geographic transport data package
#install.packages("tmap")         # visualization package (see Chapter 8)
library(plyr)
library(tidyr)
library(sf)
library(dplyr)
library(spDataLarge)
library(stplanr)      # geographic transport data package
library(tmap)         # visualization package (see Chapter 8)
require(sp)
require(rgdal)
require(maps)


#read sf zone centers points
Zones<- st_read ("ShapefileDir","FI_municipality_vs._Postal_code" )

#read sf polygons
PolygonZones<- st_read ("ShapefileDir","BoundaryZones" )


# read in Zone polygons
#Zones <- readOGR(".", "FI_municipality_vs._Postal_code")
class(Zones)

#Reorder the first by the municipality code
movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}
#str(Zones)

Zones<-movetolast(Zones, c("X", "Y"))

# tell R that End and startpoints coordinates are in the same lat/lon reference system
# as the parks data -- BUT ONLY BECAUSE WE KNOW THIS IS THE CASE!
#Zones <- spTransform(Zones, CRS("+proj=longlat +datum=WGS84"))
#proj4string(Zones)
names(Zones)


# read in Specific o-d data, and turn it into a SpatialPointsDataFrame
# Here the data is for spacific route of Helsinki-Lahti-Yvaskyla-Oulu-Kemi for freight Finland data 2016
Primary_OD <- read.csv("All Fin2016_2.csv", stringsAsFactors = FALSE, header = TRUE)

# build counter trip
Primary_OD$TripNo<-1
str(Primary_OD)

names(Primary_OD)

#[35] "million.liters"                 "CO2.kt"                        
#[37] "million.t"                      "million.km"                    
#[39] "million.tkm"                    "million.journeys"              
#[41] "journey.kWh"                    "TripNo"                        

##########################
#change the PLZ from 4digit to 2 digit
#Primary_OD$fromPLZ<-Primary_OD$fromPLZ2D
#Primary_OD$toPLZ<-Primary_OD$toPLZ2D

#change the Municipality variable name 
names(Zones)[1]<- "Unique_Min"



#####################################
#set filter for the type of Truck among "1-Trucks_Upto_11t_pay" OR "2-Trucks_11to30t_pay" OR "3-Trucks_30to58t_pay"

#Primary_OD = filter(Primary_OD, load.capacity.categories == "1-Trucks_Upto_11t_pay")


#####################################
#set filter fo emission type from 0 to 6
#Primary_OD = filter(Primary_OD, emission == 6)

#####################################
#Filter for failed scenario, "Under.11t.Scenario.1_Failed" OR "Under.11t.Scenario.2_Failed" OR "Under.11t.Scenario.3_Failed"
# "X11.30ton.Scenario.1_Failed" OR "X11.30ton.Scenario.2_Failed" OR "X11.30ton.Scenario.1_Failed"

#Primary_OD = filter(Primary_OD, X11.30ton.Scenario.3_Failed != 0)
#Primary_OD = filter(Primary_OD, Same.trip.All.leg == 1)


#######################################
#Summary set category for "million.tkm" each Transport category based on "typeOfGoodsNST2007"
#ddply(Primary_OD,~typeOfGoodsNST2007,summarise,Tonkm_of_typeOfGoodsNST2007=sum(unique(million.tkm)))

#Summary set category for "million.km" each Transport category based on "typeOfGoodsNST2007"
ddply(Primary_OD,~typeOfGoodsNST2007,summarise,Km_of_typeOfGoodsNST2007=sum(unique(million.km)))


#Summary set category for emission
#ddply(Primary_OD,~emission,summarise,Tonkm_of_emission=sum(unique(million.tkm)))

#Summary set to all filtered
#data.frame(a=c(sum(Primary_OD$million.journeys),sum(Primary_OD$million.km),sum(Primary_OD$million.t),sum(Primary_OD$million.tkm),sum(Primary_OD$million.liters),sum(Primary_OD$CO2.kt),sum(Primary_OD$million.kWh)))

########################
#########################################
#set filter list for destinations and origins for special corridors FOR electrification road
#x<- list(16,18,20,49,69,71,72,75,77,81,82,86,91,92,98,106,108,109,111,139,142,143,145,152,164,165,172,179,181,182,186,202,211,216,230,232,235,239,240,241,244,245,250,256,257,265,283,285,286,291,301,316,317,398,399,407,410,418,425,433,434,435,436,444,475,489,494,499,500,504,505,532,535,536,543,560,563,564,576,577,581,588,592,595,601,604,611,616,624,626,630,635,638,680,691,694,704,729,734,738,743,748,751,753,753,755,781,791,837,846,850,851,853,858,859,892,905,908,921,922,927,931,935,980,992)
#Primary_OD = filter(Primary_OD, fromPLZ %in% x)
#Primary_OD = filter(Primary_OD, toPLZ %in% x)
#Primary_OD$Group3_Scenario2<-1

#Primary_OD$Group3_Scenario1<-1



##########################
#based on Gropu of trucks "1-Trucks_Upto_11t_pay" OR "2-Trucks_11to30t_pay" OR "3-Trucks_30to58t_pay"
G <- c("1-Trucks_Upto_11t_pay","1-Trucks_Upto_11t_pay","1-Trucks_Upto_11t_pay","1-Trucks_Upto_11t_pay","1-Trucks_Upto_11t_pay","2-Trucks_11to30t_pay","2-Trucks_11to30t_pay","2-Trucks_11to30t_pay","2-Trucks_11to30t_pay","2-Trucks_11to30t_pay","3-Trucks_30to58t_pay","3-Trucks_30to58t_pay","3-Trucks_30to58t_pay","3-Trucks_30to58t_pay","3-Trucks_30to58t_pay")
G1 <- c("Group1_Scenario1","Group1_Scenario2","Group1_Scenario3","Group1_Scenario4","Group1_Scenario5","Group2_Scenario1","Group2_Scenario2","Group2_Scenario3","Group2_Scenario4","Group2_Scenario5","Group3_Scenario1","Group3_Scenario2","Group3_Scenario3","Group3_Scenario4","Group3_Scenario5")

# items for type of goods NST2007 in Finland
G2 <- c(1:19,99)
# items for emission category
G3<- c("-1993", "euro1", "euro2","euro3","euro4","euro5")

SummaryFin <- matrix(0, ncol =15 , nrow = 137)

#For All Groups

for(i in 1:15){
  FilterODG1<-filter(Primary_OD, load.capacity.categories == G[i])
  
  #all trips
  SummaryFin[1,i]<-G[i]
  SummaryFin[2,i]<-sum(FilterODG1$million.journeys)
  SummaryFin[3,i]<-sum(FilterODG1$million.km)
  SummaryFin[4,i]<-sum(FilterODG1$million.t)
  SummaryFin[5,i]<-sum(FilterODG1$million.tkm)
  SummaryFin[6,i]<-sum(FilterODG1$million.liters)
  SummaryFin[7,i]<-sum(FilterODG1$CO2.kt)
  SummaryFin[8,i]<-sum(FilterODG1$million.kWh)
  
  #All failed trips
  FilterODG2<-filter(FilterODG1, !!as.symbol(G1[i])!= 0)
  SummaryFin[10,i]<-sum(FilterODG2$million.journeys)
  SummaryFin[11,i]<-sum(FilterODG2$million.km)
  SummaryFin[12,i]<-sum(FilterODG2$million.t)
  SummaryFin[13,i]<-sum(FilterODG2$million.tkm)
  SummaryFin[14,i]<-sum(FilterODG2$million.liters)
  SummaryFin[15,i]<-sum(FilterODG2$CO2.kt)
  SummaryFin[16,i]<-sum(FilterODG2$million.kWh)
  
  #Average missed distance per failed trip (km) in simulation
  #for the all shortage leg
  Primary_OD2 = filter(FilterODG1, End.R.indicator != "")
  Primary_OD2 = filter(Primary_OD2, !!as.symbol(G1[i]) != 0)
  SummaryFin[17,i]<- mean(Primary_OD2[,G1[i]])
  
  #All failed trips in the same PLZs
  FilterODG3 = filter(FilterODG2, Same.trip.All.leg == 1)
  SummaryFin[28,i]<-sum(FilterODG3$million.journeys)
  SummaryFin[29,i]<-sum(FilterODG3$million.km)
  SummaryFin[30,i]<-sum(FilterODG3$million.t)
  SummaryFin[31,i]<-sum(FilterODG3$million.tkm)
  SummaryFin[32,i]<-sum(FilterODG3$million.liters)
  SummaryFin[33,i]<-sum(FilterODG3$CO2.kt)
  SummaryFin[34,i]<-sum(FilterODG3$million.kWh)
  
  #Average missed distance per failed trip (km) in simulation
  #for the same 2digit plzs
  Primary_OD1 = filter(FilterODG3, Same.trip.last.leg == 1)
  SummaryFin[35,i] <- mean(Primary_OD1[,G1[i]])
  
  #Average missed distance per failed trip (km) in simulation
  #for different 2digit plzs 
  
  Primary_OD3 = filter(FilterODG1, End.R.indicator != "")
  Primary_OD3 = filter(Primary_OD3, !!as.symbol(G1[i]) != 0)
  Primary_OD3 = filter(Primary_OD3, Same.trip.All.leg != 1)
  SummaryFin[26,i]<- mean(Primary_OD3[,G1[i]])
  
  #Summary set category for "million.tkm" each Transport category based on "typeOfGoodsNST2007"
  #1-All trips
  #2-successful trips
  #3-manageable trips
  
  #successful trips + failed in the same PlZs
  t1<-40
  
  for (m in G2){
    #filter for successful trips
    FilterODG4 <-filter(FilterODG1,!!as.symbol(G1[i])== 0)
    
    #filter for failed same PLZs
    FilterODG5 <- filter(FilterODG3, Same.trip.last.leg == 1)
    
    #1-All trips
    #2-successful trips
    #3-manageable trips
    
    SummaryFin[t1,i]<-sum(filter(FilterODG1, typeOfGoodsNST2007== m)$million.tkm)
    SummaryFin[t1+23,i]<-sum(filter(FilterODG4, typeOfGoodsNST2007== m)$million.tkm)
    SummaryFin[t1+46,i]<-sum(filter(FilterODG5, typeOfGoodsNST2007== m)$million.tkm) + sum(filter(FilterODG4, typeOfGoodsNST2007== m)$million.tkm)
    t1<- t1+1
    }
  
  #Summary set category for "million.tkm" each Transport category based on "emission"
  
  #1-All trips
  #2-successful trips
  #3-manageable trips
  
  t2<- 110
  for (m1 in G3){
    #filter for successful trips
    FilterODG4 <-filter(FilterODG1,!!as.symbol(G1[i])== 0)
    #filter for failed same PLZs
    FilterODG5 <- filter(FilterODG3, Same.trip.last.leg == 1)
    
    #1-All trips
    #2-successful trips
    #3-manageable trips
    
    SummaryFin[t2,i]<-sum(filter(FilterODG1,emission == m1)$million.tkm)
    SummaryFin[t2+9,i]<-sum(filter(FilterODG4,emission == m1)$million.tkm)
    SummaryFin[t2+18,i]<-sum(filter(FilterODG5, emission == m1)$million.tkm) + sum(filter(FilterODG4,emission == m1)$million.tkm)
    t2<- t2+1
    }
  
    
  }
    
    

########################
#sensivity loop for shortage distance filtering "X11.30ton.Scenario.1_Failedshortage"
#This will show how much figure would be added if
#to set G1[1] to G1[5] illustrates the sensivity analysis for shortage failed trips 
# of scenario 1 to 5 set i2 1-5 as the scenario number of under 11 ton
# of scenario 1 to 5 set i2 6-10 as the scenario number of 11-30 ton
# of scenario 1 to 5 set i2 11-15 as the scenario number of 30-58 ton

i2 <-7


RangeSeq<-seq(from=0,to=max(Primary_OD[,G1[i2]]),by=20)
n<-length(RangeSeq)

SensiTable <- matrix(0, ncol = 8, nrow = n)
i<-1

for(i in 1:n){
  FilterOD<-filter(Primary_OD, !!as.symbol(G1[i2]) <= RangeSeq[i] & !!as.symbol(G1[i2]) >0)
  SensiTable[i,1]<-RangeSeq[i]
  SensiTable[i,2]<-sum(FilterOD$million.journeys)
  SensiTable[i,3]<-sum(FilterOD$million.km)
  SensiTable[i,4]<-sum(FilterOD$million.t)
  SensiTable[i,5]<-sum(FilterOD$million.tkm)
  SensiTable[i,6]<-sum(FilterOD$million.liters)
  SensiTable[i,7]<-sum(FilterOD$CO2.kt)
  SensiTable[i,8]<-sum(FilterOD$million.kWh)
}

SensiTable2<- data.frame(SensiTable)
#plot(SensiTable)
#library(ggplot2)
#autoplot(SensiTable2)

#ggplot(SensiTable2,aes(X1))+
 # geom_line(aes(y = X2, colour = "var0"))+
  #geom_line(aes(y = X3, colour = "var1"))+
  #geom_line(aes(y = X4, colour = "var2"))+
  #geom_line(aes(y = X5, colour = "var3"))+
  #geom_line(aes(y = X6, colour = "var4"))+
  #geom_line(aes(y = X7, colour = "var5"))+
  #geom_line(aes(y = X8, colour = "var6"))



##########################
#mean for shortage leg

#for the same 2digit plzs
#Primary_OD1 = filter(Primary_OD, Same.trip.last.leg == 1)
#MeanshortageSamePLZ<- mean(Primary_OD1$X11.30ton.Scenario.3_Failedshortage)

#for the all shortage leg
#Primary_OD2 = filter(Primary_OD, End.R.indicator != "")
#Primary_OD2 = filter(Primary_OD2, X11.30ton.Scenario.3_Failedshortage != 0)
#Meanshortageallleg<- mean(Primary_OD2$X11.30ton.Scenario.3_Failedshortage)

#for different 2digit plzs 
#Primary_OD3 = filter(Primary_OD, End.R.indicator != "")
#Primary_OD3 = filter(Primary_OD3, X11.30ton.Scenario.3_Failedshortage != 0)
#Primary_OD3 = filter(Primary_OD3, Same.trip.All.leg != 1)
#Meanshortagediffleg<- mean(Primary_OD3$X11.30ton.Scenario.3_Failedshortage)


#"million.kWh"                         "Under.11t.Scenario.1_Failed"        
#[55] "Under.11t.Scenario.1_Failedshortage" "Under.11t.Scenario.2_Failed"        
#[57] "Under.11t.Scenario.2_Failedshortage"

#########################################
#set filter list for destinations and origins for special corridors
#x<- list(16,18,20,49,69,71,72,75,77,81,82,86,91,92,98,106,108,109,111,139,142,143,145,152,164,165,172,179,181,182,186,202,211,216,230,232,235,239,240,241,244,245,250,256,257,265,283,285,286,291,301,316,317,398,399,407,410,418,425,433,434,435,436,444,475,489,494,499,500,504,505,532,535,536,543,560,563,564,576,577,581,588,592,595,601,604,611,616,624,626,630,635,638,680,691,694,704,729,734,738,743,748,751,753,753,755,781,791,837,846,850,851,853,858,859,892,905,908,921,922,927,931,935,980,992)
#Primary_OD = filter(Primary_OD, fromPLZ %in% x)
#Primary_OD = filter(Primary_OD, toPLZ %in% x)


#set filter for external or internal legs

#Primary_OD = filter(Primary_OD, fromPLZ == toPLZ)
#names(Primary_OD)



#######################################
#Summary set category for "million.tkm" each Transport category based on "typeOfGoodsNST2007"
#ddply(Primary_OD,~typeOfGoodsNST2007,summarise,Tonkm_of_typeOfGoodsNST2007=sum(unique(million.tkm)))

#Summary set category for emission
#ddply(Primary_OD,~emission,summarise,Tonkm_of_emission=sum(unique(million.tkm)))

#Summary set to all filtered
#data.frame(a=c(sum(Primary_OD$million.journeys),sum(Primary_OD$million.km),sum(Primary_OD$million.t),sum(Primary_OD$million.tkm),sum(Primary_OD$million.liters),sum(Primary_OD$CO2.kt),sum(Primary_OD$million.kWh)))

#setFilter for the same OD trave results
#zones_attr= Primary_OD %>%
#  group_by(fromPLZ, toPLZ)%>%
#  summarize(TripNo=sum(million.journeys),TripDistance=sum(million.km), Triploadkg=sum(million.t), TripTonKm= sum(million.tkm), TripFuel=sum(million.liters), TripCO2= sum(CO2.kt), TripKW= sum(million.kWh)) %>% 
#  dplyr::rename(Unique_Min =fromPLZ)


summary(zones_attr$Unique_Min %in% Zones$Unique_Min)

#for making the same the Unique_Min code in both matrix
Zones$Unique_Min <- as.numeric(as.character(Zones$Unique_Min))
str(Zones)
#breast$class <- as.numeric(as.character(breast$class))

zones_joined = left_join(Zones, zones_attr, by = "Unique_Min")
str(zones_joined)
sum(zones_joined$TripNo)

names(zones_joined)

#Destination summary zones
zones_od = Primary_OD %>% 
  group_by(toPLZ) %>% 
  #summarize_if(is.numeric, sum) %>% 
  summarize(TripNo_D=sum(million.journeys),TripDistance_D=sum(million.km), Triploadkg_D=sum(million.t), TripTonKm_D= sum(million.tkm), TripFuel_D=sum(million.liters), TripCO2_D= sum(CO2.kt), TripKW_D= sum(million.kWh)) %>% 
  dplyr::select(Unique_Min = toPLZ, TripNo_D, TripDistance_D, Triploadkg_D,TripTonKm_D,TripFuel_D,TripCO2_D,TripKW_D) %>% 
  inner_join(zones_joined, ., by = "Unique_Min")
zones_od
zones_attr
sum(zones_attr$TripNo)

#Abstract O_D 
Abs_zones_od = zones_od %>%
  group_by(Unique_Min) %>%
  summarize(TripNo= sum(TripNo), TripDistance=sum(TripDistance),Triploadkg=sum(Triploadkg),TripTonKm=sum(TripTonKm), TripFuel=sum(TripFuel), TripCO2= sum(TripCO2), TripKW= sum(TripKW), TripNo_D= mean(TripNo_D),TripDistance_D=mean(TripDistance_D),Triploadkg_D= mean(Triploadkg_D),TripTonKm_D=mean(TripTonKm_D), TripFuel_D=mean(TripFuel_D), TripCO2_D= mean(TripCO2_D), TripKW_D= mean(TripKW_D)) %>% 
  dplyr::rename(Zone_No=Unique_Min)


#Faceted Maps for average daily trip attractions and generations in different zones 
qtm(Abs_zones_od, symbols.size =c("TripNo", "TripNo_D"), fill.n = 8) +
  tm_shape(PolygonZones) + tm_borders() +
  tm_layout(panel.labels = c("Origin", "Destination"))

######################################
#abstract results
#data.frame(a=c(sum(Abs_zones_od$TripNo_D),sum(Abs_zones_od$TripDistance_D),sum(Abs_zones_od$Triploadkg_D),sum(Abs_zones_od$TripTonKm_D),sum(Abs_zones_od$TripFuel_D),sum(Abs_zones_od$TripFuel_D),sum(Abs_zones_od$TripKW_D)))




###################################
#save the trip attraction and generation as point shap file
#st_write(Abs_zones_od, "ShapefileDir/Trucks_Upto11tons_failedScenario2_ODpoints_internal.shp",driver="ESRI Shapefile")


#Desire Lines
od_top10 = Abs_zones_od %>% 
  arrange(desc(TripNo)) %>% 
  top_n(10, wt = TripNo)

od_top10


#od_intra = filter(Primary_OD, fromPLZ == toPLZ)
od_inter = filter(zones_od, Unique_Min != toPLZ)
names(od_inter)
od_inter<-movetolast(od_inter, c("Postal_Cod", "Name_of_th", "community_","X_Longtitu", "Y_Latitude"))
names(od_inter)
#<-zones_od[Dist_Zone & EndZone, ]
#zones
#odlines <- od_coords2line(zones_od)
desire_lines = od2line(od_inter, zones_od)

#odlines <- od_coords2line(zones_od)
str(desire_lines)

#> Creating centroids representing desire line start and end points.
qtm(desire_lines, lines.col ="TripNo", lines.lwd = "TripNo")+ 
tm_layout(legend.width=20) #+
  tm_polygons(PolygonZones)

class(desire_lines)
Abs_zones_od
plot(Abs_zones_od)
  plot(desire_lines,breaks = c(0, 50, 70, 80, 90, 95, 100))



#desire_lines and the backgroundzones


tm_shape(PolygonZones) + tm_borders() +
  tm_shape(Abs_zones_od)+
  tm_symbols(size = "TripNo",shapes = "25",
             legend.format = list(text.align="right", text.to.columns = TRUE))+
    tm_shape(desire_lines) +
  tm_lines(
    palette = "plasma",breaks = c(0, 5,10,15,20,25,30,35,40,45,50, 100),
    lwd = "TripNo",
    scale = 5,
    title.lwd = "TripNo",
    alpha = 0.5,
    col = "TripNo",
    title = "Active travel",
    legend.lwd.show = TRUE
  ) +
  tm_scale_bar() +
  tm_layout(
    legend.bg.alpha = 0.5,
    legend.bg.color = "white"
  )


#sum(desire_line2$TripNo_O)

#save the desire line as shap file
#st_write(desire_lines, "ShapefileDir/Trucks_Upto11tons_failedScenario2_desirelines_external.shp",driver="ESRI Shapefile")

