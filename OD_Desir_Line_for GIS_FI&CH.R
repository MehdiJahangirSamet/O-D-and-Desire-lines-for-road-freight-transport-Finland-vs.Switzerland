#O-D flow presentations
rm(list=objects())
#install.packages("tidyr")
#install.packages("sf")
#install.packages("dplyr")
#install.packages("spDataLarge")
#install.packages("stplanr")      # geographic transport data package
#install.packages("tmap")         # visualization package (see Chapter 8)
#install.packages("dodgr")
#install.packages("pbapply")

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
library(dodgr)
library(pbapply)



#read sf zone centers points
Zones<- st_read ("ShapefileDir","FI_municipality_vs._Postal_code" )

#read sf polygons
PolygonZones<- st_read ("ShapefileDir","BoundaryZones" )

#read rout networks
route_network_sf <- st_read ("ShapefileDir","Link_Network" )

#R <- st_read ("ShapefileDir","Trucks_30-58t_Allroutes_external" )

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
#[69] "Group1_Scenario1"               "Group1_Scenario2"              
#[71] "Group1_Scenario3"               "Group1_Scenario4"              
#[73] "Group1_Scenario5"               "Group2_Scenario1"              
#[75] "Group2_Scenario2"               "Group2_Scenario3"              
#[77] "Group2_Scenario4"               "Group2_Scenario5"              
#[79] "Group3_Scenario1"               "Group3_Scenario2"              
#[81] "Group3_Scenario3"               "Group3_Scenario4"              
#[83] "Group3_Scenario5"               "TripNo"

##########################
#change the PLZ from 4digit to 2 digit
#Primary_OD$fromPLZ<-Primary_OD$fromPLZ2D
#Primary_OD$toPLZ<-Primary_OD$toPLZ2D

#change the Municipality variable name 
names(Zones)[1]<- "Unique_Min"



#####################################
#set filter for the type of Truck among "1-Trucks_Upto_11t_pay" OR "2-Trucks_11to30t_pay" OR "3-Trucks_30to58t_pay"

Primary_OD = filter(Primary_OD, load.capacity.categories == "3-Trucks_30to58t_pay")


#####################################
#Filter for failed scenario, "Under.11t.Scenario.1_Failed" OR "Under.11t.Scenario.2_Failed" OR "Under.11t.Scenario.3_Failed"
# "Group1_Scenario1" OR "Group2_Scenario1" OR "Group3_Scenario1"
#[69] "Group1_Scenario1"               "Group1_Scenario2"              
#[71] "Group1_Scenario3"               "Group1_Scenario4"              
#[73] "Group1_Scenario5"               "Group2_Scenario1"              
#[75] "Group2_Scenario2"               "Group2_Scenario3"              
#[77] "Group2_Scenario4"               "Group2_Scenario5"              
#[79] "Group3_Scenario1"               "Group3_Scenario2"              
#[81] "Group3_Scenario3"               "Group3_Scenario4"              
#[83] "Group3_Scenario5"               "TripNo"
#Primary_OD = filter(Primary_OD, Group1_Scenario1 != 0)
#Primary_OD = filter(Primary_OD, Same.trip.All.leg == 1)



#########################################
#set filter list for destinations and origins for special corridors
#x<- list(16,18,20,49,69,71,72,75,77,81,82,86,91,92,98,106,108,109,111,139,142,143,145,152,164,165,172,179,181,182,186,202,211,216,230,232,235,239,240,241,244,245,250,256,257,265,283,285,286,291,301,316,317,398,399,407,410,418,425,433,434,435,436,444,475,489,494,499,500,504,505,532,535,536,543,560,563,564,576,577,581,588,592,595,601,604,611,616,624,626,630,635,638,680,691,694,704,729,734,738,743,748,751,753,753,755,781,791,837,846,850,851,853,858,859,892,905,908,921,922,927,931,935,980,992)
#Primary_OD = filter(Primary_OD, fromPLZ %in% x)
#Primary_OD = filter(Primary_OD, toPLZ %in% x)


#set filter for external or internal legs

Primary_OD = filter(Primary_OD, fromPLZ != toPLZ)
#names(Primary_OD)



#######################################
#Summary set category for "million.tkm" each Transport category based on "typeOfGoodsNST2007"
#ddply(Primary_OD,~typeOfGoodsNST2007,summarise,Tonkm_of_typeOfGoodsNST2007=sum(unique(million.tkm)))

#Summary set category for emission
#ddply(Primary_OD,~emission,summarise,Tonkm_of_emission=sum(unique(million.tkm)))

#Summary set to all filtered
#data.frame(a=c(sum(Primary_OD$million.journeys),sum(Primary_OD$million.km),sum(Primary_OD$million.t),sum(Primary_OD$million.tkm),sum(Primary_OD$million.liters),sum(Primary_OD$CO2.kt),sum(Primary_OD$million.kWh)))

#setFilter for the same OD trave results
zones_attr= Primary_OD %>%
  group_by(fromPLZ, toPLZ)%>%
  summarize(TripNo=sum(million.journeys),TripDistance=sum(million.km), Triploadkg=sum(million.t), TripTonKm= sum(million.tkm), TripFuel=sum(million.liters), TripCO2= sum(CO2.kt), TripKW= sum(million.kWh)) %>% 
  dplyr::rename(Unique_Min =fromPLZ)


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
#st_write(Abs_zones_od, "ShapefileDir/Trucks_RoadE_0-58t_ODpoints_internal.shp",driver="ESRI Shapefile")


#Desire Lines
od_top10 = Abs_zones_od %>% 
  arrange(desc(TripNo)) %>% 
  top_n(10, wt = TripNo)

od_top10


#od_intra = filter(Primary_OD, fromPLZ == toPLZ)
od_inter = filter(zones_od, Unique_Min != toPLZ)
names(od_inter)
od_inter<-movetolast(od_inter, c("Postal_Cod", "Name_of_th", "Name_of__1","X", "Y","geometry"))
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
    tm_shape(desire_lines)+
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
names(desire_lines)

#[1] "Unique_Min"     "toPLZ"          "TripNo"         "TripDistance"  
#[5] "Triploadkg"     "TripTonKm"      "TripFuel"       "TripCO2"       
#[9] "TripKW"         "TripNo_D"       "TripDistance_D" "Triploadkg_D"  #
#[13] "TripTonKm_D"    "TripFuel_D"     "TripCO2_D"      "TripKW_D"      
#[17] "Postal_Cod"     "Name_of_th"     "Name_of__1"

#############################
#############################
#line2routes
#route_localsln, from, to, l = NULL)
#names(Zones)[1]<-"geo_code"
#names(flow)[1]<-"geo_code"


#flow<-od_inter
#zones<-Zones
#desire_lines<- od2line(
#  flow,
#  zones,
#  destinations = NULL,
#  zone_code = names(zones)[1],
#  origin_code = names(flow)[1],
#  dest_code = names(flow)[2],
#  zone_code_d = NA,
#  silent = FALSE
#)



############################################
##############################################
#only one rout can be produced per run!!!!!!!!!!!!

#data(route_network_sf)
#rnet <- overline(route_network_sf, attrib = "TripNo")
#sln <- SpatialLinesNetwork(rnet)


sln <- SpatialLinesNetwork(route_network_sf)
weightfield(sln)
str(sln)
sln_clean_graph(sln)


##########################
##########################
#point to point routes

#desire_lines$geometry
#nrow(desire_lines)

#for(i in 1:nrow(desire_lines)){
#  desire_lines$Xstart[i]<-sf::st_coordinates(desire_lines)[2*i-1,1]
#  desire_lines$Ystart[i]<-sf::st_coordinates(desire_lines)[2*i-1,2]
#  desire_lines$XEnd[i]<-sf::st_coordinates(desire_lines)[2*i,1]
#  desire_lines$YEnd[i]<-sf::st_coordinates(desire_lines)[2*i,2]
#}

#from <- c(desire_lines$Xstart,desire_lines$Ystart)
#to <- cbind(desire_lines$XEnd,desire_lines$YEnd)

#from<-from[,1:2]
#to<-to[1,]

#weightfield(desire_lines,TripNo)<-TripNo
#l1<-desire_lines[1:4,]
#r<-0
#r <- route_local(sln, from=NULL, to=NULL, l=desire_lines[1,])

#route_dodgr(from = NULL, to = NULL, l = NULL, net = NULL)

#r_Summerized <- sum_network_routes(sln, start, end, sumvars, combinations = FALSE)


#Net_dodgr<- weight_streetnet(route_network_sf, wt_profile = "length", wt_profile_file = NULL,
#                             turn_penalty = FALSE, type_col = "motorcar", id_col = "osm_id",
#                            keep_cols = NULL, left_side = FALSE)
#i1<-0
#myroutefunction <- function(){
#  #i1<-i1+1
#  route1 <- route_local(sln, from=NULL, to=NULL, l=desire_lines[1,])
#  return(route1)
#}

##################################
#################################
#building my network

#R<-route_local(sln, from=NULL, to=NULL, l=desire_lines[1,])
#R$TripNo <- as.data.frame(desire_lines)[1,"TripNo"]
#R$TripDistance<-as.data.frame(desire_lines)[1,"TripDistance"]
#R$Triploadkg<-as.data.frame(desire_lines)[1,"Triploadkg"]
#R$TripTonKm<-as.data.frame(desire_lines)[1,"TripTonKm"]
#R$TripFuel<-as.data.frame(desire_lines)[1,"TripFuel"]
#R$TripCO2<-as.data.frame(desire_lines)[1,"TripCO2"]
#R$TripKW<-as.data.frame(desire_lines)[1,"TripKW"]

R<-NULL


for (i in 1:nrow(desire_lines)) {
  r<- tryCatch(route_local(sln, from=NULL, to=NULL, l=desire_lines[i,]), error=function(e) NULL)
  if(is.null(r)){next()}
  t<-nrow(r)
  Sumlength<- sum(r$length)
  for (j in 1:t){
  ratio<-as.data.frame(r)[j,"length"]/Sumlength
  r[j,"TripNo"] <- as.data.frame(desire_lines)[i,"TripNo"]
  r[j,"TripDistance"]<-as.data.frame(desire_lines)[i,"TripDistance"]*ratio
  r[j,"Triploadkg"]<-as.data.frame(desire_lines)[i,"Triploadkg"]
  r[j,"TripTonKm"]<-as.data.frame(desire_lines)[i,"TripTonKm"]*ratio
  r[j,"TripFuel"]<-as.data.frame(desire_lines)[i,"TripFuel"]*ratio
  r[j,"TripCO2"]<-as.data.frame(desire_lines)[i,"TripCO2"]*ratio
  r[j,"TripKW"]<-as.data.frame(desire_lines)[i,"TripKW"]*ratio
  }
  
  R<-rbind(R,r)
  
}


###################
####################
#create aggrigated rout network
rnet <- overline2(R, attrib = "TripCO2")

#sln2 <- SpatialLinesNetwork(rnet)

############################
###########################
#plot it
#plot(sln)


plot(route_network_sf$geometry)

plot(rnet,col = "red", add = TRUE,lwd = rnet$TripNo/mean(rnet$TripNo)*.2)



tm_shape(PolygonZones) + tm_borders() +
  #tm_shape(Abs_zones_od)+
  #tm_symbols(size = "TripNo",shapes = "25",
  #           legend.format = list(text.align="right", text.to.columns = TRUE))+
  tm_shape(rnet)+
  tm_lines(
    palette = "plasma",breaks = c(0, 5,10,15,20,25,30,35,40,45,50, 100),
    lwd = "TripCO2",
    scale = 5,
    title.lwd = "TripCO2_kton",
    alpha = 0.5,
    col = "red",
    title = "Trucks more than 30t payload in million",
    legend.lwd.show = TRUE
  ) +
  tm_scale_bar() +
  tm_layout(
    legend.bg.alpha = 0.5,
    legend.bg.color = "white"
  )


#r2 <- route_local(sln = sln, cents_sf[3, ], cents_sf[4, ])
#plot(r2$geometry, add = TRUE, col = "blue", lwd = 3)


#desire_lines[1,3]
#str(desire_lines)
#line2route(desire_lines[1,],route_fun =myroutefunction())
#desire_lines[1,]
#myroutefunction()

# }


#sum(desire_line2$TripNo_O)

#save the desire line as shap file
#st_write(rnet, "ShapefileDir/Trucks_30-58t_TripCO2kt_Routes_external.shp",driver="ESRI Shapefile")

