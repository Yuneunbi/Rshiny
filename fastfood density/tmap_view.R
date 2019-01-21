
library(dplyr)
library(rgdal)
library(tmap)
library(ggplot2) #fortify쓰기 위함
library(sp)
setwd("E:/2017프로젝트/샤이니/shp")

fastfood = read.csv("food_eng.csv",header = TRUE)



fastfood$brand = as.character(fastfood$brand)
fastfood$si_do = as.character(fastfood$si_do)
fastfood$si_gun_name = as.character(fastfood$si_gun_name)
fastfood$si_gun_district = as.character(fastfood$si_gun_district)

fast <- function(name){
  
  brand=fastfood[grep(name,fastfood$brand),]
  
  brand_ad= paste0(brand$si_do,brand$si_gun_name,brand$si_gun_district)
  brand_freq = data.frame(table(brand_ad),stringsAsFactors=F)
  brand_freq$brand_ad = as.character( brand_freq$brand_ad)
  return(brand_freq)
  
}


lotte=fast("롯데리아")
king = fast("버거킹")
mac = fast("맥도날드")
fastfood=NULL
fastfood=merge(lotte,king,by="brand_ad",all=TRUE)
fastfood= merge(fastfood,mac,by="brand_ad",all=TRUE)

names(fastfood)<- c('district','lotte_freq','king_freq','mac_freq')


korea<-readOGR(dsn="shape file/KOR_adm_shp", layer="KOR_adm2",encoding="UTF-8")
korea@data$NAME_1 = as.character(korea@data$NAME_1)
korea@data$NAME_2 = as.character(korea@data$NAME_2)
korea@data$TYPE_2 = as.character(korea@data$TYPE_2)
korea@data$NAME_3= paste0(korea@data$NAME_1,korea@data$NAME_2,korea@data$TYPE_2)

korea@data<-left_join(korea@data, fastfood, by=c('NAME_3'='district'))

x="lotte_freq"
map1<-qtm(shp=korea,fill=x ,fill.palette="Reds",fill.title="Freq",fill.textNA="NA") +tmap_mode("view")
#tmap_mode를 "view" 모드로 하는 경우, 마우스 드래그로 줌을 자유롭게 할 수 있습니다.
map2 <-qtm(korea, symbols.size=x ,symbols.col="NAME_1")
# 구역 별로 색깔 넣고 원으로 나타내기

map1
map2
