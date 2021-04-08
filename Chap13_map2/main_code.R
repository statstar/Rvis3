if(!require(ggplot2))install.packages("ggplot2"); library(ggplot2)
if(!require(ggmap)) install.packages("ggmap"); library(ggmap)

# ggmap(get_map(location='south korea', zoom=7))
#error 발생 : google maps API terms of Service가 변경됨

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")


# 좌, 우, 위, 아래의 값을 지정해 주어야 지도 그림이 그려짐
kor <- c(left = 124, bottom = 33, right = 132, top = 39)
map <- get_stamenmap(kor, zoom = 6, maptype = "toner-lite")
ggmap(map)
map <- get_stamenmap(kor, 
                     zoom = 6, maptype = "watercolor")
ggmap(map)


if(!require(dplyr)) install.packages("dplyr");library(dplyr)
if(!require(forcats)) install.packages("forcats");library(forcats)

setwd("C:/Users/USER/Dropbox/R-work/vis/day2/extra")

load(file='crime.rda')
head(crime)
table(crime$offense)

# define helper
`%notin%` <- function(lhs, rhs) !(lhs %in% rhs)

# reduce crime to violent crimes in downtown houston
violent_crimes <- crime %>% 
  filter(
    offense %notin% c("auto theft", "theft", "burglary"),
    -95.39681 <= lon & lon <= -95.34188,
    29.73631 <= lat & lat <=  29.78400
  ) %>% 
  mutate(
    offense = fct_drop(offense), # refactor 
    offense = fct_relevel(offense, 
                          c("robbery", "aggravated assault", "rape", "murder") #factor order
    )
  )

head(violent_crimes)
table(violent_crimes$offense)

# use qmplot to make a scatterplot on a map
qmplot(lon, lat, data = violent_crimes, maptype = "toner-lite", color = I("red"))

qmplot(lon, lat, data = violent_crimes, maptype = "toner-lite", geom = "density2d", color = I("red"))

robberies <- violent_crimes %>% filter(offense == "robbery")

qmplot(lon, lat, data = violent_crimes, geom = "blank", 
       zoom = 15, maptype = "toner-background", darken = .7, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA)+
  scale_fill_gradient2("Robbery\nPropensity", low = "white", mid = "yellow", high = "red", midpoint = 650)

qmplot(lon, lat, data = violent_crimes, maptype = "toner-background", color = offense) + 
  facet_wrap(~ offense)

#유럽지도 가져오기
europe <- c(left = -12, bottom = 35, right = 30, top = 63)
get_stamenmap(europe, zoom = 5) %>% ggmap()


kor <- c(left = 124, bottom = 33, right = 132, top = 39)
map <- get_stamenmap(kor, zoom = 7, maptype = "toner-lite")
plot(map)


wifi <- read.csv('wifi.csv', header=T) #wifi.csv
head(wifi)

ggmap(map) + geom_point(data=wifi, aes(x=lon, y=lat, color=company))

ggmap(map) + stat_density_2d(data=wifi, aes(x=lon, y=lat))

ggmap(map) + 
  stat_density_2d(data=wifi, aes(x=lon, y=lat, fill=..level.., alpha=..level..), geom='polygon', size=2, bins=30)
#일단 aes 안에 들어 있는 fill은 문자 그대로 색깔로 채우라는 뜻입니다.
# ..level..은 레벨(level)이 높을수록,  예를 들어 기압이 높거나 고도가 높을수록
# 더 진한 색깔을 칠하라는 뜻입니다.
# alpha는 투명도를 나타냅니다.
# 역시 같은 원리로 레벨이 높으면 불투명하게(색이 더 잘 드러나게)
# 칠하고 낮을 때는 투명하게(희미하게) 칠하라는 의미입니다

# geom='polygon'에서 polygon은 다각형이라는 뜻입니다.
# 기본은 위에서 본 것처럼 선(線)으로 돼 있는데 그것 말고 도형으로 그리라고 명령을 준 겁니다.
# 선을 기준으로 size는 선 굵기, bins는 선 간격이라는 뜻입니다.
# 이름이 이렇게 붙은 건 원래 점을 이은 선을 기준으로 삼고 있는 까닭입니다.

p <- ggmap(map) + stat_density_2d(data=wifi, aes(x=lon, y=lat, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28)
p
p<-p + scale_fill_gradient(low='yellow', high='red', guide=F)
p
p<-p + scale_alpha(range=c(0.02, 0.8), guide=F)
p


airport <- read.csv('airport.csv', header=T) #airport.csv

route <- read.csv('route.csv', header=T) #route.csv

head(airport)
head(route)

ggmap(map) + geom_point(data=airport, aes(x=lon, y=lat, size=3))


p <- ggmap(map) + geom_point(data=airport, aes(x=lon, y=lat, size=3))

p<-p + geom_line(data=route, aes(x=lon, y=lat, group=id))
p
CJU_route<-route[route$airport=="CJU","id"]
route1 <- route[route$id %in% CJU_route,]
p + geom_line(data=route1, aes(x=lon, y=lat, group=id, size=2), col='gold')

# Q. 기상관측소 데이터를 찾아서 지도에 그려보세요.
# stn<- read.csv(choose.files())

if(!require(raster))install.packages("raster"); library(raster)
korea <- getData('GADM', country='kor', level=2)
plot(korea)

ggplot(data=korea, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill='white', color='black')
# 잘 보시면 예전에 못 보던 게 하나 보입니다.
# group : 선을 모아 면을 만들 때도 짝을 맞춰주는 속성
# 이 지도는 옛날 행정구역임

if(!require(rgdal))install.packages("rgdal"); library(rgdal)
if(!require(maptools))install.packages("maptools"); library(maptools)

korea <- readOGR('SIG_201703/TL_SCCO_SIG.shp')#'TL_SCCO_SIG.shp'

ggplot() + geom_polygon(data=korea, aes(x=long, y=lat, group=group), fill='white', color='black')

#19대 대통령 선거 시군구별 득표율
result <- read.csv('result.csv', header=T) #'result.csv'
head(result)
head(korea)

korea1<-merge(korea, result, by.x="SIG_CD", by.y="id")
head(korea1)

SIG_CD<-paste(unique(korea1$SIG_CD))

f.korea <- fortify(korea) #maptools package
f.korea$SIG_CD <-NA

for (i in 1:250){
  f.korea[f.korea$id==(i-1),"SIG_CD"]=SIG_CD[i]  
}

korea2 <- merge(f.korea, result, by.x="SIG_CD", by.y="id")

p<-ggplot() + geom_polygon(data=korea2, aes(x=long, y=lat, group=group, fill=moon))
p + scale_fill_gradient(low='white', high='#004ea2')

if(!require(viridis))install.packages("viridis");library(viridis)

p<-ggplot() + geom_polygon(data=korea2, aes(x=long, y=lat, group=group, fill=moon))
p + scale_fill_viridis()
# 기존에 있는 파레트를 이용해서 그려보자.

p + scale_fill_viridis(direction=-1) + theme_void() + guides(fill=F)
# 색을 반대로, 회색을 지우고, 범례도 지워보자

# 다른 후보들의 그림을 그려보세요.
# 문재인 후보와 홍준표 후보의 차이를 시각적으로 살펴보세요.