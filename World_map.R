# 首先导入我们需要的几个 R 包：
library(dplyr) #数据预处理
library(maptools) #用于读取地图矢量数据
require(rgdal) #读取地图数据
library(ggplot2) #绘制地图
library(grid) #图形嵌套
library(plyr)
library(sf)
library(readr)
# 导入世界地图并把 crs 转换成 4326（其实这份数据的 crs 就是 4326）：

worldmap <- read_sf("https://img.hcharts.cn/mapdata/custom/world-palestine-highres.geo.json") %>% st_transform(crs = 4326)
#把疫情数据读取进来并根据经纬度坐标把这个数据框转换成 sf 对象，同样使用 4326 投影坐标系，这样地图和数据才不会分家：
CovURL <- RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
datacase <- read.csv(text = CovURL, check.names = F)
col_name <- colnames(datacase)
Date <- col_name[length(col_name)]
con_sf <- read.csv(text = CovURL, check.names = F) %>%
    select("Country/Region", Lat, Long, Date) %>%
    `colnames<-`(c("Country", "lat", "long", "con")) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

con_sf2 <- read.csv(text = CovURL, check.names = F) %>%
    select("Country/Region", Date) %>%
    `colnames<-`(c("Country",  "con"))

con_df <- aggregate(con ~ Country, data = con_sf2, sum)

# 最后绘制地图：

mybreaks <- c(1, 20, 100, 1000, 50000)
ggplot(worldmap) +
    geom_sf(size = 0.1, color = "black", fill = NA) +
    geom_sf(data = con_sf, aes(size = con,color = con,)) +
    theme(panel.grid = element_blank()) +
    scale_size_continuous(name = "Confirmed cases",
                          trans = "log",
                          range = c(1, 7),
                          breaks = mybreaks,
                          labels = c("1-19", "20-99", "100-999", "1,000-49,999", "50,000+")) +
    scale_color_viridis_c(option = "inferno",
                          name = "Confirmed cases",
                          trans = "log",
                          breaks = mybreaks,
                          labels = c("1-19", "20-99", "100-999", "1,000-49,999", "50,000+")) +
    guides(color = guide_legend()) +
    labs(title = "Global distribution of COVID-19",
         subtitle = paste0("Deadline: ", "0",Date,"20"),
         caption = "Data source: CSSEGISandData/COVID-19: Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE\nhttps://github.com/CSSEGISandData/COVID-19\nDrawing：Jiachun Li")

intersect(worldmap$)
