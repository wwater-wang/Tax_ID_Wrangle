library(tidyverse)
library(stringr)
library(lubridate)
#setwd("C:\\Users\\charliewang\\Desktop\\__統編資料整理__")

GV <- read_csv("BGMOPEN1.csv",
               col_types = cols(
                 統一編號 = col_character()
               )
               )
GV <- GV %>% select(-starts_with("名稱")) %>%
  filter(!is.na(統一編號)) %>%
  mutate(設立日期 = ymd(as.numeric(設立日期)+19110000))

#縣市清整------------------------------
clean_county <- function(x){
  x <- str_replace(x, "臺北縣", "新北市") #升格直轄市
  x <- str_replace(x, "臺南縣", "臺南市")
  x <- str_replace(x, "高雄縣", "高雄市")
  return(x)
}

GV <- GV %>%
  mutate(county = ifelse(str_detect(營業地址, "縣|市"), str_sub(營業地址,1,3), ""),
         county = clean_county(county))
GV %>% filter(city == "平鎮")
#鄉鎮市區
clean_city <- function(x){
  x <- str_replace(x, "光鎮","") # 改制後升格 or 特殊
  x <- str_replace(x, "土城市","土城區")
  x <- str_replace(x, "大社鄉","大社區")
  x <- str_replace(x, "大寮鄉","大寮區")
  x <- str_replace(x, "大樹鄉","大樹區")
  x <- str_replace(x, "仁武鄉","仁武區")
  x <- str_replace(x, "永康市","永康區")
  x <- str_replace(x, "汐止市","汐止區")
  x <- str_replace(x, "岡山鎮","岡山區")
  x <- str_replace(x, "東勢鎮","東勢區")
  x <- str_replace(x, "板橋市","板橋區")
  x <- str_replace(x, "林口鄉","林口區")
  x <- str_replace(x, "林園鄉","林園區")
  x <- str_replace(x, "梓官鄉","梓官區")
  x <- str_replace(x, "鳥松鄉","鳥松區")
  x <- str_replace(x, "旗山鎮","旗山區")
  x <- str_replace(x, "鳳山市","鳳山區")
  x <- str_replace(x, "樹林市","樹林區")
  x <- str_replace(x, "鹽水鎮","鹽水區")
  x <- str_replace(x, "那瑪夏鄉","那瑪夏區")
  x <- str_replace(x, "伸港區|臺北市|新竹市|關西區|學園區|平菜市|西門市","")
  x <- ifelse(is.na(x), "", x)
  return(x)
}

GV <- GV %>%
  mutate(city = str_replace(營業地址, county, ""), #排除縣市的'市'
         city = str_sub(city, 1, 4),
         city = ifelse(str_detect(city, "三地門|阿里山|那瑪夏|太麻里"),
                       str_extract(city, ".{1,4}[鄉|鎮|市|區]"),#鄉鎮市區長度不大於4
                       str_extract(city, ".{1,2}[鄉|鎮|市|區]"))) %>% #鄉鎮市區長度不大於2
  mutate(city = clean_city(city),
         city = ifelse(str_detect(營業地址, "香山區"), "香山區", city))

#村里
GV <- GV %>%
  mutate(village = paste(county, city, sep = ""),
         village = str_replace(營業地址, village, ""),
         village = ifelse(str_detect(village, "哨船頭里|拉芙蘭里|達卡努瓦里|南沙魯里|頭家東里|烏樹林里|鯉魚潭村"),
                          str_extract(village, ".{3,4}[村|里]"),
                          str_extract(village, ".{2}[村|里]"))) %>%
  mutate(village = ifelse(city == "南庄鄉" & str_detect(營業地址, "東村|西村"),
                          str_extract(營業地址, "[東|西]村"),
                          village))

GV %>% filter(!is.na(總機構統一編號))

names(GV) <- c("address", "taxID", "head_taxID", "company_name", "Capital", "set_date", "use_invoice",
               "行業代號", "行業代號_1", "行業代號_2", "行業代號_3", "county", "city", "village")

#待處理區---------------------------
# 區鎮市後面只會接-->"里" 鄉後面只會接-->"村" 將錯寫的改正
#特殊字 統一用: 臺-->台 豊-->豐