library(tidyverse)
library(stringr)
library(lubridate)
#setwd("C:\\Users\\charliewang\\Desktop\\__統編資料整理__")
tsic <- read_csv("Industry_category\\tsic.csv")

tsic <- tsic %>% filter(版本別 == 8) %>%
  select(-版本別, -定義, -備註)
names(tsic) <- c("ID", "level", "name", "A")

A <- tsic %>% filter(level == '大') %>% mutate(G = name, note = ID) %>% select(note, G)

AA <- tsic %>% filter(level == '中') %>% mutate(B = name, note = ID) %>% select(note, B)
aa <- AA %>% filter(str_detect(note, "-")) %>% mutate(note = str_split(note, "-")) #處理45-46類型
aa <- aa %>% unnest()
AA <- AA %>% filter(!str_detect(note, "-")) %>% bind_rows(aa) %>% arrange(note)

AAA <- tsic %>% filter(level == '小') %>% mutate(C = name, note = ID) %>% select(note, C)
AAAA <- tsic %>% filter(level == '細') %>% mutate(D = name, note = ID) %>% select(note, D)
AAAAA <- tsic %>% filter(level == '子') %>% mutate(E = name) %>% select(ID, E, A)
tsic <- AAAAA %>%
  mutate(note = str_sub(ID,1,4)) %>%
  left_join(AAAA) %>%
  mutate(note = str_sub(ID,1,3)) %>%
  left_join(AAA) %>%
  mutate(note = str_sub(ID,1,2)) %>%
  left_join(AA) %>%
  mutate(note = A) %>%
  left_join(A) %>%
  mutate(A = G) %>%
  select(ID, A, B, C, D, E) %>%
  mutate(ID = str_replace(ID, "-", ""))

#統編與行業整併-------------------------
GV <- GV %>%
  left_join(tsic[,c("ID", "B")], by = c("行業代號" = "ID")) %>% #View()#僅先以第一個代號做判別
  rename(cat1 = B) %>%
  left_join(tsic[,c("ID", "B")], by = c("行業代號_1" = "ID")) %>%
  rename(cat2 = B) %>%
  left_join(tsic[,c("ID", "B")], by = c("行業代號_2" = "ID")) %>%
  rename(cat3 = B) %>%
  left_join(tsic[,c("ID", "B")], by = c("行業代號_3" = "ID")) %>%
  rename(cat4 = B)

GV <- GV %>%
  mutate(note = paste(cat1, cat2, cat3, cat4, sep = ","),
         note = str_replace_all(note, ",NA", ""),
         note = note %>% strsplit(",") %>% sapply(function(x) paste(unique(x), collapse = ","))) %>%
  select(-starts_with("cat"))

#公務機關與教育機構處理--------------------------------
gov <- read_csv("統編產業分類\\gov.csv")
names(gov) <- c("taxID", "company_name")
gov <- gov %>% mutate(note = '政府機關')

univsty <- read_csv("統編產業分類\\university.csv")
univsty <- univsty %>% filter(!is.na(機關所在縣市)) %>% select(統一編號:機關所在縣市)
names(univsty) <- c("taxID", "company_name", "county")
univsty <- univsty %>% mutate(note = "教育單位")

school <- read_csv("統編產業分類\\school.csv")
names(school) <- c('company_name', "taxID")
school <- school %>% mutate(note = "教育單位")

GV <- GV %>%
  bind_rows(gov, univsty, school)

  
#output data-----------------------------------
write_csv(GV, "統編產業分類\\final_GV.csv")
