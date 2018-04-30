library(tidyverse)
library(jsonlite)
library(tidytext)
library(wordcloud)

#######################
data scraping
#######################
options(scipen=999)

api_key <- "****************************"

nutrient_codes_raw <- fromJSON(paste0("https://api.nal.usda.gov/ndb/list?format=json&lt=n&sort=id&api_key=", api_key, "&max=190"))

nutrient_codes_clean <- nutrient_codes_raw$list$item %>%
  select(2:3)

food_codes_urls <- data.frame(base_url = paste0("https://api.nal.usda.gov/ndb/list?format=json&lt=f&sort=n&api_key=", api_key, "&max=500&offset="), offset = as.character(seq(from = 0, to = 220113, by = 500))) %>%
  unite(url, base_url, offset, sep = "") 

food_codes_clean <- data.frame(id = NA, name = NA)


for (i in 1:nrow(food_codes_urls)) {
  
  food_codes_raw <- fromJSON(food_codes_urls[i, ])
  
  food_codes_temp <- food_codes_raw$list$item %>%
    select(2:3)
  
  food_codes_clean <- bind_rows(food_codes_clean, food_codes_temp) 
  
  
}

rm(food_codes_temp)
rm(i)
rm(food_codes_urls)


food_data_urls <- food_codes_clean %>%
  select(1) %>%
  rename(code = id) %>%
  filter(!is.na(code)) %>%
  mutate(ndbno = "&ndbno=", 
         index = as.numeric(row.names(.)),
         group = cut(index, breaks = 230099 / 19.5)) %>% 
  unite(ndbno_code, ndbno, code, sep = "") %>%
  group_by(group) %>% 
  select(1, 3) %>%
  mutate(ndbno_grouped = paste0(ndbno_code, collapse = "")) %>%
  ungroup() %>%
  select(3) %>%
  distinct() %>%
  mutate(url = paste0("https://api.nal.usda.gov/ndb/V2/reports?type=f&format=json&api_key=", api_key)) %>%
  unite(food_data_url, url, ndbno_grouped, sep = "") %>%
  select(1)

food_data_urls <- as.data.frame(food_data_urls) 

food_ingredients_clean <- data.frame(ndbno = NA, ingredients = NA)

food_nutrients_clean <- data.frame(ndbno = NA, nutrient = NA, value = NA, unit = NA)

for (i in 1:nrow(food_data_urls)) {
  
  food_data_raw <- fromJSON(food_data_urls[i, ])
  print(i)
  
  food_iterate <- food_data_raw$foods$food
  
  food_data_unpacked_ingredients <- food_data_raw$foods$food$ing
  
  if (!is.null(food_data_unpacked_ingredients)) {
    
    for (i in 1:nrow(food_data_unpacked_ingredients)) {
      
      food_ingredients <- food_data_unpacked_ingredients %>%
        select(1) %>%
        mutate(id = as.numeric(row.names(.))) %>%
        filter(id == i) %>%
        mutate(ndbno = food_data_raw$foods$food$desc$ndbno[[i]]) %>%
        select(3, 1) %>%
        rename(ingredients = desc)
      
      food_ingredients_clean <- bind_rows(food_ingredients_clean, food_ingredients)
      print(i)
    }
  }
  
  food_data_unpacked_nutrients <- food_data_raw$foods$food$nutrients
  
  for (i in 1:nrow(food_iterate)) {
    
    if (nrow(food_data_unpacked_nutrients[[i]]) > 0) { 
      
      food_nutrients <- food_data_unpacked_nutrients[[i]] %>%
        select(name, value, unit) %>%
        mutate(ndbno = food_data_raw$foods$food$desc$ndbno[[i]],
               value = as.character(value)) %>%
        rename(nutrient = name)
      
      food_nutrients_clean <- bind_rows(food_nutrients_clean, food_nutrients)
    }
    print(i)
  }
}

###################################
data cleaning
###################################
load("food_codes_clean.RData")
load("food_ingredients_clean.RData")
load("food_nutrients_clean.RData")

nutrients <- food_nutrients_clean
nutrients$value <- as.numeric(nutrients$value)
nutrients <- nutrients[which(nutrients$value!=0),]

ingredient <- food_ingredients_clean[-1,2]%>%
  gsub("\\.","",.)%>%
  gsub("\\(.*?\\)","",.)%>%
  gsub("CONTAINS:","",.)%>%
  str_split(.,",",simplify = TRUE)

code <- food_codes_clean[-1,]

###################################
data analysis
###################################
#find top 10 nutrients in all foods
nutrients%>%
  group_by(nutrient)%>%
  summarise(count = n())%>%
  arrange(desc(count))%>%
  head(10)%>%
  ggplot(aes(reorder(nutrient,count),count,fill = nutrient))+
  geom_bar(stat = "identity")+geom_text(aes(label = count),vjust = 1.5,colour = "white")+
  theme(axis.text.x = element_text(angle = 90,size = 12),legend.position = "none")+ggtitle("Top 10 nutrient")+theme(plot.title = element_text(hjust = 0.5))

#top 10 foods that contain specific energy
#energy
energy <- nutrients[which(nutrients$nutrient=="Energy"),]
newenergy <- 
  tibble("id"=energy$ndbno,"nutrient"=energy$nutrient,"value"=energy$value,"unit"=energy$unit)

for (i in 1:215127) {
  if(newenergy$unit[i]=="kJ"){
    newenergy$value[i] <- newenergy$value[i]*0.239006
    newenergy$unit[i] <- "kcal"
  }
}

newenergy <- newenergy%>%
  left_join(food_codes_clean,by="id")%>%
  arrange(desc(value))%>%
  head(10)

#sugar
sugar <- nutrients[which(nutrients$nutrient=="Sugars, total"),]
newsugar <- 
  tibble("id"=sugar$ndbno,"nutrient"=sugar$nutrient,"value"=sugar$value,"unit"=sugar$unit)

newsugar <- newsugar%>%
  left_join(food_codes_clean,by="id")%>%
  arrange(desc(value))%>%
  head(10)

#protein
protein <- nutrients[which(nutrients$nutrient=="Protein"),]
newprotein <- 
  tibble("id"=protein$ndbno,"nutrient"=protein$nutrient,"value"=protein$value,"unit"=protein$unit)

newprotein <- newprotein%>%
  left_join(food_codes_clean,by="id")%>%
  arrange(desc(value))%>%
  head(10)

#calcium
calcium <- nutrients[which(nutrients$nutrient=="Calcium, Ca"),]
newcalcium <- 
  tibble("id"=calcium$ndbno,"nutrient"=calcium$nutrient,"value"=calcium$value,"unit"=calcium$unit)

newcalcium <- newcalcium%>%
  left_join(food_codes_clean,by="id")%>%
  arrange(desc(value))%>%
  head(10)

newenergy

newsugar

newprotein

newcalcium

#combine two different nutrients
two_nutrient<-nutrients %>%
  filter(nutrient =="Energy" | nutrient == "Carbohydrate, by difference") %>%
  filter(unit=="kcal" | unit =="g") %>%
  select(-unit) %>%
  spread(key=nutrient,value=value) %>%
  filter(!is.na(`Carbohydrate, by difference`)) %>%
  filter(!is.na(Energy)) 

ggplot(two_nutrient,aes(Energy,`Carbohydrate, by difference`)) +
  geom_point()
  
  colnames(two_nutrient)<-c("id","c","e")
two_nutrient %>%
  filter(e>1200) %>%
  filter(c>60) %>%
  left_join(food_codes_clean,by="id") %>%
  select(name,c,e)

two_nutrient %>%
  filter(e>1200) %>%
  filter(c<60) %>%
  left_join(food_codes_clean,by="id") %>%
  select(name,c,e)
  
#top 10 ingredient in all foods
  newingredient <- ingredient%>%
  as.character()%>%
  str_trim(side = "both")%>%
  tibble("ingredient"=.)%>%
  filter(!is.na(ingredient))%>%
  filter(ingredient != "")%>%
  group_by(ingredient)%>%
  summarize(count = n())%>%
  arrange(desc(count))

wordcloud(words = newingredient$ingredient,freq = newingredient$count,
            random.order = FALSE,max.words = 70,rot.per=0.60, 
            colors=brewer.pal(8, "Dark2"))

#specific analysis on juice
juice <- grep("JUICE",code[,2])
newcode <- code[juice,]
juicecode <-
  tibble("ndbno"=newcode$id,"name"=newcode$name)

meannutrient<-nutrients%>%
  group_by(nutrient)%>%
  summarize(avemean=mean(value,na.rm=TRUE))

juicecode%>%
  left_join(nutrients,by="ndbno")%>%
  group_by(nutrient)%>%
  summarise(mean=mean(value,na.rm=TRUE))%>%
  arrange(desc(mean))%>%
  head(20)
  
  juicecode%>%
     left_join(nutrients,by="ndbno")%>%
     group_by(nutrient)%>%
     summarise(mean=mean(value,na.rm=TRUE))%>%
     left_join(meannutrient,by="nutrient")%>%
     filter(mean>avemean)%>%
     arrange(desc(mean))
