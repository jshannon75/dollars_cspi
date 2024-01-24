library(RSelenium)
library(purrr)
library(tidyverse)
library(rvest)
library(XML)

#Open browser for scraping
rD <- rsDriver(browser = "firefox",
               chromever = NULL)
remDr <- rD$client
#Close connection
remDr$close()
rD$server$stop()

##Create list of city URLs in Georgia ####

state_url="https://www.familydollar.com/locations/ga/"

remDr$navigate(state_url)

page<-remDr$getPageSource()[[1]] %>%
  read_html() 
  
doc <- htmlTreeParse(page, useInternal=T)
links<-xpathSApply(doc, "//a[@href]", xmlGetAttr, "href")
doc_links<-data.frame(links=links) %>%
  filter(str_detect(links,"/locations/") & links!="https://www.familydollar.com/locations/")

write_csv(doc_links,"data/fd_citylist.csv")

### Store pages

storeurl_lookup<-function(url_sel){
  remDr$deleteAllCookies()
  Sys.sleep(3)
  remDr$navigate(url_sel)
  
  page<-remDr$getPageSource()[[1]] %>%
    read_html() 
  
  doc <- htmlTreeParse(page, useInternal=T)
  links<-xpathSApply(doc, "//a[@href]", xmlGetAttr, "href")
  doc_links<-data.frame(store_url=links) %>% 
    filter(str_detect(store_url,"locations/")) %>%
    filter(substr(store_url,nchar(store_url)-1,nchar(store_url)-1) %in% c(0:9)) %>%
    distinct()
  doc_links
}

#storeurl_lookup("https://www.familydollar.com/locations/ga/adel/")

storelinks<-map_df(doc_links$links,storeurl_lookup)

write_csv(storelinks,"data/fd_storelinks.csv")
storelinks<-read_csv("data/fd_storelinks.csv")


### Services extraction ####

xpath_num=1
services_list<-function(xpath_num){
  xpath_sel<-paste("/html/body/div[3]/main/div/div[2]/div[3]/div[1]/div/div[3]/div[4]/span[",xpath_num,"]/img",sep="")
  
  xpath_sel_class<-paste("/html/body/div[3]/main/div/div[2]/div[3]/div[1]/div/div[3]/div[4]/span[",xpath_num,"]",sep="")
  services_xpath_class<-remDr$findElement(using="xpath",
                                    value=xpath_sel_class)
  elementClass<-services_xpath_class$getElementAttribute("class")[[1]]
  
  services_xpath<-remDr$findElement(using="xpath",
                                    value=xpath_sel)
  data.frame(class=elementClass,
             services=services_xpath$getElementAttribute("alt")[[1]])
}

#services_list(1)

services_df<-function(url_sel){
  remDr$deleteAllCookies()
  remDr$navigate(url_sel)
  Sys.sleep(3)

# FindElement(By.XPath("//div[@class='item-inner']/span[@class='']"));
# 
# st_address_x<-remDr$findElement(using="xpath",
#                                  value="/html/body/div[3]/main/div/div[2]/div[3]/div[1]/div/div[3]/div[4]//span[@class='hideitY']")
# st_address_x$getElementAttribute()
#  img<-st_address_x$findElements(using="class","hideitY")
#  img$getElementText()
#  img1$`Reference class`
#  img[1]
 # img$getElementAttribute("src")
 # img$getElementText()
  latx<-remDr$findElement(using="xpath",
                         value="/html/head/meta[20]")
  lat_sel<-latx$getElementAttribute("content")[[1]]
  
  longx<-remDr$findElement(using="xpath",
                          value="/html/head/meta[21]")
  long_sel<-longx$getElementAttribute("content")[[1]]
  
  store_namex<-remDr$findElement(using="xpath",
                                                value="/html/body/div[3]/main/div/div[2]/div[3]/div[1]/div/div[1]/div[1]")
  store_name_sel<-store_namex$getElementText()[[1]]
  
  stradd_x<-remDr$findElement(using="xpath",
                             value="/html/body/div[3]/main/div/div[2]/div[3]/div[1]/div/div[1]/span/span[1]")
  stradd_sel<-stradd_x$getElementText()[[1]]
  
  cityx<-remDr$findElement(using="xpath",
                             value="/html/body/div[3]/main/div/div[2]/div[3]/div[1]/div/div[1]/span/span[2]")
  city_sel<-cityx$getElementText()[[1]]
  
  statex<-remDr$findElement(using="xpath",
                            value="/html/body/div[3]/main/div/div[2]/div[3]/div[1]/div/div[1]/span/span[3]")
  state_sel<-statex$getElementText()[[1]]
  
  services_rawx<-remDr$findElement(using="xpath",
                                  value="/html/body/div[3]/main/div/div[2]/div[3]/div[1]/div/div[3]/div[4]")
  reveal_text<-services_rawx$getElementAttribute("class")[[1]]
  reveal_count<-str_count(reveal_text,"revealY")

  map_df(1:5,services_list) %>%
    mutate(store_url=url_sel,
           store_name=store_name_sel,
           stradd=stradd_sel,
           city=city_sel,
           state=state_sel,
           lat=lat_sel,
           long=long_sel) %>%
    filter(class=="hideitY")
}

test<-services_df("https://www.familydollar.com/locations/ga/lithonia/28805/")

# storelinks10<-sample_n(storelinks,10)
# test<-map_df(storelinks10$store_url,possibly(services_df))

store_services<-map_df(storelinks$store_url,possibly(services_df))
check<-anti_join(storelinks,store_services)
store_services2<-map_df(check$store_url,possibly(services_df))
check2<-anti_join(check,store_services2)

ga_services_all<-store_services %>%
  bind_rows(store_services2) %>%
  mutate(dummy=1) %>%
  pivot_wider(names_from=services,values_from=dummy,values_fill=0) %>%
  right_join(storelinks) %>%
  separate(store_name,into=c("A","store_num"),sep="#",remove=FALSE) %>%
  select(-A) %>%
  select(store_url,store_name,store_num,state,stradd,city,lat,long,everything())
  
write_csv(ga_services_all,"data/georgia_fd_sample_2023_12_04.csv")
