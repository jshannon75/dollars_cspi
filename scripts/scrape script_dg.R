library(RSelenium)
library(purrr)
library(tidyverse)

store_pages<-read_csv("data/dg_store_links.csv") %>%
  mutate(store_url2=str_replace(store_url,
                    "https://www.dollargeneral.com/content/dollargeneral/en/store-directory/","")) %>%
  separate(store_url2,sep="/",into=c("state","city","store_id"),
           remove=FALSE)

#Open browser for scrpaing
rD <- rsDriver(browser = "firefox",
               chromever = NULL)
remDr <- rD$client
remDr$close()
rD$server$stop()

url_sel<-sample_n(store_pages,1)
url_test<-sample_n(store_pages,20)


url_extract<-function(url_sel){
  url_sep<-separate(data.frame(url_temp=
                                 str_replace(url_sel,
                                  "https://www.dollargeneral.com/content/dollargeneral/en/store-directory/","")),
                    col=url_temp,sep="/",into=c("a","b","store_id"))

  remDr$navigate(url_sel)
  Sys.sleep(5)
  
  #Store info
  st_address_x<-remDr$findElement(using="xpath",
                                value="/html/body/div[4]/div/div[2]/div/div/div[1]/div[1]/h1")
  st_address<-st_address_x$getElementText()[[1]]
  
  city_x<-remDr$findElement(using="xpath",
                            value="/html/body/div[4]/div/div[2]/div/div/div[1]/div[1]/div")
  city<-city_x$getElementText()[[1]]
  
  latlong_x<-remDr$findElement(using="xpath",
                             value="/html/body/div[4]/div/div[2]/div/div/div[1]/div[2]")
  lat<-latlong_x$getElementAttribute(attrName = "data-latitude")[[1]]
  long<-latlong_x$getElementAttribute(attrName = "data-longitude")[[1]]
  
  #Store services
  services <- remDr$findElement(using = "xpath",
                                 value = '/html/body/div[4]/div/div[2]/div/div/div[5]/div[2]/ul')
  list_count<-length(services$findChildElements(using="xpath",value="li"))

  list_extract<-function(item_sel){
    value_xsel<-paste("/html/body/div[4]/div/div[2]/div/div/div[5]/div[2]/ul/li[",item_sel,"]/span",sep="")
    value_item<-remDr$findElement(using = "xpath",
                                  value = value_xsel)
    value_item$getElementText()[[1]]
  }
  
  data.frame(store_service=t(data.frame(map(1:list_count,list_extract))),
                     row.names=NULL) %>%
    mutate(store_id=url_sep$store_id,
           address=st_address,
           city_url=city,
           lat=lat,
           long=long)
}

ga<-store_pages %>%
  filter(state=="ga")

ga_services<-map_df(ga$store_url,possibly(url_extract))

check<-anti_join(ga,ga_services)

ga_services2<-map_df(check$store_url,possibly(url_extract))

check2<-anti_join(check,ga_services2)

ga_services3<-map_df(check2$store_url,possibly(url_extract))

check3<-anti_join(check2,ga_services3)

ga_services_all<-ga_services %>%
  bind_rows(ga_services2) %>%
  bind_rows(ga_services3) %>%
  mutate(dummy=1) %>%
  pivot_wider(names_from=store_service,values_from=dummy,values_fill=0) %>%
  right_join(ga) %>%
  select(store_url,state,address,city,city_url,store_id,lat,long,everything()) %>%
  select(-store_url2)

write_csv(ga_services_all,"data/georgia_sample_2023_12_02.csv")
