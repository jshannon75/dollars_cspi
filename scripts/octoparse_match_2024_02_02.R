library(tidyverse)
library(readxl)

octo_dg<-read_xlsx("data/dg_gastores_services_2024_02_01.xlsx")
octo_cities<-octo_dg %>%
  distinct(Street,City) %>%
  separate(City,into=c("city","statezip"),sep=",") %>%
  mutate(city1=tolower(city))

octo_cities_count<-octo_cities %>%
  count(city1)

urls<-read_csv("data/dg_gastores_2024_02_01.csv")

urls1<-urls %>%
  separate(URL,into=c("a","b","c","d","e","f","g","h","city1","j"),sep="/") %>%
  count(city1,name="urlcount")

octo_match<-octo_cities_count %>%
  full_join(urls1)
