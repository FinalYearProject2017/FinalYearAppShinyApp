library("dplyr")
# library("corrplot")
library("Amelia")
library("data.table")

indicators <- read.csv("indicators.csv")
load("good_series.rda")
eci_df = read.csv("country_all_ranking_en.csv")
eci_countries = eci_df %>% filter(Year == 1990) %>% select(Country)
good_countries = c()
# get_enough_data <- function(country_iso2c = c("IN","SA")) {
  
  
  for(c in eci_countries$Country){
  
    tmp_df <- indicators %>% filter(CountryName == c)
    good_ind <- c()
    
    for(i in colnames(tmp_df)){
    if (sum(is.na(tmp_df[i])) < 10) {
      good_ind <- append(good_ind,i)
    }
  }
  tmp_df <- tmp_df[good_ind]
  if ("NY.GNP.MKTP.CN" %in% colnames(tmp_df)){
    good_countries <- append(good_countries,c)
    fname = paste0("countryData/",c,".csv")
    write.csv(file = fname,tmp_df,row.names = F)
  }}
# }

nice_df <- get_enough_data(c("SA","IN","CN","US","DE"))
indicator_topic = read.csv("../input/Series.csv") %>% select_("Topic","SeriesCode")
good_ind = good_series %>% filter(indicator %in% colnames(nice_df))
ind_name_df = merge(x = indicator_topic, y = good_ind, by.x = "SeriesCode", by.y = "indicator") 

library(ggplot2)
var = "PA.NUS.ATLS"
x = ind_name_df %>% filter(SeriesCode == var) %>% select(name)
ggplot(final_df %>% select(-c(CountryCode)),
              aes(x=Year,y=PA.NUS.ATLS,color=CountryName)) + geom_line() + ggtitle(x$name)

final_df = read.csv("final_data.csv")
final_df[sapply(final_df, is.numeric)] <- lapply(final_df[sapply(final_df, is.numeric)],
                                                 function(x) ifelse(is.na(x), 0, x))

get_most_important = function(Country = "India"){

  correlations <- as.data.frame(cor(final_df %>% 
                      filter(CountryName == Country) %>% 
                      select(-c(CountryCode,CountryName,Year)))) %>% 
    select_("NY.GNP.MKTP.CN")
  
  setDT(correlations, keep.rownames = TRUE)[]
  correlations = correlations %>% filter(rn != "NY.GNP.MKTP.CN")
  plt = ggplot(correlations,aes(x=rn,y=NY.GNP.MKTP.CN)) + geom_bar(stat="identity") + ggtitle(Country)  

}
p = get_most_important(Country = "Germany")
col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white", 
                           "cyan", "#007FFF", "blue","#00007F"))

corrplot(correlations,type="lower",order="FPC",col=col1(100))






