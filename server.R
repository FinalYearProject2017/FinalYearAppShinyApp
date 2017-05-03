library(shiny)
library(ggplot2)
library(WDI)
library(RCurl)
library(dplyr)
library(data.table)
# library(showtext) # for fonts

# font.add.google("Poppins", "myfont")
# showtext.auto()

# Demo Indicators
# EG.ELC.HYRO.ZS
# NY.GDP.PCAP.KD

load("good_series.rda")
final_df = read.csv("final_data.csv")
final_df[sapply(final_df, is.numeric)] <- lapply(final_df[sapply(final_df, is.numeric)],
                                                 function(x) ifelse(is.na(x), 0, x))
country_groups <- c("1A","4E","Z4","7E","Z7","XC","EU","F1","XD","XE","XJ",
                    "ZJ","XL","XM","XN","XO","ZQ",
                    "XP","XQ","XU","XR","XS","OE","8S","ZF","ZG","XT","1W")

shinyServer(function(input, output, session) {
  output$mytable = renderDataTable({good_series})
  output$countryTable = renderDataTable({read.csv("countryTable.csv",stringsAsFactors = F)})
  output$imp_feature_table = renderDataTable({read.csv("imp_features.csv",stringsAsFactors = F)})
   the_data <- reactive({
      series <- input$indicator
        x <- read.csv("indicators.csv", strip.white = T)
        x <- x %>% rename(iso2c = CountryCode, year = Year, country = CountryName)
        value <- x[[input$indicator]]
        x <- x %>% select(iso2c,year,country)
        x <- cbind(x,value)
      return(x)
   })

   the_plot <- reactive({
      x <- the_data()
      series <- input$indicator

      if(input$incl_groups == "Individual Countries"){
         x <- x %>%
            filter(!iso2c %in% country_groups)
      }

      if(input$incl_groups == "Country Groups"){
         x <- x %>%
            filter(iso2c %in% country_groups)
      }

      if(nrow(x) == 0){
         x <- the_data()
        updateSelectInput(session, "incl_groups", selected = "Both")
      }

      selected_countries <- x %>%
         group_by(country) %>%
         summarize(n = sum(!is.na(value)),
                   ave = mean(value, na.rm = TRUE, tr = 0.2)) %>%
         filter(n > input$minimum) %>%
         arrange(ave)

      if(input$fixed_scale){
         the_facet <- facet_wrap(~country)
      } else {
         the_facet <- facet_wrap(~country, scales = "free_y")
      }

      if(input$smoother) {
         the_smooth <- geom_smooth(se = FALSE, span = input$span, method = "loess")
      } else {
         the_smooth <- NULL
      }

      if(input$sample_only & nrow(selected_countries) > 16){
         set.seed(input$samp_count)
         selected_countries <- selected_countries[sample(1:nrow(selected_countries), 16, replace = FALSE), ] %>%
            arrange(ave)

      }

      if(input$singleCountry){
        p1 <- x %>% filter(iso2c == input$countryName) %>%
              filter(!is.na(value)) %>%
              ggplot(aes(x = year, y = value)) +
              geom_point(size = 1) +
              the_smooth +
              theme_grey(10) +
              theme(legend.position = "none") +
              ggtitle(good_series[good_series$indicator == input$indicator, 2])
      }else{
          p1 <- x %>%
            filter(country %in% selected_countries$country) %>%
            filter(!is.na(value)) %>%
            mutate(country = factor(country, levels = selected_countries$country)) %>%
            ggplot(aes(x = year, y = value, colour = country)) +
            geom_point(size = 1) +
            the_facet +
            the_smooth +
            theme_grey(10) +
            theme(legend.position = "none") +
            ggtitle(good_series[good_series$indicator == input$indicator, 2])
      }
      return(p1)
   })
  
   get_most_important = reactive({
     
     fname = paste0("countryData/",input$imp_country,".csv")
     country_df = read.csv(fname)
     country_df[is.na(country_df)] = 0
     c = as.data.frame(cor(country_df %>% 
                             select(-c(CountryCode,CountryName,Year)))) %>%
       select_("NY.GNP.MKTP.CN")

     correlations = setDT(c, keep.rownames = TRUE)[]
     correlations = correlations %>% filter(rn != "NY.GNP.MKTP.CN") %>% 
       filter(NY.GNP.MKTP.CN != 1) %>% filter(NY.GNP.MKTP.CN != -1) %>% 
       arrange(NY.GNP.MKTP.CN)
     
     if(nrow(correlations) > 20){
      
      top_5 = correlations %>% slice(1:10)
      idx_1 = nrow(correlations)-10
      bottom_5 = correlations %>% slice(idx_1:nrow(correlations))
      correlations = rbind(top_5,bottom_5)
     }
     
     plt = ggplot(correlations,aes(x=rn,y=NY.GNP.MKTP.CN)) +
       geom_bar(stat="identity") +
       ggtitle(input$imp_country) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       ylab("Correlation to GNI") + xlab("Indicator Name")
     return(plt)
   })
   output$the_plot <- renderPlot(the_plot(), res = 100)
   output$the_importance_plot = renderPlot(get_most_important(), res = 100)
})
