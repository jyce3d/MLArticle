library(rvest)

process_surface <- function( icons) {
  sqm2 <- c()
  for (i in 1:length(icons)) {
    
    itag <- icons[i] %>% html_nodes("i.property-icon-surface")
    
    if (length(itag)>0) {
      children <- icons[i] %>% html_children()
      sqm2[i] <- children[4] %>% html_text()
      
    } else sqm2[i] <-"NA"
    
    
  }
  return(sqm2)
}

  html_data <- read_html('https://www.immocube.be/fr/component/properties/?view=list&page=8&view=list&goal=0')

  type <- html_data %>% html_nodes("div.prop-type") %>% html_text() 
  city <-html_data %>% html_nodes("div.prop-city") %>% html_text()
  price <- html_data %>% html_nodes("div.prop-price") %>% html_text() 

  icons <- html_data %>% html_nodes("div.icons")
  sqm2 <- process_surface(icons)


  pricedf <- as.data.frame(price)
  pricedf <- pricedf[pricedf$price!="",]
  pricedf <- gsub("\t|\n", "", pricedf)
  pricedf <- gsub("Vendu", "NA", pricedf)

  pricel <-as.vector(pricedf)

  citydf <- as.data.frame(city)
  citydf <- citydf[citydf$city !="", ]
  citydf <-gsub("\t|\n", "", citydf)
  cityl <- as.vector(citydf)
#i <- html_data %>% html_nodes("i.property-icon-surface"


                                
  tab8 <- data.frame(type, cityl, pricel, sqm2)


tab = rbind(tab1, tab2, tab3, tab4, tab5, tab6, tab7, tab8)

write.csv(tab, "c:\\temp\\immocub.csv")