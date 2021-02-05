# webscrap test
#by jyce3d 2020

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

process_page <-function(page) {
  url =sprintf('https://www.immocube.be/fr/component/properties/?view=list&page=%d&view=list&goal=0', page)
  print(url)
  html_data <- read_html(url)

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


                                
  return (data.frame(type, cityl, pricel, sqm2))
}
  for( i in 1:8) {
    if (i==1) {
        tab[[1]] <- process_page(1)
    }
    else {
      tab[[i]] <- rbind(tab[[i-1]], process_page(i))
    }
  }

write.csv(tab[8], "c:\\temp\\immocub.csv")