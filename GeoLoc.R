library(RCurl)

#install.packages("RCurl")
#install.packages("RJSONIO")

library(RJSONIO)
library(plyr)
library(httr)



#Sys.setenv(http_proxy="http://xxxx:8080")

#df <- data.frame("KeyWord"=character(), "Name"=character(),"Address"=character(),"lat"=character(),"lng"=character())


key_xxx= ""AIzaSyC.............."

url <- function(searchtext, return.call = "json", key = key_xxx) {
  root <- "https://maps.googleapis.com/maps/api/place/textsearch/json"
  u <- paste(root, "?query=", searchtext, "&key=", key, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE, returnAll = F) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  print(paste0("querying: ", address))
  curl <- getCurlHandle()
  curlSetOpt(.opts = list(proxy = 'xxxx', connecttimeout = 2000), curl = curl)
  doc <- getURL(u,curl=curl)
  #doc <- GET(u)
  df = NULL
  x <- fromJSON(doc,asText = TRUE)
  if(x$status!="OK")
  {
    print(x$status)
  }else if(x$status=="OK") {
    print(x$status)
    xl <- x$results
    for (elem in xl) {
      df1 <- data.frame("KeyWord"=address, "Name"=elem$name,"Address"=elem$formatted_address,"lat"=elem$geometry$location[1],"lng"=elem$geometry$location[2])
      df <-  rbind(df,df1)
    }
    
   if(returnAll){ return(df)} else {return (df[1,])}
    
  } 
}


# address1 <- geoCode("Shoppers Stop Bengaluru, Karnataka, India")



