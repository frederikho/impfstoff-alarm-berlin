

### Libraries
library(httr)
library(jsonlite)
library(dplyr)
library(beepr)
library(purrr)
#library(tuneR)



counter = 0
while (1 == 1){
  if (counter == 0){
    statistics <- data.frame(matrix(ncol=5), stringsAsFactors= FALSE)
    colnames(statistics) <- c("time", "time_formatted","available", "termin", "impfzentrum_name")
  }
  counter = counter +1
  statistics %>% bind_rows(set_names(rep(NA, ncol(.)), colnames(.)))
  
  url <- "https://api.impfstoff.link/?v=0.3"
  res = GET(url, add_headers("robots = 1"))
  
  if (res$status_code == 200)
  {
    data <- as.data.frame(fromJSON(rawToChar(res$content))$stats)
    cat(paste0(Sys.time(), ": downloade Daten... "))
    
    if (TRUE %in% data$open){
      
      for (i in 1:nrow(data[data$open == TRUE, ])){ #checking all open Impfzentren
        row <- data[data$open == TRUE, ][i, ]
        data2 <- as.data.frame(unlist(row))
        colnames(data2) <- (c("val"))
        
        #checking all which day is open 
        for (i in 2:length(rownames(subset(data2, data2$val == data2['lastUpdate', ])))){
          day <- rownames(subset(data2, data2$val == data2['lastUpdate', ]))[i] %>% substr(., 15, 16)  
          month <- rownames(subset(data2, data2$val == data2['lastUpdate', ]))[i] %>% substr(., 12, 13)
          Impfzentrum_name <- data2['name', ]
          cat(paste0("Freier Termin am ", day, ".", month, ". im ", Impfzentrum_name, "! \n"))
          beep(1)
          statistics[counter, 'termin'] <- paste0(day, ".", month, ".")
          statistics[counter, "impfzentrum_name"] <- Impfzentrum_name
          if (day < 20){
            #beep(2)
          }
            
        }
        rm(row, data2)
      }
      statistics[counter, 'available'] <- TRUE
    } else{
      cat(paste0("Keine freien Termine verfügbar. \n"))
      statistics[counter, 'available'] <- FALSE
    }
    
    statistics$time[counter] <- Sys.time()
    statistics$time_formatted[counter] <- format(Sys.time(), format = "%F %R:%S") 
    write.csv2(statistics, file = "statistics.csv")
    
    
    Sys.sleep(4)      
    
  } else {
    print("Error, Fehlercode: ", res$status_code)
  }
  
}
