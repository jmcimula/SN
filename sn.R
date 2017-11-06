library(tidyverse)

data <- read.csv("~/social_network/social_network_test.csv")
data <- data %>%
  mutate( rpdate = lubridate::dmy(reportdate)) %>% 
  select(-reportdate)

#dplyr::distinct(data, rpdate, actor)
#test_a <- dplyr::filter(data, rpdate=="2017-01-11", actor=="C")
nb <- dplyr::distinct(data, actor) %>% as.data.frame()
df <- data.frame()
for(k in 1:nrow(nb)) {
  
  val <- as.character(nb$actor[k])
  test_a <- dplyr::filter(data,rpdate=="2017-01-11", actor==val)
  
  n <- nrow(test_a)
  
  for (i in 1:n){
    
    backward <- test_a$order_id[i-1]
    origin <- test_a$order_id[i]
    forward <- test_a$order_id[i+1]
    
    forw <- abs(origin - forward)
    back <- abs(origin - backward)
  
    if(origin!=1){
      
      if(is.na(forw)){
        forw <- "NA"
      }else if(identical(forw,integer(0))){
        forw_item <- data[data$order_id==data$order_id[origin +1], ]$actor
      }else{
        if (forw > 1){
          here <- origin + 1
          forw_item <- data[data$order_id==data$order_id[here], ]$actor
        }
      }
      
      if(identical(back, integer(0))){
        back_item <- data[data$order_id==data$order_id[origin -1], ]$actor
      }else{
        if(back > 1){
          here <- origin - 1
          back_item <- data[data$order_id==data$order_id[here], ]$actor
        }
        
      }
      sub_df <- data.frame(id=origin,actor = val,backward=back_item,forward=forw_item)
      df <- rbind(df,sub_df)
    }
  }
  
}


