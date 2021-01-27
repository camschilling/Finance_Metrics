library(tidyverse)
library(rvest)

fcf_yield<- function(stock){
      url<-paste0("https://finance.yahoo.com/quote/", stock, "/key-statistics?p=", stock)
      
      dat <- url %>% 
            read_html() %>% 
            html_table() %>% 
            map_df(bind_cols) %>% 
            # Transpose
            t() %>%
            as_tibble()
      lr<-nrow(dat)
      
      to_num <- function(x){
            suf<-substr(x, nchar(x), nchar(x))
            if(suf == "T"){
                  c = 1000000000000
            }
            else if(suf == "B"){
                  c = 1000000000
            }
            else if(suf == "M"){
                  c = 1000000
            }
            n<-as.numeric(substr(x, 1, nchar(x)-1))
            c*n
      }
      
      mcap<-to_num(dat[[2,1]])
      fcf<-to_num(dat[[lr,59]])
      
      (fcf/mcap)*100
}