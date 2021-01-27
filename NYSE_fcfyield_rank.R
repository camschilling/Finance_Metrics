#Program
source("fcf_yield.R")

#Settings
#Number of top stocks to evaluate (top market cappers)
n<-1000


#Stock Data (Update at https://www.nasdaq.com/market-activity/stocks/screener)
##cannot find direct download link off of nasdaq website, 
##it seems to be clipped using CSS's clip-path and using link  generated after download has not worked
stocks<-read.csv("Stockslist/stocks_210127.csv")
#Filter by market cap
stocks<-stocks %>%
      filter(!is.na(Last.Sale) & Market.Cap > 0) %>%
      arrange(desc(Market.Cap))


##Create empty data frame and pull FCF Yield for Each Stock Ticker
nr<-c(1:n)

df<-as.data.frame(NULL)

for(i in nr){
      stock<-paste0(nyse[[i,1]])
      firm<-paste0(nyse[[i,2]])
      sect<-paste0(nyse[[i,10]])
      ind<-paste0(nyse[[i,11]])
      dt<-Sys.Date()
      fcfy<-tryCatch(fcf_yield(stock)
               , error = function(e) { 
                     return(NA)
                     })
      r<-data.frame(stock,firm,sect,ind,fcfy,dt)
      df<-rbind(df, r)
   }

colnames(df)<- c("Stock", "Company", "Sector", "Industry", "FCF_Y", "Date")
df<-df[complete.cases(df),]

##Can import previous files and rbind them together to create master dataset for trending over time
##Do this later // run a script pulling random sample of stocks?

fname<-paste0("Tracking/mc_",n,".csv")
write.csv(df, fname, row.names = FALSE)

plot(df$FCF_Y)
