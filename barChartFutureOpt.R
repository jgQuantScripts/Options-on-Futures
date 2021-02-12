require("httr");require("data.table");require("stringr");require("rvest")
require("dplyr");require("pbapply");require("RQuantLib");require("derivmkts")
require("quantmod");require("lubridate")
# *****************************************************************************
#                         Get Quotes + Contract Details
# *****************************************************************************
ticker = "ESH21"
symbol="ES"       # short Contract Code
barCharF = "MW"   # Weekly Code: Fri
barCharMW = "MI"  # Weekly Code: Mon & Wed
barChar = "T8"    # EOM Code
# function to get Future Chains/Quotes/tickers
getFutQuotes = function(ticker,symbol)
  {
  # get the list of contracts available
  # page url
  pg <- html_session(paste0("https://www.barchart.com/futures/quotes/",ticker,"/futures-prices"))
  # save page cookies
  cookies <- pg$response$cookies
  # Use a named character vector for unquote splicing with !!!
  token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                             cookies$name)))
  # get data by passing in url and cookies
  pg <- 
    pg %>% rvest:::request_GET(
      paste0("https://www.barchart.com/proxies/core-api/v1/quotes/get?fields=",
             "symbol%2CcontractSymbol%2ClastPrice%2CpriceChange%2CopenPrice%2",
             "ChighPrice%2ClowPrice%2CpreviousPrice%2Cvolume%2CopenInterest%2",
             "CtradeTime%2CsymbolCode%2CsymbolType%2ChasOptions&list=futures.",
             "contractInRoot&root=",symbol,"&meta=field.shortName%2Cfield.type%2Cfield.",
             "description&hasOptions=true&page=1&limit=100&raw=1")
      ,
      config = httr::add_headers(`x-xsrf-token` = token)
    )
  
  # raw data
  data_raw <- httr::content(pg$response)
  
  # convert into a data table
  futs = lapply(as.list(1:length(data_raw$data)), function(ii){
    as.data.frame(do.call(cbind,data_raw$data[[ii]]$raw))
  })
  futs = as.data.frame(rbindlist(futs,use.names = TRUE,fill = TRUE))
  futs$lastPrice = sapply(futs$lastPrice, as.numeric)
  futs$priceChange = sapply(futs$priceChange, as.numeric)
  futs$openPrice = sapply(futs$openPrice, as.numeric)
  futs$highPrice = sapply(futs$highPrice, as.numeric)
  futs$lowPrice = sapply(futs$lowPrice, as.numeric)
  futs$previousPrice = sapply(futs$previousPrice, as.numeric)
  futs$openInterest = sapply(futs$openInterest, as.numeric)
  futs$volume = sapply(futs$volume, as.numeric)
  futs$tradeTime = as.POSIXct(as.numeric(futs$tradeTime),origin="1970-01-01")
  futs$pctChange = round(futs$lastPrice/futs$previousPrice-1,4)
  # asssign Quotes
  assign("futs",futs,envir = .GlobalEnv)
  # extract futures contracts
  futNames = futs$symbol
  # exclude cash futures
  futNames = futNames[!str_detect(futNames,"00")]
  # return Future Quotes
  futNames
}
# get Future Chains/Quotes
futNames = getFutQuotes(ticker=ticker,symbol = symbol)
# get contract Profile | ticker = 'ESH21'
getProfile = function(ticker)
{
  # url to Profile
  url0 = paste0("https://www.barchart.com/futures/quotes/",ticker,"/profile")
  pg = read_html(url0)
  # get Profile
  pg %>% html_node("table") %>% html_table() %>% as.data.frame()
}
profile = getProfile(ticker=ticker)
# *****************************************************************************
#                         Quarterly Options
# *****************************************************************************
# Quarterly Future Codes Table
getExpiry = function(ticker)
{
  # extract letter from ticker
  letter = str_sub(ticker,-3,-3)
  YR     = str_sub(ticker,-2,-1)
  # JANUARY
  if(letter == 'F')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-01-01")),
                   as.Date(paste0("20",YR,"-01-31")), by = 1)
  }
  # FEBRUARY
  if(letter == 'G')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-02-01")),
                   as.Date(paste0("20",YR,"-02-28")), by = 1)
  }
  # MARCH
  if(letter == 'H')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-03-01")),
                   as.Date(paste0("20",YR,"-03-31")), by = 1)
  }
  # APRIL
  if(letter == 'J')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-04-01")),
                   as.Date(paste0("20",YR,"-04-30")), by = 1)
  }
  # MAY
  if(letter == 'K')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-05-01")),
                   as.Date(paste0("20",YR,"-05-31")), by = 1)
  }
  # JUNE
  if(letter == 'M')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-06-01")),
                   as.Date(paste0("20",YR,"-06-30")), by = 1)
  }
  # JULY
  if(letter == 'N')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-07-01")),
                   as.Date(paste0("20",YR,"-07-31")), by = 1)
  }
  # AUGUST
  if(letter == 'Q')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-08-01")),
                   as.Date(paste0("20",YR,"-08-31")), by = 1)
  }
  # SEPTEMBER
  if(letter == 'U')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-09-01")),
                   as.Date(paste0("20",YR,"-09-30")), by = 1)
  }
  # OCTOBER
  if(letter == 'V')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-10-01")),
                   as.Date(paste0("20",YR,"-10-31")), by = 1)
  }
  # NOVEMBER
  if(letter == 'X')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-11-01")),
                   as.Date(paste0("20",YR,"-11-30")), by = 1)
  }
  # DECEMBER
  if(letter == 'Z')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-12-01")),
                   as.Date(paste0("20",YR,"-12-31")), by = 1)
  }
  # empty XTS 
  naXTS = xts(rep(NA,length(DTS)), order.by = DTS)
  # get expiration date
  DTS[options.expiry(naXTS)]
}
# Pull Quarterly Option + calculate greeks
getFutQuartelyOptions = function(futNames, EXERCISE)
  {
  # get Monthly options
  optMonthly = pblapply(as.list(futNames), function(ticker){
    # sleep 10 seconds to avoid ip address block
    Sys.sleep(10)
    # page url
    pg <- html_session(paste0("https://www.barchart.com/futures/quotes/",ticker,
                              "/options?moneyness=allRows&futuresOptionsView=split"))
    # save page cookies
    cookies <- pg$response$cookies
    # Use a named character vector for unquote splicing with !!!
    token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                               cookies$name)))
    # get data by passing in url and cookies
    pg <- 
      pg %>% rvest:::request_GET(
        paste0("https://www.barchart.com/proxies/core-api/v1/quotes/get?symbol=",ticker,
               "&list=futures.options&fields=strike%2ChighPrice%2ClowPrice%2ClastPrice%2",
               "CpriceChange%2CbidPrice%2CaskPrice%2Cvolume%2CopenInterest%2Cpremium%2C",
               "tradeTime%2ClongSymbol%2CsymbolCode%2CsymbolType%2ChasOptions&meta=",
               "field.shortName%2Cfield.description%2Cfield.shortName%2Cfield.type&",
               "hasOptions=true&raw=1")
        ,
        config = httr::add_headers(`x-xsrf-token` = token)
      )
    
    # raw data
    data_raw <- httr::content(pg$response)
    if(length(data_raw$data) == 0) {
      data = NULL
      cat(paste0("\nNo Option Data for: ", ticker,"\n"))
      }
    if(length(data_raw$data) != 0)
      {
      # convert into a data table
      data = lapply(as.list(1:length(data_raw$data)), function(ii){
        df = data_raw$data[[ii]]$raw
        df = sapply(df, function(x) ifelse(is.null(x), NA, x))
        df = as.data.frame(t(df))
      })
      
      data = as.data.frame(rbindlist(data,use.names = TRUE,fill = TRUE))
      
      data$strike = sapply(data$strike, as.numeric)
      data$highPrice = sapply(data$highPrice, as.numeric)
      data$lowPrice = sapply(data$lowPrice, as.numeric)
      data$lastPrice = sapply(data$lastPrice, as.numeric)
      data$priceChange = sapply(data$priceChange, as.numeric)
      data$bidPrice = sapply(data$bidPrice, as.numeric)
      data$askPrice = sapply(data$askPrice, as.numeric)
      data$premium = sapply(data$premium, as.numeric)
      data$openInterest = sapply(data$openInterest, as.numeric)
      data$volume = sapply(data$volume, as.numeric)
      data$tradeTime = as.POSIXct(as.numeric(data$tradeTime),origin="1970-01-01")
      
      futData = futs[which(futs$symbol == ticker),]
      # adding future contract specifics
      data$underlyingSymbol    = futData$symbol
      data$underlyingClose     = futData$lastPrice
      data$underlyingChange    = futData$priceChange
      data$underlyingOpen      = futData$openPrice
      data$underlyingHigh      = futData$highPrice
      data$underlyingLow       = futData$lowPrice
      data$underlyingPrevClose = futData$previousPrice
      data$underlyingTradeTime = futData$tradeTime
      data$underlyingVolume    = futData$volume
      data$underlyingOI        = futData$openInterest
      data$underlyingPctChange = futData$pctChange
      # Generate Expiration date
      data$expirationDate = getExpiry(ticker=ticker)
      # Column for Date Pulled
      data$accessDate = as.character(Sys.Date())
      # Days to expiration
      data$days2Exp = as.numeric(as.Date(data$expirationDate) - as.Date(data$accessDate))
      # Extract Flag
      data$Flag = str_sub(data$longSymbol,-1,-1)
      
      # calculate Implied Volatility
      # calculate IV
      if(EXERCISE == "european")
      {
        ivs = pblapply(as.list(1:nrow(data)),function(ii){
          tmp = try(EuropeanOptionImpliedVolatility(
            type = ifelse(data$Flag[ii] == "C","call","put"), 
            value=as.numeric(data$lastPrice)[ii],
            underlying=as.numeric(data$underlyingClose)[ii], 
            strike=as.numeric(data$strike)[ii], 
            dividendYield=0, 
            riskFreeRate=0,
            maturity=as.numeric(yearFraction(as.Date(data$accessDate[ii]),
                                             as.Date(data$expirationDate[ii]),
                                             1)), 
            volatility=as.numeric(0.1700)),
            silent=TRUE)
          if(inherits(tmp,'try-error')){
            iv = round(as.numeric(0.1700),4)
          }else{
            iv = round(tmp[[1]],4)
          }
          iv
        })
        
      }else{
        ivs = pblapply(as.list(1:nrow(data)),function(ii){
          tmp = try(AmericanOptionImpliedVolatility(
            type = ifelse(data$Flag[ii] == "C","call","put"), 
            value=as.numeric(data$lastPrice)[ii],
            underlying=as.numeric(data$underlyingClose)[ii], 
            strike=as.numeric(data$strike)[ii], 
            dividendYield=0, 
            riskFreeRate=0,
            maturity=as.numeric(yearFraction(as.Date(data$accessDate[ii]),
                                             as.Date(data$expirationDate[ii]),
                                             1)), 
            volatility=as.numeric(0.1700)),
            silent=TRUE)
          if(inherits(tmp,'try-error')){
            iv = round(as.numeric(0.1700),4)
          }else{
            iv = round(tmp[[1]],4)
          }
          iv
        })
      }
      # add Caluclated IVs to Options Date
      data$calc_IV = do.call(rbind,ivs)  
      
      # calculate greeks
      CALLS = subset(data, data$Flag == "C")
      PUTS = subset(data, data$Flag == "P")
      
      # greeks for calls
      cGREEKS = greeks2(bscall,list(s=as.numeric(CALLS$underlyingClose),
                                    k=as.numeric(CALLS$strike),
                                    v=as.numeric(CALLS$calc_IV),
                                    r=rep(0,nrow(CALLS)),
                                    tt=as.numeric(CALLS$days2Exp)/252,
                                    d=rep(0,nrow(CALLS))))  
      # transpose greeks
      cGREEKS = round(t(cGREEKS),5)
      # combine with call options
      CALLS = cbind(CALLS,cGREEKS)
      
      # greeks for calls
      pGREEKS = greeks2(bsput,list(s=as.numeric(PUTS$underlyingClose),
                                   k=as.numeric(PUTS$strike),
                                   v=as.numeric(PUTS$calc_IV),
                                   r=rep(0,nrow(PUTS)),
                                   tt=as.numeric(PUTS$days2Exp)/252,
                                   d=rep(0,nrow(PUTS))))  
      # transpose greeks
      pGREEKS = round(t(pGREEKS),5)
      # combine with call options
      PUTS = cbind(PUTS,pGREEKS)
      # combine calls/puts
      data = rbind(CALLS,PUTS)
    }
    # return full data set
    data
  })
  # exclude NULL tables
  optMonthly = optMonthly[lapply(optMonthly,length)>0]
  # rbind all option data
  opts = rbindlist(optMonthly, use.names = TRUE, fill = TRUE)
  # remove unecessary columns
  opts = opts[,-c("symbolCode","symbolType","hasOptions")]
  # return all options
  as.data.frame(opts)
}
# Exercise Type : S&P 500 : only Weekly and EOM options are European
# https://www.cmegroup.com/trading/equity-index/weekly-eom-options-faq.html
EXERCISE = "american"
quOpts = getFutQuartelyOptions(futNames,EXERCISE = "american")
# *****************************************************************************
#                         End-of-Month (EOM) Options
# *****************************************************************************
# Monthly Future Codes Table | Month - 2 - Futures Code
getMoCode = function(DATE)
{
  MO = format(DATE,"%m")
  # JANUARY
  if(MO == '01')
  {
    DTS = 'F'
  }
  # FEBRUARY
  if(MO == '02')
  {
    DTS = 'G'
  }
  # MARCH
  if(MO == '03')
  {
    DTS = 'H'
  }
  # APRIL
  if(MO == '04')
  {
    DTS = 'J'
  }
  # MAY
  if(MO == '05')
  {
    DTS = 'K'
  }
  # JUNE
  if(MO == '06')
  {
    DTS = 'M'
  }
  # JULY
  if(MO == '07')
  {
    DTS = 'N'
  }
  # AUGUST
  if(MO == '08')
  {
    DTS = 'Q'
  }
  # SEPTEMBER
  if(MO == '09')
  {
    DTS = 'U'
  }
  # OCTOBER
  if(MO == '10')
  {
    DTS = 'V'
  }
  # NOVEMBER
  if(MO == '11')
  {
    DTS = 'X'
  }
  # DECEMBER
  if(MO == '12')
  {
    DTS = 'Z'
  }
  # return Monthly Code
  DTS
}
# get Table of Tickers for URLs
getTable4URL = function(profile,barChar)
{
  ROWS = which(profile[,1] == "Trading Months")
  MOS = profile[ROWS,2]
  # remove ( )
  MOS = gsub("\\(|\\)|\\,","",MOS)
  # split string based on N-char 
  MOS = do.call(c,str_split(MOS, " "))
  TRES = MOS[str_count(MOS) == 3]
  TMOS = MOS[str_count(MOS) == 1]
  # create seq of dates to compare with current date
  firSeq = as.Date(paste0(TRES,"-28-",format(Sys.Date(), "%Y")), format="%b-%d-%Y")
  # Keep Expirations > Current Date
  SEQ2KEEP = firSeq > Sys.Date()
  # subset future codes
  firSeq = firSeq[SEQ2KEEP]
  TMOS   = TMOS[SEQ2KEEP]
  # next years sequence
  secSeq = as.Date(paste0(TRES,"-28-",as.numeric(format(Sys.Date(), "%Y"))+1), format="%b-%d-%Y")
  # generate future table
  shrtSymb = profile[which(profile[,1] == "Barchart Symbol"),2]
  # this year | Months left
  tbl1 = as.data.frame(seq(as.numeric(format(Sys.Date(), "%m")), 12,1))
  colnames(tbl1) = "Month"
  # next year | Months left
  tbl2 = as.data.frame(seq(1, 12,1))
  colnames(tbl2) = "Month"
  # construct the long form contract name 
  LONG1 = paste0(shrtSymb,TMOS,format(Sys.Date(),"%y"))
  LONG2 = paste0(shrtSymb,TMOS,as.numeric(format(Sys.Date(),"%y"))+1)
  # NA column to fill in 
  tbl1$Contract = NA
  tbl2$Contract = NA
  for(ii in 1:length(firSeq))
    {
    # locates which month matches our table's SEQ | THIS YR
    POS1 = which(tbl1 == as.numeric(format(firSeq,"%m"))[ii])
    # assign long contract name | THIS YR
    tbl1$Contract[POS1] = LONG1[ii]
  }
  # fill in NAs from Last
  tbl1 = na.locf(tbl1,fromLast = TRUE)
  # Generate barchart's Monthly Code for link
  tbl1$barChar = NA
  for(ii in 1:nrow(tbl1)){
    # get Monthly Code
    MO = getMoCode(as.Date(paste0(tbl1$Month[ii],"-28-",format(Sys.Date(),"%Y")), 
                           format="%m-%d-%Y"))
    tbl1$barChar[ii] =  paste0(barChar,MO,format(Sys.Date(),"%y"))
  }
  for(ii in 1:length(secSeq))
  {
    # locates which month matches our table's SEQ | NEXT YEAR
    POS2 = which(tbl2 == as.numeric(format(secSeq,"%m"))[ii])
    # assign long contract name | NEXT YR
    tbl2$Contract[POS2] = LONG2[ii]
  }
  # fill in NAs from Last
  tbl2 = na.locf(tbl2,fromLast = TRUE)
  # Generate barchart's Monthly Code for link
  tbl2$barChar = NA
  for(ii in 1:nrow(tbl2)){
    # get Monthly Code
    MO = getMoCode(as.Date(paste0(tbl2$Month[ii],"-28-",as.numeric(format(Sys.Date(),"%Y"))+1), 
                           format="%m-%d-%Y"))
    tbl2$barChar[ii] =  paste0(barChar,MO,as.numeric(format(Sys.Date(),"%y"))+1)
  }
  # combine this year + next year
  rbind(tbl1,tbl2)
}
urlFUTS = getTable4URL(profile=profile,barChar=barChar)
# gets Monthly expiration
getMonthlyExpiry = function(barChar)
{
  # extract letter from ticker
  letter = str_sub(barChar,-3,-3)
  YR     = str_sub(barChar,-2,-1)
  # JANUARY
  if(letter == 'F')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-01-01")),
                   as.Date(paste0("20",YR,"-01-31")), by = 1)
  }
  # FEBRUARY
  if(letter == 'G')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-02-01")),
                   as.Date(paste0("20",YR,"-02-28")), by = 1)
  }
  # MARCH
  if(letter == 'H')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-03-01")),
                   as.Date(paste0("20",YR,"-03-31")), by = 1)
  }
  # APRIL
  if(letter == 'J')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-04-01")),
                   as.Date(paste0("20",YR,"-04-30")), by = 1)
  }
  # MAY
  if(letter == 'K')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-05-01")),
                   as.Date(paste0("20",YR,"-05-31")), by = 1)
  }
  # JUNE
  if(letter == 'M')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-06-01")),
                   as.Date(paste0("20",YR,"-06-30")), by = 1)
  }
  # JULY
  if(letter == 'N')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-07-01")),
                   as.Date(paste0("20",YR,"-07-31")), by = 1)
  }
  # AUGUST
  if(letter == 'Q')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-08-01")),
                   as.Date(paste0("20",YR,"-08-31")), by = 1)
  }
  # SEPTEMBER
  if(letter == 'U')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-09-01")),
                   as.Date(paste0("20",YR,"-09-30")), by = 1)
  }
  # OCTOBER
  if(letter == 'V')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-10-01")),
                   as.Date(paste0("20",YR,"-10-31")), by = 1)
  }
  # NOVEMBER
  if(letter == 'X')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-11-01")),
                   as.Date(paste0("20",YR,"-11-30")), by = 1)
  }
  # DECEMBER
  if(letter == 'Z')
  {
    DTS = seq.Date(as.Date(paste0("20",YR,"-12-01")),
                   as.Date(paste0("20",YR,"-12-31")), by = 1)
  }
  # get expiration date | last Friday of the Month
  DTS[last(which(weekdays(DTS) == 'Friday'))]
}

# EXERCISE = "european"
# Pull Monthly Options + calculate greeks
getFutMonthlyOptions = function(urlFUTS,EXERCISE)
{
  # get Monthly options
  optMonthly = pblapply(as.list(1:nrow(urlFUTS)), function(ii){
    # sleep 10 seconds to avoid ip address block
    Sys.sleep(10)
    # page url
    pg <- html_session(paste0("https://www.barchart.com/futures/quotes/",
                              urlFUTS$Contract[ii],
                              "/options/",urlFUTS$barChar[ii],
                              "?moneyness=allRows&futuresOptionsView=split"))
    # save page cookies
    cookies <- pg$response$cookies
    # Use a named character vector for unquote splicing with !!!
    token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                               cookies$name)))
    # get data by passing in url and cookies
    pg <- 
      pg %>% rvest:::request_GET(
        paste0("https://www.barchart.com/proxies/core-api/v1/quotes/get?symbol=",urlFUTS$barChar[ii],
               "&list=futures.options&fields=strike%2ChighPrice%2ClowPrice%2ClastPrice",
               "%2CpriceChange%2CbidPrice%2CaskPrice%2Cvolume%2CopenInterest%2Cpremium",
               "%2CtradeTime%2ClongSymbol%2CsymbolCode%2CsymbolType%2ChasOptions&meta=",
               "field.shortName%2Cfield.description%2Cfield.shortName%2Cfield.type&",
               "hasOptions=true&raw=1")
        ,
        config = httr::add_headers(`x-xsrf-token` = token)
      )
    
    # raw data
    data_raw <- httr::content(pg$response)
    if(length(data_raw$data) == 0) {
      data = NULL
      cat(paste0("\nNo Option Data for: ", urlFUTS$barChar[ii],"\n"))
    }
    if(length(data_raw$data) != 0)
    {
      # convert into a data table
      data = lapply(as.list(1:length(data_raw$data)), function(ii){
        #as.data.frame(do.call(cbind,data_raw$data[[ii]]$raw))
        df = data_raw$data[[ii]]$raw
        df = sapply(df, function(x) ifelse(is.null(x), NA, x))
        df = as.data.frame(t(df))
      })
      
      data = as.data.frame(rbindlist(data,use.names = TRUE,fill = TRUE))
      
      data$strike = sapply(data$strike, as.numeric)
      data$highPrice = sapply(data$highPrice, as.numeric)
      data$lowPrice = sapply(data$lowPrice, as.numeric)
      data$lastPrice = sapply(data$lastPrice, as.numeric)
      data$priceChange = sapply(data$priceChange, as.numeric)
      data$bidPrice = sapply(data$bidPrice, as.numeric)
      data$askPrice = sapply(data$askPrice, as.numeric)
      data$premium = sapply(data$premium, as.numeric)
      data$openInterest = sapply(data$openInterest, as.numeric)
      data$volume = sapply(data$volume, as.numeric)
      data$tradeTime = as.POSIXct(as.numeric(data$tradeTime),origin="1970-01-01")
      
      ticker = urlFUTS$Contract[ii]
      futData = futs[which(futs$symbol == ticker),]
      # adding future contract specifics
      data$underlyingSymbol    = futData$symbol
      data$underlyingClose     = futData$lastPrice
      data$underlyingChange    = futData$priceChange
      data$underlyingOpen      = futData$openPrice
      data$underlyingHigh      = futData$highPrice
      data$underlyingLow       = futData$lowPrice
      data$underlyingPrevClose = futData$previousPrice
      data$underlyingTradeTime = futData$tradeTime
      data$underlyingVolume    = futData$volume
      data$underlyingOI        = futData$openInterest
      data$underlyingPctChange = futData$pctChange
      # Generate Expiration date
      data$expirationDate = getMonthlyExpiry(barChar = urlFUTS$barChar[ii])
      # Column for Date Pulled
      data$accessDate = as.character(Sys.Date())
      # Days to expiration
      data$days2Exp = as.numeric(as.Date(data$expirationDate) - as.Date(data$accessDate))
      # Extract Flag
      data$Flag = str_sub(data$longSymbol,-1,-1)
      
      # calculate Implied Volatility
      # calculate IV
      if(EXERCISE == "european")
      {
        ivs = pblapply(as.list(1:nrow(data)),function(ii){
          tmp = try(EuropeanOptionImpliedVolatility(
            type = ifelse(data$Flag[ii] == "C","call","put"), 
            value=as.numeric(data$lastPrice)[ii],
            underlying=as.numeric(data$underlyingClose)[ii], 
            strike=as.numeric(data$strike)[ii], 
            dividendYield=0, 
            riskFreeRate=0,
            maturity=as.numeric(yearFraction(as.Date(data$accessDate[ii]),
                                             as.Date(data$expirationDate[ii]),
                                             1)), 
            volatility=as.numeric(0.1700)),
            silent=TRUE)
          if(inherits(tmp,'try-error')){
            iv = round(as.numeric(0.1700),4)
          }else{
            iv = round(tmp[[1]],4)
          }
          iv
        })
        
      }else{
        ivs = pblapply(as.list(1:nrow(data)),function(ii){
          tmp = try(AmericanOptionImpliedVolatility(
            type = ifelse(data$Flag[ii] == "C","call","put"), 
            value=as.numeric(data$lastPrice)[ii],
            underlying=as.numeric(data$underlyingClose)[ii], 
            strike=as.numeric(data$strike)[ii], 
            dividendYield=0, 
            riskFreeRate=0,
            maturity=as.numeric(yearFraction(as.Date(data$accessDate[ii]),
                                             as.Date(data$expirationDate[ii]),
                                             1)), 
            volatility=as.numeric(0.1700)),
            silent=TRUE)
          if(inherits(tmp,'try-error')){
            iv = round(as.numeric(0.1700),4)
          }else{
            iv = round(tmp[[1]],4)
          }
          iv
        })
      }
      # add Caluclated IVs to Options Date
      data$calc_IV = do.call(rbind,ivs)  
      
      # calculate greeks
      CALLS = subset(data, data$Flag == "C")
      PUTS = subset(data, data$Flag == "P")
      
      # greeks for calls
      cGREEKS = greeks2(bscall,list(s=as.numeric(CALLS$underlyingClose),
                                    k=as.numeric(CALLS$strike),
                                    v=as.numeric(CALLS$calc_IV),
                                    r=rep(0,nrow(CALLS)),
                                    tt=as.numeric(CALLS$days2Exp)/252,
                                    d=rep(0,nrow(CALLS))))  
      # transpose greeks
      cGREEKS = round(t(cGREEKS),5)
      # combine with call options
      CALLS = cbind(CALLS,cGREEKS)
      
      # greeks for calls
      pGREEKS = greeks2(bsput,list(s=as.numeric(PUTS$underlyingClose),
                                   k=as.numeric(PUTS$strike),
                                   v=as.numeric(PUTS$calc_IV),
                                   r=rep(0,nrow(PUTS)),
                                   tt=as.numeric(PUTS$days2Exp)/252,
                                   d=rep(0,nrow(PUTS))))  
      # transpose greeks
      pGREEKS = round(t(pGREEKS),5)
      # combine with call options
      PUTS = cbind(PUTS,pGREEKS)
      # combine calls/puts
      data = rbind(CALLS,PUTS)
    }
    # return full data set
    data
  })
  # exclude NULL tables
  optMonthly = optMonthly[lapply(optMonthly,length)>0]
  # rbind all option data
  opts = rbindlist(optMonthly, use.names = TRUE, fill = TRUE)
  # remove unecessary columns
  opts = opts[,-c("symbolCode","symbolType","hasOptions")]
  # return all options
  as.data.frame(opts)
}
# Exercise Type : S&P 500 : only Weekly and EOM options are European
# https://www.cmegroup.com/trading/equity-index/weekly-eom-options-faq.html
moOpts = getFutMonthlyOptions(urlFUTS,EXERCISE = "european")
# *****************************************************************************
#                         Weekly Options - Monday,Wednesday,Friday
# *****************************************************************************
# Monthly Future Codes Table | Month - 2 - Futures Code
getMoCode = function(DATE)
{
  MO = format(DATE,"%m")
  # JANUARY
  if(MO == '01')
  {
    DTS = 'F'
  }
  # FEBRUARY
  if(MO == '02')
  {
    DTS = 'G'
  }
  # MARCH
  if(MO == '03')
  {
    DTS = 'H'
  }
  # APRIL
  if(MO == '04')
  {
    DTS = 'J'
  }
  # MAY
  if(MO == '05')
  {
    DTS = 'K'
  }
  # JUNE
  if(MO == '06')
  {
    DTS = 'M'
  }
  # JULY
  if(MO == '07')
  {
    DTS = 'N'
  }
  # AUGUST
  if(MO == '08')
  {
    DTS = 'Q'
  }
  # SEPTEMBER
  if(MO == '09')
  {
    DTS = 'U'
  }
  # OCTOBER
  if(MO == '10')
  {
    DTS = 'V'
  }
  # NOVEMBER
  if(MO == '11')
  {
    DTS = 'X'
  }
  # DECEMBER
  if(MO == '12')
  {
    DTS = 'Z'
  }
  # return Monthly Code
  DTS
}
# generate link codes | Letter codes from URL link
getWeeklyExpiry = function(symbol,barCharF,barCharMW,profile)
{
  # sequence of days to generate Years - 4-Months
  toYRS = seq.Date(Sys.Date(), Sys.Date()+days(124),1)
  # Monday, Wednesday, Friday Options: MWF
  MWF = toYRS[weekdays(toYRS,abbreviate = TRUE) %in% c("Mon","Wed","Fri")]
  # Mondays 
  Mon = na.omit(MWF[weekdays(MWF,abbreviate = TRUE) %in% "Mon"])
  Wed = na.omit(MWF[weekdays(MWF,abbreviate = TRUE) %in% "Wed" ])
  Fri = na.omit(MWF[weekdays(MWF,abbreviate = TRUE) %in% "Fri"])
  
  # current codes for E-mini
  # Fri | MW1H21  MW2H21  MW4H21  MW3J21 MW3K21
  # Wed | MI3G21  MI4G21  MI1H21  
  # Mon | MI8G21  MI9G21  MI6H21  MI7H21
  
  # generate Weekly barChart Codes
  # calculate the week number of the month
  MON = lapply(as.list(1:length(Mon)), function(ii){
        paste0(barCharMW,                      # barChart ticker
               ceiling(day(Mon[ii])/7)+5,      # Week of the Mo
               getMoCode(Mon[ii]),             # Monthly Code
               format(Sys.Date(), format="%y"))# Year
  })
  WED = lapply(as.list(1:length(Wed)), function(ii){
    paste0(barCharMW,                       # barChart ticker
           ceiling(day(Wed[ii])/7) ,        # Week of the Mo
           getMoCode(Wed[ii]),              # Monthly Code
           format(Sys.Date(), format="%y")) # Year
  })
  FRI = lapply(as.list(1:length(Fri)), function(ii){
    paste0(barCharF,                         # barChart ticker
           ceiling(day(Fri[ii])/7) ,         # Week of the Mo
           getMoCode(Fri[ii]),               # Monthly Code
           format(Sys.Date(), format="%y"))  # Year
  })
  # combine dates with codes
  FRI = as.data.frame(cbind(as.character(Fri),do.call(rbind,FRI)))
  colnames(FRI) = c("Dates","Codes")
  WED = as.data.frame(cbind(as.character(Wed),do.call(rbind,WED)))
  colnames(WED) = c("Dates","Codes")
  MON = as.data.frame(cbind(as.character(Mon),do.call(rbind,MON)))
  colnames(MON) = c("Dates","Codes")
  # from profile
  ROWS = which(profile[,1] == "Trading Months")
  MOS = profile[ROWS,2]
  # remove ( )
  MOS = gsub("\\(|\\)|\\,","",MOS)
  # split string based on N-char 
  MOS = do.call(c,str_split(MOS, " "))
  TRES = MOS[str_count(MOS) == 3]
  TMOS = MOS[str_count(MOS) == 1]
  
  # location of months that match contract months
  locF = which(format(as.Date(FRI$Dates), "%b") %in% TRES)
  locW = which(format(as.Date(WED$Dates), "%b") %in% TRES)
  locM = which(format(as.Date(MON$Dates), "%b") %in% TRES)
  
  # create new column
  FRI$Contract = NA
  WED$Contract = NA
  MON$Contract = NA
  
  for(ii in 1:length(locF)){
    k = format(as.Date(FRI$Dates)[locF[ii]], "%b")
   FRI$Contract[locF[ii]] = paste0(symbol,TMOS[which(TRES %in% k)],format(Sys.Date(),"%y"))
  }
  for(ii in 1:length(locW)){
    k = format(as.Date(WED$Dates)[locW[ii]], "%b")
    WED$Contract[locW[ii]] = paste0(symbol,TMOS[which(TRES %in% k)],format(Sys.Date(),"%y"))
  }
  for(ii in 1:length(locM)){
    k = format(as.Date(MON$Dates)[locM[ii]], "%b")
    MON$Contract[locM[ii]] = paste0(symbol,TMOS[which(TRES %in% k)],format(Sys.Date(),"%y"))
  }
  # na fill - backwards
  FRI = na.locf(FRI,fromLast = TRUE)
  WED = na.locf(WED,fromLast = TRUE)
  MON = na.locf(MON,fromLast = TRUE)
  # combine tables
  ALL = rbind(MON,WED,FRI)
  # order by date
  ALL[order(as.Date(ALL$Dates), decreasing = FALSE),]
  # return table
  ALL
}

if(exists("barCharF") & exists("barCharMW"))
{
urlFUTS = getWeeklyExpiry(symbol = symbol,barCharF = barCharF,
                          barCharMW = barCharMW,profile = profile)
}else{
urlFUTS = getWeeklyExpiry(symbol = symbol,barCharF = barCharF,
                          barCharMW = barCharF,profile = profile)
# only extract Friday Options
urlFUTS = urlFUTS[which(weekdays(as.Date(urlFUTS$Dates),abbreviate = TRUE) == "Fri"),]
}
# Pull Weekly Options + calculate greeks
getFutWeeklyFridayOptions = function(urlFUTS,EXERCISE)
{
  # get Weekly options
  optWeekly = pblapply(as.list(1:nrow(urlFUTS)), function(ii){
    # sleep 10 seconds to avoid ip address block
    Sys.sleep(10)
    # page url
    pg <- html_session(paste0("https://www.barchart.com/futures/quotes/",
                              urlFUTS$Contract[ii],
                              "/options/",urlFUTS$Codes[ii],
                              "?moneyness=allRows&futuresOptionsView=split"))
    # save page cookies
    cookies <- pg$response$cookies
    # Use a named character vector for unquote splicing with !!!
    token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                               cookies$name)))
    # get data by passing in url and cookies
    pg <- 
      pg %>% rvest:::request_GET(
        paste0("https://www.barchart.com/proxies/core-api/v1/quotes/get?symbol=",urlFUTS$Codes[ii],
               "&list=futures.options&fields=strike%2ChighPrice%2ClowPrice%2ClastPrice",
               "%2CpriceChange%2CbidPrice%2CaskPrice%2Cvolume%2CopenInterest%2Cpremium",
               "%2CtradeTime%2ClongSymbol%2CsymbolCode%2CsymbolType%2ChasOptions&meta=",
               "field.shortName%2Cfield.description%2Cfield.shortName%2Cfield.type&",
               "hasOptions=true&raw=1")
        ,
        config = httr::add_headers(`x-xsrf-token` = token)
      )
    
    # raw data
    data_raw <- httr::content(pg$response)
    if(length(data_raw$data) == 0) {
      data = NULL
      cat(paste0("\nNo Option Data for: ", 
                 urlFUTS$Codes[ii]," | ",
                 urlFUTS$Contract[ii]," | ",
                 urlFUTS$Dates[ii],"\n"))
    }
    if(length(data_raw$data) != 0)
    {
      # convert into a data table
      data = lapply(as.list(1:length(data_raw$data)), function(ii){
        as.data.frame(do.call(cbind,data_raw$data[[ii]]$raw))
      })
      
      data = as.data.frame(rbindlist(data,use.names = TRUE,fill = TRUE))
      
      data$strike = sapply(data$strike, as.numeric)
      data$highPrice = sapply(data$highPrice, as.numeric)
      data$lowPrice = sapply(data$lowPrice, as.numeric)
      data$lastPrice = sapply(data$lastPrice, as.numeric)
      #data$priceChange = sapply(data$priceChange, as.numeric)
      data$bidPrice = sapply(data$bidPrice, as.numeric)
      data$askPrice = sapply(data$askPrice, as.numeric)
      data$premium = sapply(data$premium, as.numeric)
      data$openInterest = sapply(data$openInterest, as.numeric)
      data$volume = sapply(data$volume, as.numeric)
      data$tradeTime = as.POSIXct(as.numeric(data$tradeTime),origin="1970-01-01")
      
      ticker = urlFUTS$Contract[ii]
      futData = futs[which(futs$symbol == ticker),]
      # adding future contract specifics
      data$underlyingSymbol    = futData$symbol
      data$underlyingClose     = futData$lastPrice
      data$underlyingChange    = futData$priceChange
      data$underlyingOpen      = futData$openPrice
      data$underlyingHigh      = futData$highPrice
      data$underlyingLow       = futData$lowPrice
      data$underlyingPrevClose = futData$previousPrice
      data$underlyingTradeTime = futData$tradeTime
      data$underlyingVolume    = futData$volume
      data$underlyingOI        = futData$openInterest
      data$underlyingPctChange = futData$pctChange
      # Generate Expiration date
      data$expirationDate = urlFUTS$Dates[ii]
      # Column for Date Pulled
      data$accessDate = as.character(Sys.Date())
      # Days to expiration
      data$days2Exp = as.numeric(as.Date(data$expirationDate) - as.Date(data$accessDate))
      # Extract Flag
      data$Flag = str_sub(data$longSymbol,-1,-1)
      
      # calculate Implied Volatility
      # calculate IV
      if(EXERCISE == "european")
      {
        ivs = pblapply(as.list(1:nrow(data)),function(ii){
          tmp = try(EuropeanOptionImpliedVolatility(
            type = ifelse(data$Flag[ii] == "C","call","put"), 
            value=as.numeric(data$lastPrice)[ii],
            underlying=as.numeric(data$underlyingClose)[ii], 
            strike=as.numeric(data$strike)[ii], 
            dividendYield=0, 
            riskFreeRate=0,
            maturity=as.numeric(yearFraction(as.Date(data$accessDate[ii]),
                                             as.Date(data$expirationDate[ii]),
                                             1)), 
            volatility=as.numeric(0.1700)),
            silent=TRUE)
          if(inherits(tmp,'try-error')){
            iv = round(as.numeric(0.1700),4)
          }else{
            iv = round(tmp[[1]],4)
          }
          iv
        })
        
      }else{
        ivs = pblapply(as.list(1:nrow(data)),function(ii){
          tmp = try(AmericanOptionImpliedVolatility(
            type = ifelse(data$Flag[ii] == "C","call","put"), 
            value=as.numeric(data$lastPrice)[ii],
            underlying=as.numeric(data$underlyingClose)[ii], 
            strike=as.numeric(data$strike)[ii], 
            dividendYield=0, 
            riskFreeRate=0,
            maturity=as.numeric(yearFraction(as.Date(data$accessDate[ii]),
                                             as.Date(data$expirationDate[ii]),
                                             1)), 
            volatility=as.numeric(0.1700)),
            silent=TRUE)
          if(inherits(tmp,'try-error')){
            iv = round(as.numeric(0.1700),4)
          }else{
            iv = round(tmp[[1]],4)
          }
          iv
        })
      }
      # add Caluclated IVs to Options Date
      data$calc_IV = do.call(rbind,ivs)  
      
      # calculate greeks
      CALLS = subset(data, data$Flag == "C")
      PUTS = subset(data, data$Flag == "P")
      
      # greeks for calls
      cGREEKS = greeks2(bscall,list(s=as.numeric(CALLS$underlyingClose),
                                    k=as.numeric(CALLS$strike),
                                    v=as.numeric(CALLS$calc_IV),
                                    r=rep(0,nrow(CALLS)),
                                    tt=as.numeric(CALLS$days2Exp)/252,
                                    d=rep(0,nrow(CALLS))))  
      # transpose greeks
      cGREEKS = round(t(cGREEKS),5)
      # combine with call options
      CALLS = cbind(CALLS,cGREEKS)
      
      # greeks for calls
      pGREEKS = greeks2(bsput,list(s=as.numeric(PUTS$underlyingClose),
                                   k=as.numeric(PUTS$strike),
                                   v=as.numeric(PUTS$calc_IV),
                                   r=rep(0,nrow(PUTS)),
                                   tt=as.numeric(PUTS$days2Exp)/252,
                                   d=rep(0,nrow(PUTS))))  
      # transpose greeks
      pGREEKS = round(t(pGREEKS),5)
      # combine with call options
      PUTS = cbind(PUTS,pGREEKS)
      # combine calls/puts
      data = rbind(CALLS,PUTS)
    }
    # return full data set
    data
  })
  # exclude NULL tables
  optWeekly = optWeekly[lapply(optWeekly,length)>0]
  # rbind all option data
  opts = rbindlist(optWeekly, use.names = TRUE, fill = TRUE)
  # remove unecessary columns
  opts = opts[,-c("symbolCode","symbolType","hasOptions")]
  # return all options
  as.data.frame(opts)
}
# Exercise Type : S&P 500 : only Weekly and EOM options are European
# https://www.cmegroup.com/trading/equity-index/weekly-eom-options-faq.html
wkOpts = getFutWeeklyFridayOptions(urlFUTS,EXERCISE = "european")
# ************************************************************************************************
# ************************************************************************************************
# ************************************************************************************************
ALL = rbind(quOpts,moOpts,wkOpts)
saveRDS(ALL,paste0("/Volumes/3TB/FUTURES/OPTIONS/",symbol,"/",
                   format(Sys.Date(),format="%Y%m%d"),".rds"))


# Nasdaq 100 E-Mini Mar '21 (NQH21)
# symbol="NQ"       # short Contract Code
# barCharF = "MQ"   # Weekly Code: Fri
# barChar = "MQ6"   # EOM Code

# Dow Futures Mini Mar '21 (YMH21)
# symbol="YM"       # short Contract Code
# barCharF = ?      # Weekly Code: Fri
# barChar = "BYM"   # EOM Code

# Russell 2000 E-Mini Mar '21 (QRH21) | American Only
# symbol="QR"       # short Contract Code

# S&P Midcap E-Mini Mar '21 (EWH21)   | American Only
# symbol="EW"       # short Contract Code
