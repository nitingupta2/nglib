
getField <- function(fieldName, df, numSymbols) {
    if(fieldName %in% colnames(df)) {
        return(c(df[[fieldName]], rep(NA, numSymbols - nrow(df))))
    } else {
        return(NA)
    }
}

getTradeTime <- function(tradeTime, timeZone) {
    if(!is.na(tradeTime)) {
        return(as.POSIXct(tradeTime, tz = "America/New_York", origin = "1970-01-01") %>% as.character())
    } else {
        return(NA)
    }
}

getSnapshot <- function(vSymbols) {
    numSymbols <- length(vSymbols)
    dfSnapshot <- tibble(Symbol = str_replace_all(vSymbols, "\\-", "\\."),
                         Name = NA, TradeTime = NA, Last = NA, Change = NA, PriceLow = NA, PriceHigh = NA,
                         YearlyPriceLow = NA, YearlyPriceHigh = NA, MarketCap = NA, PE = NA, Yield = NA)
    tickerString <- paste(vSymbols, collapse = ",")
    queryURL <- sprintf("https://query1.finance.yahoo.com/v7/finance/quote?formatted=false&symbols=%s",tickerString)
    dfSnapshotTemp <- fromJSON(queryURL)$quoteResponse$result
    if(is.data.frame(dfSnapshotTemp) & nrow(dfSnapshotTemp) > 0) {
        dfSnapshot$Name <- getField("longName", dfSnapshotTemp, numSymbols)
        dfSnapshot$TradeTime <- getField("regularMarketTime", dfSnapshotTemp, numSymbols)
        dfSnapshot$Timezone <- getField("exchangeTimezoneShortName", dfSnapshotTemp, numSymbols)
        dfSnapshot$Last <- getField("regularMarketPrice", dfSnapshotTemp, numSymbols)
        dfSnapshot$Change <- getField("regularMarketChangePercent", dfSnapshotTemp, numSymbols)
        dfSnapshot$PriceLow <- getField("regularMarketDayLow", dfSnapshotTemp, numSymbols)
        dfSnapshot$PriceHigh <- getField("regularMarketDayHigh", dfSnapshotTemp, numSymbols)
        dfSnapshot$YearlyPriceLow <- getField("fiftyTwoWeekLow", dfSnapshotTemp, numSymbols)
        dfSnapshot$YearlyPriceHigh <- getField("fiftyTwoWeekHigh", dfSnapshotTemp, numSymbols)
        dfSnapshot$MarketCap <- getField("marketCap", dfSnapshotTemp, numSymbols)
        dfSnapshot$PE <- getField("trailingPE", dfSnapshotTemp, numSymbols)
        dfSnapshot$Yield <- getField("trailingAnnualDividendYield", dfSnapshotTemp, numSymbols)

        dfSnapshot <- dfSnapshot %>%
            mutate(TradeTime = map2_chr(TradeTime, Timezone, .f = ~getTradeTime(.x, .y))) %>%
            mutate(MarketCap = ifelse(MarketCap >= 1000000000,
                                      paste0(round(MarketCap/1000000000, 2), "B"),
                                      paste0(round(MarketCap/1000000, 2), "M"))) %>%
            mutate(Change = ifelse(!is.na(Change), paste0(round(Change, 2),"%"), NA),
                   PE = round(PE, 2),
                   PriceLow = round(PriceLow, 2),
                   PriceHigh = round(PriceHigh, 2),
                   YearlyPriceLow = round(YearlyPriceLow, 2),
                   YearlyPriceHigh = round(YearlyPriceHigh, 2),
                   Yield = ifelse(!is.na(Yield), paste0(round(Yield, 4)*100, "%"), NA)) %>%
            unite(Range.Today, PriceLow, PriceHigh, sep = " - ") %>%
            unite(Range.52.weeks, YearlyPriceLow, YearlyPriceHigh, sep = " - ") %>%
            select(-Timezone)
    }
    return(dfSnapshot)
}