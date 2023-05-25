
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

# getSnapshot <- function(vSymbols) {
#     numSymbols <- length(vSymbols)
#     dfSnapshot <- tibble(Symbol = str_replace_all(vSymbols, "\\-", "\\."),
#                          Name = NA, TradeTime = NA, Last = NA, Change = NA, PriceLow = NA, PriceHigh = NA,
#                          YearlyPriceLow = NA, YearlyPriceHigh = NA, MarketCap = NA, PE = NA, Yield = NA)
#     tickerString <- paste(vSymbols, collapse = ",")
#     queryURL <- sprintf("https://query1.finance.yahoo.com/v7/finance/quote?formatted=false&symbols=%s",tickerString)
#     dfSnapshotTemp <- fromJSON(queryURL)$quoteResponse$result
#     if(is.data.frame(dfSnapshotTemp) & nrow(dfSnapshotTemp) > 0) {
#         dfSnapshot$Name <- getField("longName", dfSnapshotTemp, numSymbols)
#         dfSnapshot$TradeTime <- getField("regularMarketTime", dfSnapshotTemp, numSymbols)
#         dfSnapshot$Timezone <- getField("exchangeTimezoneShortName", dfSnapshotTemp, numSymbols)
#         dfSnapshot$Last <- getField("regularMarketPrice", dfSnapshotTemp, numSymbols)
#         dfSnapshot$Change <- getField("regularMarketChangePercent", dfSnapshotTemp, numSymbols)
#         dfSnapshot$PriceLow <- getField("regularMarketDayLow", dfSnapshotTemp, numSymbols)
#         dfSnapshot$PriceHigh <- getField("regularMarketDayHigh", dfSnapshotTemp, numSymbols)
#         dfSnapshot$YearlyPriceLow <- getField("fiftyTwoWeekLow", dfSnapshotTemp, numSymbols)
#         dfSnapshot$YearlyPriceHigh <- getField("fiftyTwoWeekHigh", dfSnapshotTemp, numSymbols)
#         dfSnapshot$MarketCap <- getField("marketCap", dfSnapshotTemp, numSymbols)
#         dfSnapshot$PE <- getField("trailingPE", dfSnapshotTemp, numSymbols)
#         dfSnapshot$Yield <- getField("trailingAnnualDividendYield", dfSnapshotTemp, numSymbols)
#
#         dfSnapshot <- dfSnapshot %>%
#             mutate(TradeTime = map2_chr(TradeTime, Timezone, .f = ~getTradeTime(.x, .y))) %>%
#             mutate(MarketCap = round(MarketCap/1000000000, 2)) %>%
#             mutate(Symbol = str_replace_all(Symbol, "\\.", "\\-")) %>%
#             mutate(Change = ifelse(!is.na(Change), paste0(round(Change, 2),"%"), NA),
#                    PE = round(PE, 2),
#                    PriceLow = sprintf("%.2f", round(PriceLow, 2)),
#                    PriceHigh = sprintf("%.2f", round(PriceHigh, 2)),
#                    YearlyPriceLow = sprintf("%.2f", round(YearlyPriceLow, 2)),
#                    YearlyPriceHigh = sprintf("%.2f", round(YearlyPriceHigh, 2)),
#                    Yield = ifelse(!is.na(Yield), paste0(round(Yield, 4)*100, "%"), NA)) %>%
#             unite(Range.Today, PriceLow, PriceHigh, sep = " - ") %>%
#             unite(Range.52.weeks, YearlyPriceLow, YearlyPriceHigh, sep = " - ") %>%
#             select(-Timezone)
#     }
#     return(dfSnapshot)
# }


getSnapshot <- function(vSymbols) {
    numSymbols <- length(vSymbols)
    dfCols <- tribble(~internalQF, ~yahooQF,
                      "Name", "Name (Long)",
                      "Last", "Last Trade (Price Only)",
                      "Change", "Change in Percent",
                      "PriceLow", "Days Low",
                      "PriceHigh", "Days High",
                      "YearlyPriceLow", "52-week Low",
                      "YearlyPriceHigh", "52-week High",
                      "MarketCap", "Market Capitalization",
                      "PE", "P/E Ratio",
                      "Yield", "Dividend Yield")

    vColNames <- c("TradeTime", dfCols$internalQF)

    dfSnapshot <- getQuote(vSymbols, what = yahooQF(dfCols$yahooQF))

    dfSnapshot <- dfSnapshot %>%
        tk_tbl(preserve_index = F) %>%
        magrittr::set_colnames(vColNames) %>%
        # mutate(Symbol = str_replace_all(vSymbols, "\\-", "\\.")) %>%
        mutate(MarketCap = round(MarketCap/1000000000, 2)) %>%
        mutate(Change = ifelse(!is.na(Change), paste0(round(Change, 2),"%"), NA),
               PE = round(PE, 2),
               PriceLow = sprintf("%.2f", round(PriceLow, 2)),
               PriceHigh = sprintf("%.2f", round(PriceHigh, 2)),
               YearlyPriceLow = sprintf("%.2f", round(YearlyPriceLow, 2)),
               YearlyPriceHigh = sprintf("%.2f", round(YearlyPriceHigh, 2)),
               Yield = ifelse(!is.na(Yield), paste0(round(Yield, 4)*100, "%"), NA)) %>%
        unite(Range.Today, PriceLow, PriceHigh, sep = " - ") %>%
        unite(Range.52.weeks, YearlyPriceLow, YearlyPriceHigh, sep = " - ") %>%
        select(Symbol, everything())

    return(dfSnapshot)
}
