EPSILON <- 1e-16


annualizedReturns <- function(Z, series_scale = 12) {
    Z <- na.omit(Z)
    numPeriods <- length(Z)
    if(numPeriods > 0) {
        cumReturn <- cumprod(1.0 + Z)
        anlReturn <- (cumReturn[length(cumReturn)])^(series_scale/numPeriods) - 1.0
        return(anlReturn)
    } else {
        return(NA_real_)
    }
}

annualizedStandardDeviation <- function(Z, series_scale = 12) {
    Z <- na.omit(Z)
    numPeriods <- length(Z)
    if(numPeriods > 0) {
        stdev <- sd(Z) * sqrt(series_scale)
        return(stdev)
    } else {
        return(NA_real_)
    }
}

annualizedSemiDeviation <- function(Z, series_scale = 12) {
    semidev <- SemiDeviation(Z) * sqrt(series_scale)
    return(semidev)
}

getPerformanceDataList <- function(dfDailyReturns, dfMonthlyRiskFreeReturns, lPastYears=list("Overall")) {

    dfReturns <- getMonthlyReturns(dfDailyReturns) %>% drop_na()
    numberOfMonths = nrow(dfReturns)

    lPerf <- list() ; lPeriod <- list() ; perfCtr <- 0
    for(yrs in lPastYears) {
        mths <- numberOfMonths
        if(!str_detect(str_to_upper(yrs), "ALL")) mths <- yrs * 12
        if(numberOfMonths < mths) next
        dfReturnsSub <- tail(dfReturns, mths)

        firstDateSub <- dfReturnsSub %>% pull(Date) %>% first() %>% as.Date()
        firstDailyDateSub <- lubridate::make_date(lubridate::year(firstDateSub), lubridate::month(firstDateSub), 1)

        dfDailyReturnsSub <- dfDailyReturns %>% filter(Date >= firstDailyDateSub)

        dfPerf <- getPerformanceMetrics(dfDailyReturnsSub, dfMonthlyRiskFreeReturns)

        firstDate <- dplyr::first(dfReturnsSub$Date)
        lastDate <- dplyr::last(dfReturnsSub$Date)
        tableCaption <- paste(format(firstDate, "%b %Y"), "-", format(lastDate, "%b %Y"))
        tableName <- paste0(yrs, ifelse(yrs == 1, " year", ifelse(yrs == "Overall", "" ," years")))

        perfCtr <- perfCtr + 1
        lPerf[[perfCtr]] <- list(tableName = tableName, tableCaption = tableCaption, tableData = dfPerf)
    }
    return(lPerf)
}

getPerformanceMetrics <- function(dfDailyReturns, dfMonthlyRiskFreeReturns) {

    dfReturns <- dfMonthlyRiskFreeReturns %>% inner_join(getMonthlyReturns(dfDailyReturns), by = "Date") %>% drop_na()

    firstDate <- first(dfReturns$Date) ; lastDate <- last(dfReturns$Date)
    firstYearMonth <- paste(lubridate::month(firstDate, label=T, abbr=T),lubridate::year(firstDate))
    lastYearMonth <- paste(lubridate::month(lastDate, label=T, abbr=T),lubridate::year(lastDate))
    metricsHeading <- paste(firstYearMonth, lastYearMonth, sep = " - ")

    df <- dfReturns %>%
        gather(Symbol, Ra, -Date, -TBILLS) %>%
        mutate(Symbol = factor(Symbol, levels = unique(Symbol))) %>%
        mutate(Excess = Ra - TBILLS) %>%
        mutate(Downside = ifelse((Ra + 1e-12) > TBILLS, NA, Ra)) %>%
        group_by(Symbol)

    riskFreeRatePercent <- annualizedReturns(df$TBILLS)
    riskFreeRatePercent <- paste0(round(riskFreeRatePercent*100, digits = 2), "%")
    dfAnlReturn <- df %>% dplyr::summarise(AnnualizedReturn = annualizedReturns(Ra))
    dfAnlReturnExcess <- df %>% dplyr::summarise(AnnualizedReturnExcess = annualizedReturns(Excess))
    dfAnlStdev <- df %>% dplyr::summarise(AnnualizedStdDev = annualizedStandardDeviation(Ra))
    dfSemiDev <- df %>% dplyr::summarise(AnnualizedSemidev = annualizedSemiDeviation(Downside))
    dfSkewness <- df %>% dplyr::summarise(Skewness = skewness(Ra))

    # summarise maximum drawdown on daily returns
    dfWorstDD <- dfDailyReturns %>%
        dplyr::summarise_if(is_bare_double, ~ suppressWarnings(maxDrawdown(.x, invert = F))) %>%
        gather(Symbol, WorstDD) %>%
        mutate(Symbol = factor(Symbol, levels = levels(df$Symbol)))

    dfMonthwise <- df %>% dplyr::summarise(`Worst Month` = min(Ra),
                                           `Best Month` = max(Ra))
                                           #`Profitable Months` = mean((Ra + 1e-12) >= 0.0))

    dfPerf <- reduce(list(dfAnlReturn, dfAnlReturnExcess, dfAnlStdev, dfSemiDev, dfWorstDD, dfSkewness, dfMonthwise),
                     inner_join, by = "Symbol") %>%
        mutate(Sharpe = AnnualizedReturnExcess/AnnualizedStdDev,
               Sortino = AnnualizedReturnExcess/AnnualizedSemidev) %>%
        select(-AnnualizedReturnExcess, -AnnualizedSemidev, -Sortino, -Skewness) %>%
        select(Symbol, AnnualizedReturn, AnnualizedStdDev, Sharpe, WorstDD, everything())

    vColNames <- colnames(dfPerf) %>%
        str_replace_all("Annualized", "Annualized ") %>%
        str_replace("Sharpe", sprintf("Sharpe Rf=%s", riskFreeRatePercent)) %>%
        str_replace("Sortino", sprintf("Sortino MAR=%s", riskFreeRatePercent)) %>%
        str_replace("Calmar", "Calmar Ratio") %>%
        str_replace("WorstDD", "Worst Drawdown")
    colnames(dfPerf) <- vColNames

    return(dfPerf)
}


getRecentReturns <- function(dfReturns, pastYears = 12) {
    mostRecentMonthNumber <- dfReturns %>% slice(n()) %>% pull(Date) %>% lubridate::month()
    numMonthsFromTail <- pastYears*12 + mostRecentMonthNumber

    dfReturns <- dfReturns %>% tail(numMonthsFromTail)

    dfMTD <- dfReturns %>%
        slice(n()) %>%
        dplyr::rename(Year = Date) %>%
        mutate(Year = paste(lubridate::month(Year, label = TRUE, abbr = TRUE), lubridate::year(Year)))

    dfYearly <- table.CalendarReturns(data.frame(dfReturns, row.names = 1), as.perc = FALSE, digits = 4) %>%
        timetk::tk_tbl(rename_index = "Year", silent = TRUE) %>%
        arrange(desc(Year)) %>%
        mutate(Year = as.character(Year)) %>%
        select(-c(2:13)) %>%
        head(pastYears)

    dfRecentReturns <- bind_rows(dfMTD, dfYearly) %>%
        data.frame(row.names = 1) %>%
        t() %>%
        timetk::tk_tbl(rename_index = "Symbol", silent = TRUE) %>%
        mutate(Symbol = factor(Symbol, levels = unique(Symbol)))
}


getMonthlyRiskFreeReturns <- function(symbol = "DTB3", firstDownloadDate = "2009-01-01") {
    dfRiskFree <- tq_get(symbol, get = "economic.data", from = firstDownloadDate) %>%
        set_names(c("Symbol","Date","TBILLS")) %>%
        mutate(Year = lubridate::year(Date)) %>%
        add_count(Year) %>%
        mutate(TBILLS = if_else(n > 250, TBILLS/(100*n), TBILLS/(100*261))) %>%
        select(Date, TBILLS) %>%
        getMonthlyReturns()
}

# Download capital gains from Yahoo Finance
getCapitalGains <- function(symbol) {
    startEpoch <- as.numeric(as.POSIXct("1970-01-01"))
    lastEpoch <- as.numeric(as.POSIXct(Sys.Date()))

    urlPath <- glue::glue("https://finance.yahoo.com/quote/{symbol}/history?period1={startEpoch}&period2={lastEpoch}&interval=capitalGain%7Cdiv%7Csplit&filter=capital&frequency=1d&includeAdjustedClose=true")
    htmldoc <- read_html(urlPath)

    dfCap <- htmldoc %>%
        html_table() %>%
        as.data.frame() %>%
        drop_na() %>%
        select(Date = 1, CapGain = 2) %>%
        as_tibble() %>%
        mutate(Symbol = symbol) %>%
        drop_na() %>%
        select(Symbol, Date, CapGain)

    if(nrow(dfCap) > 0 & is_double(dfCap$CapGain)) {
        dfCap <- dfCap %>%
            mutate(Date = anytime::anydate(Date)) %>%
            mutate(CapGain = parse_number(CapGain))
    } else {
        dfCap <- tibble(Symbol = character(), Date = as.Date(character()), CapGain = double())
    }

    return(dfCap)
}


getOHLCReturns <- function(symbol, firstDownloadDate = "1965-01-01", endDownloadDate = Sys.Date()) {
    print(paste("Downloading data for", symbol))

    dfOHLC <- NULL
    if(str_detect(symbol, "/")) {
        dfSymbol <- Quandl::Quandl(symbol, start_date = firstDownloadDate, end_date = endDownloadDate, collapse = "daily", order = "asc")
        dfSymbol <- dfSymbol %>%
            select(Date = 1, Adjusted = 2) %>%
            as_tibble() %>%
            mutate(Open=NA_real_, High=NA_real_, Low=NA_real_, Close=NA_real_, Volume=NA_real_) %>%
            select(Date, Open:Volume, Adjusted) %>%
            tq_mutate(select = Adjusted, mutate_fun = Delt, col_rename = "Return")
    } else {
        dfSymbol <- tq_get(symbol, get = "stock.prices", from = firstDownloadDate, to = endDownloadDate)
        if(!is.na(dfSymbol)) {
            dfSymbol <- dfSymbol %>%
                rename_all(str_to_title) %>%
                dplyr::filter(abs(Open) > EPSILON | abs(High) > EPSILON | abs(Low) > EPSILON | abs(Close) > EPSILON | abs(Adjusted) > EPSILON) %>%
                tq_mutate(select = Adjusted, mutate_fun = Delt, col_rename = "Return")
        }
    }

    # check additional capital gains returns for mutual and index funds
    if(!is.na(str_extract(symbol, "^[A-Za-z]+$")) & (str_count(symbol) >= 5)) {
        print(paste("Downloading capital gains data for", symbol))
        dfCap <- getCapitalGains(symbol)

        if(nrow(dfCap) > 0) {
            dfSymbol <- dfSymbol %>%
                left_join(dfCap %>% select(-Symbol), by = "Date") %>%
                arrange(Date) %>%
                mutate(CapReturn = CapGain/dplyr::lag(Close)) %>%
                mutate(Return = Return + coalesce(CapReturn, 0)) %>%
                select(-CapGain, -CapReturn)
        }
    }

    if(!is.na(dfSymbol)) dfOHLC <- dfSymbol
    return(dfOHLC)
}


# Download daily returns from Yahoo Finance or Quandl
getDailyReturns <- function(symbol, symbolPrior = NA, firstDownloadDate = "1965-01-01") {
    dfSymbol <- getOHLCReturns(symbol, firstDownloadDate = firstDownloadDate)

    # combine with data of previous symbol
    if(!is.na(symbolPrior)) {
        endDownloadDate <- min(dfSymbol$Date) + 1
        dfSymbolPrior <- getOHLCReturns(symbolPrior, firstDownloadDate = firstDownloadDate, endDownloadDate = endDownloadDate)

        print(paste("Merging data of", symbol, "and", symbolPrior))
        dfSymbol <- dfSymbolPrior %>% bind_rows(dfSymbol[2:nrow(dfSymbol),])
    }

    if(!is.null(dfSymbol)) dfSymbol <- dfSymbol %>% mutate(LogReturn = log(1 + Return))
    return(dfSymbol)
}

# Download daily returns from Yahoo Finance or Quandl
# getDailyReturns <- function(symbol, symbolPrior = NA, firstDownloadDate = "1965-01-01") {
#     print(paste("Downloading data for", symbol))
#
#     dfSymbol <- tq_get(symbol, get = "stock.prices", from = firstDownloadDate) %>%
#         dplyr::filter(abs(open) > EPSILON | abs(high) > EPSILON | abs(low) > EPSILON | abs(close) > EPSILON | abs(adjusted) > EPSILON) %>%
#         tq_mutate(select = adjusted, mutate_fun = Delt, col_rename = "Return")
#
#     if(!is.na(symbolPrior)) {
#         print(paste("Downloading data for", symbolPrior))
#         firstSymbolDate <- min(dfSymbol$date)+1
#
#         # Check to download data from Quandl
#         if(stringr::str_count(symbolPrior, "/") > 0) {
#             dfSymbolPrior <- Quandl::Quandl(symbolPrior,
#                                             start_date = firstDownloadDate, end_date = firstSymbolDate,
#                                             collapse = "daily",
#                                             order = "asc")
#             dfSymbolPrior <- dfSymbolPrior %>%
#                 select(c(1, 2)) %>%
#                 set_names(c("date", "adjusted")) %>%
#                 as_tibble() %>%
#                 mutate(open=NA_real_, high=NA_real_, low=NA_real_, close=NA_real_, volume=NA_real_) %>%
#                 select(date, open:volume, adjusted) %>%
#                 tq_mutate(select = adjusted, mutate_fun = Delt, col_rename = "Return")
#         } else {
#             dfSymbolPrior <- tq_get(symbolPrior, get = "stock.prices", from = firstDownloadDate, to = firstSymbolDate) %>%
#                 dplyr::filter(abs(open) > EPSILON | abs(high) > EPSILON | abs(low) > EPSILON | abs(close) > EPSILON | abs(adjusted) > EPSILON) %>%
#                 tq_mutate(select = adjusted, mutate_fun = Delt, col_rename = "Return")
#         }
#
#         print(paste("Merging data of", symbol, "and", symbolPrior))
#         dfSymbol <- bind_rows(dfSymbolPrior, dfSymbol[2:nrow(dfSymbol),])
#     }
#
#     # Capitalize column names and set log return
#     dfSymbol <- dfSymbol %>%
#         rename_all(str_to_title) %>%
#         mutate(LogReturn = log(1 + Return))
#     return(dfSymbol)
# }


# Returns a data frame of monthly returns in the format: Date, <colname_initial> with month end date,
# from a data frame containing daily returns in the format:
# Date, <colname_initial> OR <colname_initial> with Date in rownames
getMonthlyReturns <- function(dfDailyReturns, removeNAs = T) {

    if(ncol(dfDailyReturns)==1L) {
        dfDailyReturns <- dfDailyReturns %>% rownames_to_column(var = "DailyDate")
    }
    else {
        colnames(dfDailyReturns)[1] <- "DailyDate"
    }

    cumreturn <- function(Z) {if(sum(is.na(Z))==length(Z)) return(NA) else return(prod(1L + Z, na.rm = T) - 1L)}

    dfMonthlyReturns <- dfDailyReturns %>%
        mutate(Date = as.Date(as.yearmon(DailyDate), frac=1)) %>%
        dplyr::select(-DailyDate) %>%
        group_by(Date) %>%
        dplyr::summarise_all(list(cumreturn))

    if(removeNAs) dfMonthlyReturns <- dfMonthlyReturns %>% drop_na()

    return(dfMonthlyReturns)
}


# Returns a table of returns by calendar year
getCalendarReturns <- function(dfReturns) {
    dfReturns <- data.frame(dfReturns %>% getMonthlyReturns(), row.names = 1)
    tblReturns <- PerformanceAnalytics::table.CalendarReturns(dfReturns)
    if(ncol(dfReturns)>1) {
        tblReturns <- tblReturns[13:ncol(tblReturns)]
    }
    return(tblReturns)
}


# Returns a data frame of cumulative log returns
getCumulativeLogReturns <- function(dfReturns) {
    dfCumReturns <- dfReturns %>%
        map_if(is_bare_double, function(Z) {cumsum(ifelse(is.na(Z), 0, log(1.0 + Z))) + Z*0}) %>%
        as_tibble()

    return(dfCumReturns)
}

# Rolling Annualized Returns
# dfReturns: Date, TBILLS.MonthlyReturns, Stock.MonthlyReturns, SP500TR.MonthlyReturns
# return: list of data frames of rolling annualized returns by rolling period
rollingAnnualizedReturns <- function(Z, rollingMonths) {
    Z <- na.omit(Z)
    cumReturn <- cumprod(1.0 + Z)
    rollAnlReturn <- (cumReturn/dplyr::lag(cumReturn, n = rollingMonths))^(12/rollingMonths) - 1
}


getRollingAnnualizedReturns <- function(dfReturns, rollingMonths) {
    numberOfMonths = nrow(dfReturns)

    dfReturns <- dfReturns %>%
        drop_na() %>%
        gather(Security, Return, -Date) %>%
        mutate(Security = factor(Security, levels = unique(Security))) %>%
        group_by(Security)

    dfRollingReturns <- dfReturns %>%
        mutate(RollingReturn = rollingAnnualizedReturns(Return, rollingMonths)) %>%
        select(-Return) %>%
        ungroup() %>%
        spread(Security, RollingReturn) %>%
        drop_na()

    return(dfRollingReturns)
}


# PRE: dfReturns has monthly returns
# POST: Returns a data frame <date_from, date_to> of the recession intervals during this time series
getRecessionIntervals <- function(dfReturns, dfRecessions) {
    dfRecessions <- dfReturns %>%
        full_join(dfRecessions, by = "Date") %>%
        filter(Date >= min(dfReturns$Date) & Date <= max(dfReturns$Date)) %>%
        arrange(Date) %>%
        mutate(Recession = na.locf(Recession, na.rm = F)) %>%
        replace_na(list(Recession = 0)) %>%
        select(Date, Recession)

    runs <- rle(as.logical(dfRecessions$Recession == 1))
    l <- list(start = cumsum(runs$lengths)[which(runs$values)] - runs$lengths[which(runs$values)] + 1,
              end = cumsum(runs$lengths)[which(runs$values)])
    dfRect <- tibble(date_from = dfRecessions$Date[l$start], date_to = dfRecessions$Date[l$end])
    return(dfRect)
}

# Reorder correlation matrix by hierarchical clustering
reorderCorrelationMatrix <- function(mCor) {
    dd <- as.dist((1-mCor)/2)
    hc <- hclust(dd)
    mCor <- mCor[hc$order, hc$order]
    return(mCor)
}

# Returns correlations of monthly returns of a given strategy with other strategies, in the given time frames
getCorrelationsVerbose <- function(dfReturns, lPastYears=list('ALL'), strategyName) {
    firstDate <- as.Date(first(dfReturns$Date)) ; lastDate <- as.Date(last(dfReturns$Date))
    firstYearMonth <- paste(lubridate::month(firstDate, label=T, abbr=T),lubridate::year(firstDate))
    lastYearMonth <- paste(lubridate::month(lastDate, label=T, abbr=T),lubridate::year(lastDate))
    numberOfMonths = nrow(dfReturns)

    strategyColIndex <- which(colnames(dfReturns)==strategyName)
    lCor <- list()
    lCorNames <- list()
    i <- 0
    for(yrs in lPastYears) {
        if(str_detect(str_to_upper(yrs), "ALL")) mths = numberOfMonths else mths = yrs*12
        if(mths > numberOfMonths) next
        dfReturnsPast <- tail(dfReturns, n=mths)
        firstDatePast <- as.Date(first(dfReturnsPast$Date))
        firstYearMonthPast <- paste(lubridate::month(firstDatePast, label=T, abbr=T), lubridate::year(firstDatePast))

        colnames_prefix <- ifelse(str_detect(str_to_upper(yrs), "ALL"),
                                  paste(firstYearMonth,"-",lastYearMonth),
                                  paste(firstYearMonthPast,"-",lastYearMonth))

        lCorPast <- map(dfReturnsPast %>% select(-all_of(c(1, strategyColIndex))), cor.test, y = dfReturnsPast[[strategyColIndex]])
        i <- i + 1
        lCor[[i]] <- lCorPast
        lCorNames[[i]] <- colnames_prefix
    }
    names(lCor) <- lCorNames

    dfCor <- map(lCor, ~ .x %>% map_dfr(broom::tidy, .id = "predictor")) %>%
        bind_rows(.id = "time_frame") %>%
        mutate(time_frame = fct_inorder(time_frame))

    return(dfCor)
}

# Returns a data frame of correlation estimates only within each time frame
getCorrelations <- function(dfReturns, lPastYears=list('ALL'), strategyName) {
    vSymbols <- colnames(dfReturns[-1])
    dfCor <- getCorrelationsVerbose(dfReturns, lPastYears, strategyName) %>%
        select(time_frame, predictor, estimate) %>%
        spread(predictor, estimate) %>%
        select(time_frame, any_of(vSymbols)) %>%
        data.frame(row.names = 1)

    return(dfCor)
}


# Generate a data.frame of drawdowns
getDrawdowns <- function(dfReturns, colIndex=NA, colName, top=5) {
    if(!is.na(colIndex)) {
        df <- data.frame(dfReturns[c(1, colIndex)], row.names = 1)
    } else {
        df <- data.frame(dfReturns[c("Date", colName)], row.names = 1)
    }

    dfDrawdowns <- suppressWarnings(table.Drawdowns(df)) %>% as_tibble() %>%
        set_names(c("Peak","Trough","Recovery","Depth","Length","ToTrough","ToRecovery")) %>%
        mutate_at(.vars = c("Length","ToTrough","ToRecovery"), as.integer) %>%
        mutate(PeakDate = Peak, TroughDate = Trough, RecoveryDate = Recovery) %>%
        mutate_at(.vars = c("Peak", "Trough", "Recovery"),
                  function(Z) ifelse(!is.na(Z), paste(lubridate::month(Z, label=T, abbr=T),lubridate::year(Z)), NA)) %>%
        arrange(PeakDate) %>%
        mutate(Peak = factor(Peak, levels = unique(Peak))) %>%
        arrange(TroughDate) %>%
        mutate(Trough = factor(Trough, levels = unique(Trough))) %>%
        arrange(RecoveryDate) %>%
        mutate(Recovery = factor(Recovery, levels = unique(Recovery))) %>%
        select(-PeakDate, -TroughDate, -RecoveryDate) %>%
        arrange(Depth)
}


#PRE: dfReturns is a data frame of monthly returns
#POST: prints data frame of performance in each period in lPastYears list
getLatestPerformance <- function(dfDailyReturns, lPastYears=list('ALL'), ishtmlOutput = FALSE, showReturnsOnly = FALSE) {
    dfReturns <- dfDailyReturns %>% getMonthlyReturns() %>% data.frame(row.names = 1) %>% na.omit()

    hasRiskFreeReturns <- FALSE
    vRiskFreeRates <- getRiskFreeRatesSymbols()
    if(colnames(dfReturns)[1] %in% vRiskFreeRates) hasRiskFreeReturns <- TRUE

    firstDate <- as.Date(first(rownames(dfReturns)))
    lastDate <- as.Date(last(rownames(dfReturns)))
    firstYearMonth <- paste(lubridate::month(firstDate, label=T, abbr=T),lubridate::year(firstDate))
    lastYearMonth <- paste(lubridate::month(lastDate, label=T, abbr=T),lubridate::year(lastDate))

    ctr <- 1
    dfPerfFinal <- data.frame()
    numberOfMonths = nrow(dfReturns) ; numberOfAssets = ncol(dfReturns)
    for(yrs in lPastYears) {
        if(str_detect(str_to_upper(yrs), "ALL")) mths = numberOfMonths else mths = yrs*12
        if(numberOfMonths >= mths) {
            dfReturnsPast <- tail(dfReturns, n=mths)

            firstDatePast <- as.Date(first(rownames(dfReturnsPast)))
            firstYearMonthPast <- paste(lubridate::month(firstDatePast, label=T, abbr=T), lubridate::year(firstDatePast))
            firstDailyDatePast <- lubridate::make_date(lubridate::year(firstDatePast), lubridate::month(firstDatePast), 1)

            dfDailyReturnsPast <- dfDailyReturns %>% filter(Date >= firstDailyDatePast) %>% data.frame(row.names = 1)

            if(hasRiskFreeReturns) {
                dfDailyReturnsAssets <- subset(dfDailyReturnsPast, select = c(2:numberOfAssets))
                dfReturnsAssets <- subset(dfReturnsPast, select=c(2:numberOfAssets))
                dfReturnsRiskFree <- dfReturnsPast[,1,drop=F]
            }
            else {
                dfDailyReturnsAssets <- dfDailyReturnsPast
                dfReturnsAssets <- dfReturnsPast
                dfReturnsRiskFree <- data.frame(vector(mode='numeric', length=nrow(dfReturnsPast)))
                rownames(dfReturnsRiskFree)=rownames(dfReturnsAssets)
            }
            dfMaxDD <- maxDrawdown(dfDailyReturnsAssets, invert = F) * 100
            if(is.null(dim(dfMaxDD))) {
                dim(dfMaxDD) <- c(1, 1)
                colnames(dfMaxDD) <- colnames(dfReturnsAssets)
                rownames(dfMaxDD) <- "Worst Drawdown"
            }

            dfPerf <- table.AnnualizedReturns(dfReturnsAssets, scale=12, Rf=dfReturnsRiskFree)
            dfPerf[c(1,2),] <- dfPerf[c(1,2),] * 100
            rownames(dfPerf)[3] <- str_replace(row.names(dfPerf)[3], "Annualized ", "")

            dfPerf <- rbind(dfPerf, dfMaxDD)
            dfPerf <- format(round(dfPerf, digits = 2), justify = 'right', nsmall = 2, scientific = F)
            dfPerf[1,] <- paste0(dfPerf[1,], "%")
            dfPerf[2,] <- paste0(dfPerf[2,], "%")
            dfPerf[4,] <- paste0(dfPerf[4,], "%")

            if(showReturnsOnly) {
                dfPerf <- dfPerf %>% head(1)
            }

            if(str_detect(str_to_upper(yrs), "ALL")) rownames_prefix <- paste(firstYearMonth,"-",lastYearMonth)
            else rownames_prefix <- paste(firstYearMonthPast,"-",lastYearMonth)
            rownames(dfPerf) <- paste(rownames_prefix,rownames(dfPerf))

            # add an empty line between performance segments
            mEmpty <- matrix(data=colnames(dfPerf), nrow=1, ncol=ncol(dfPerf)) ; dfEmpty <- data.frame(mEmpty)
            colnames(dfEmpty) <- colnames(dfPerf)
            rownames(dfEmpty) <- paste0(rep(" ", ctr), collapse="")

            ctr <- ctr+1

            if(ishtmlOutput) {
                if(nrow(dfPerfFinal)==0) dfPerfFinal <- dfPerf else dfPerfFinal <- rbind(dfPerfFinal, dfPerf)
            } else {
                if(nrow(dfPerfFinal)==0) dfPerfFinal <- dfPerf else dfPerfFinal <- rbind(dfPerfFinal, dfEmpty, dfPerf)
            }
        }
    }
    return(dfPerfFinal)
}


# Returns the nearest number from raw number that is a multiple of m
getNearestMultipleOf <- function(rawNumber, m) {
    remainder <- rawNumber %% m
    nearestNumber <- ifelse((remainder + 1e-10) > (m/2), rawNumber + m - remainder, rawNumber - remainder)
    return(as.integer(nearestNumber))
}


# Returns date breaks depending upon the span
getDateBreaks <- function(vDates) {
    firstYear <- lubridate::year(first(vDates))
    lastYear <- lubridate::year(last(vDates))
    yearDiff <- lastYear - firstYear
    yearLabels <- c(firstYear:lastYear)
    yearBreaks <- "1 year"
    if(yearDiff > 70) {
        yearLabels <- yearLabels[yearLabels %% 10 == 0]
        yearBreaks <- "10 years"
    }
    else if(yearDiff > 12) {
        yearLabels <- yearLabels[yearLabels %% 4 == 0]
        yearBreaks <- "4 years"
    } else if(yearDiff > 1) {
        yearLabels <- yearLabels[yearLabels %% 2 == 0]
        yearBreaks <- "2 years"
    }
    dateBreaks <- seq.Date(from = as.Date(paste0(first(yearLabels), "-01-01")),
                           to = as.Date(paste0(last(yearLabels), "-12-31")),
                           by = yearBreaks)
    return(dateBreaks)
}


# get Risk Free Symbols
getRiskFreeRatesSymbols <- function() {
    return(c("LIBAUD","LIBGBP","LIBCAD","LIBCHF","LIBEUR","LIBJPY","LIBUSD","LIBOR.USD","SARINR","TBILLS","TBills","Tbills","TBILL_3M","Cash","CASH"))
}

# get Futures Symbols
getFuturesSymbols <- function() {
    vCURR <- c("AD","BP","CD","CU","JY","MP","NE","RF","RP","RY","SF")
    names(vCURR) <- rep("CURR", length(vCURR))

    vRATE <- c("BAX","ED","FEI","FES","FSS","JEY","YBA")
    names(vRATE) <- rep("RATE", length(vRATE))

    vBOND <- c("CGB","EBL","EBM","EBS","FLG","FV","JGB","TU","TY","US","YTC","YTT")
    names(vBOND) <- rep("RATE", length(vBOND))

    vSTOX <- c("EMD","ES","FCH","FDX","FFI","HCE","HSI","IFS","JNI","JTI","KOS","MFX","NQ","SIN","SMI","SSG","STW","SXE","SXF","TF","YAP","YM")
    names(vSTOX) <- rep("STOX", length(vSTOX))

    vAGRI <- c("BO","C","CC","CT","FC","JRU","KC","KW","LB","LC","LH","O","OJ","PB","RR","RS","S","SB","SM","W")
    names(vAGRI) <- rep("AGRI", length(vAGRI))

    vENMT <- c("CL2","HO2","LCO","LGO","NG2","RB2","GC2","HG2","PA2","PL2","SI2")
    names(vENMT) <- rep("ENMT", length(vENMT))

    vSymbols <- c(vCURR, vRATE, vBOND, vSTOX, vAGRI, vENMT)
}


# get business days calendar for futures
getFuturesBusinessDates <- function(startDate, endDate) {
    dts <- seq.Date(as.Date(startDate), as.Date(endDate), by=1)
    weekdts <- weekdays(dts)
    bizdts <- dts[!(weekdts %in% c("Sat","Sun","Saturday","Sunday"))]               # exclude Saturdays & Sundays
    bizdts <- bizdts[!(lubridate::month(bizdts)==1 & lubridate::day(bizdts)==1)]    # exclude Jan 1
    return(as.character(as.Date(bizdts)))
}

#PRE: User-defined operator - x and y are vectors
#POST: returns a logical vector of length(x) where x is not in y
`%notin%` <- function(x, y) !(`%in%`(x, y))

#PRE: x is a numeric vector with arbitrary number of decimal places
#POST: returns a character vector with values upto d significant decimals
getSignificantDigits <- function(x, d=8) {
    format(round(x, d), nsmall=d, scientific=F)
}

#PRE: x is a numeric or character/factor vector
#POST: returns the value which has the highest frequency, i.e. mode
find_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

#PRE: x is a numeric vector
#POST: returns the k-period percent difference similar to quantmod::Delt function
pct_diff_lagging <- function(x, k = 1) {
    return(x/dplyr::lag(x, n = k) - 1)
}

pct_diff_leading <- function(x, k = 1) {
    return(x/dplyr::lead(x, n = k) - 1)
}

#PRE: contract is of the form where last 6 digits are 201403
#POST: convert the month digits to characters 2014H
replaceByMonthChar <- function(contract) {
    vMthDigits <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
    vMthChar <- c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z")
    names(vMthDigits) <- vMthChar

    mthChar <- ""
    mthDigits <- substr(contract, nchar(contract)-1, nchar(contract)) # last 2 characters
    try({
        i <- match(mthDigits, vMthDigits)
        mthChar <- names(vMthDigits[i])
    })
    contract <- paste(substr(contract, 1, nchar(contract)-2), mthChar, sep="")
}
