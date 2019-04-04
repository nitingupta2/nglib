EPSILON <- 1e-16

getMonthlyRiskFreeReturns <- function(symbol = "DTB3", firstDownloadDate = "2009-01-01") {
    dfRiskFree <- tq_get(symbol, get = "economic.data", from = firstDownloadDate) %>%
        set_names(c("Date","TBILLS")) %>%
        mutate(Year = lubridate::year(Date)) %>%
        add_count(Year) %>%
        mutate(TBILLS = if_else(n > 250, TBILLS/(100*n), TBILLS/(100*261))) %>%
        select(Date, TBILLS) %>%
        getMonthlyReturns()
}

# Download daily returns from Yahoo Finance or Quandl
getDailyReturns <- function(symbol, symbolPrior = NA, firstDownloadDate = "1965-01-01") {
    print(paste("Downloading data for", symbol))

    dfSymbol <- tq_get(symbol, get = "stock.prices", from = firstDownloadDate) %>%
        dplyr::filter(abs(open) > EPSILON | abs(high) > EPSILON | abs(low) > EPSILON | abs(close) > EPSILON | abs(adjusted) > EPSILON) %>%
        tq_mutate(select = adjusted, mutate_fun = Delt, col_rename = "Return")

    if(!is.na(symbolPrior)) {
        print(paste("Downloading data for", symbolPrior))
        firstSymbolDate <- min(dfSymbol$date)+1

        # Check to download data from Quandl
        if(stringr::str_count(symbolPrior, "/") > 0) {
            dfSymbolPrior <- Quandl::Quandl(symbolPrior,
                                            start_date = firstDownloadDate, end_date = firstSymbolDate,
                                            collapse = "daily",
                                            order = "asc")
            dfSymbolPrior <- dfSymbolPrior %>%
                select(c(1, 2)) %>%
                set_names(c("date", "adjusted")) %>%
                tbl_df() %>%
                mutate(open=NA_real_, high=NA_real_, low=NA_real_, close=NA_real_, volume=NA_real_) %>%
                select(date, open:volume, adjusted) %>%
                tq_mutate(select = adjusted, mutate_fun = Delt, col_rename = "Return")
        } else {
            dfSymbolPrior <- tq_get(symbolPrior, get = "stock.prices", from = firstDownloadDate, to = firstSymbolDate) %>%
                dplyr::filter(abs(open) > EPSILON | abs(high) > EPSILON | abs(low) > EPSILON | abs(close) > EPSILON | abs(adjusted) > EPSILON) %>%
                tq_mutate(select = adjusted, mutate_fun = Delt, col_rename = "Return")
        }

        print(paste("Merging data of", symbol, "and", symbolPrior))
        dfSymbol <- rbind(dfSymbolPrior, dfSymbol[2:nrow(dfSymbol),])
    }

    # Capitalize column names and set log return
    dfSymbol <- dfSymbol %>%
        set_names(Hmisc::capitalize(colnames(.))) %>%
        mutate(LogReturn = log(1 + Return))
    return(dfSymbol)
}


# Returns a data frame of monthly returns in the format: Date, <colname_initial> with month end date,
# from a data frame containing daily returns in the format:
# Date, <colname_initial> OR <colname_initial> with Date in rownames
getMonthlyReturns <- function(dfDailyReturns, removeNAs = T) {

    numCols <- ncol(dfDailyReturns)
    if(numCols==1L) {
        secname <- colnames(dfDailyReturns)[1]
        dfDailyReturns[,2] <- dfDailyReturns[,1] ; colnames(dfDailyReturns)[2] <- secname
        dfDailyReturns[,1] <- rownames(dfDailyReturns) ; colnames(dfDailyReturns)[1] <- "DailyDate"
    }
    else {
        colnames(dfDailyReturns)[1] <- "DailyDate"
    }

    cumreturn <- function(Z) {if(sum(is.na(Z))==length(Z)) return(NA) else return(prod(1L + Z, na.rm = T) - 1L)}
    dfMonthlyReturns <- dfDailyReturns %>%
        mutate(MonthlyDate = as.Date(as.yearmon(DailyDate), frac=1)) %>%
        dplyr::select(-DailyDate) %>%
        group_by(MonthlyDate) %>%
        dplyr::summarise_all(funs(cumreturn))
    colnames(dfMonthlyReturns)[1] <- "Date"

    if(removeNAs) dfMonthlyReturns <- dfMonthlyReturns %>% drop_na()

    return(dfMonthlyReturns)
}


# Returns a table of returns by calendar year
getCalendarReturns <- function(dfReturns) {
    dfReturns <- data.frame(dfReturns, row.names = 1)
    tblReturns <- PerformanceAnalytics::table.CalendarReturns(dfReturns)
    if(ncol(dfReturns)>1) {
        tblReturns <- tblReturns[13:ncol(tblReturns)]
    }
    return(tblReturns)
}


# Function to calculate portfolio returns
getPortfolioReturns <- function(xtReturns, xtWeights, portfolioName, transCostPercent = 0) {
    xtWeights <- xtWeights %>% na.omit()

    # calculate monthly turn over
    lPortfolio <- Return.portfolio(R = xtReturns, weights = xtWeights, verbose = TRUE)
    beginWeights <- lPortfolio$BOP.Weight
    endWeights <- lPortfolio$EOP.Weight
    xtTransactions <- beginWeights - lag.xts(endWeights)
    monthlyTO <- xts(rowSums(abs(xtTransactions[,1:length(symbols)])), order.by=index(xtTransactions))

    # calculate transaction costs
    xtTransCosts <- monthlyTO * transCostPercent/100
    xtReturnsWithTC <- lPortfolio$returns - xtTransCosts

    dfPortfolio <- xtReturnsWithTC %>% tk_tbl(preserve_index = T) %>%
        set_names(c("Date", portfolioName)) %>%
        mutate(Date = as.Date(Date))

    return(dfPortfolio)
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
    dfRecessions <- dfReturns %>% left_join(dfRecessions, by = "Date") %>%
        mutate(Recession = na.locf(Recession, na.rm = F)) %>%
        replace_na(list(Recession = 0)) %>%
        select(Date, Recession)

    runs <- rle(as.logical(dfRecessions$Recession == 1))
    l <- list(start = cumsum(runs$lengths)[which(runs$values)] - runs$lengths[which(runs$values)] + 1,
              end = cumsum(runs$lengths)[which(runs$values)])
    dfRect <- tibble(date_from = dfRecessions$Date[l$start], date_to = dfRecessions$Date[l$end])
    return(dfRect)
}


# Returns correlations of monthly returns of a given strategy with other strategies, in the time frames
getCorrelations <- function(dfReturns, lPastYears=list('ALL'), strategyName) {
    firstDate <- as.Date(first(dfReturns$Date)) ; lastDate <- as.Date(last(dfReturns$Date))
    firstYearMonth <- paste(lubridate::month(firstDate, label=T, abbr=T),lubridate::year(firstDate))
    lastYearMonth <- paste(lubridate::month(lastDate, label=T, abbr=T),lubridate::year(lastDate))

    strategyColIndex <- which(colnames(dfReturns)==strategyName) - 1
    dfCor <- data.frame()
    numberOfMonths = nrow(dfReturns)
    for(yrs in lPastYears) {
        if(yrs=='ALL') mths = numberOfMonths else mths = yrs*12
        dfReturnsPast <- tail(dfReturns, n=mths)
        firstDatePast <- as.Date(first(dfReturnsPast$Date))
        firstYearMonthPast <- paste(lubridate::month(firstDatePast, label=T, abbr=T), lubridate::year(firstDatePast))

        colnames_prefix <- ifelse(yrs=='ALL',
                                  paste(firstYearMonth,"-",lastYearMonth),
                                  paste(firstYearMonthPast,"-",lastYearMonth))

        mCor <- cor(dfReturnsPast[-1])
        dfCorTemp <-  as.data.frame(mCor[strategyColIndex, -strategyColIndex])
        colnames(dfCorTemp) <- colnames_prefix
        rownames(dfCorTemp) <- colnames(dfReturnsPast[-1])[-strategyColIndex]
        if(ncol(dfCor)==0) {dfCor <- dfCorTemp}
        else {dfCor <- cbind(dfCor, dfCorTemp)}
    }
    return(t(dfCor))
}


# Generate a table of drawdowns
getDrawdowns <- function(dfReturns, colIndex=NA, colName, top=5) {
    if(!is.na(colIndex)) {
        return(table.Drawdowns(data.frame(dfReturns[c(1, colIndex)], row.names = 1)))
    } else {
        return(table.Drawdowns(data.frame(dfReturns[c("Date", colName)], row.names = 1)))
    }
}


#PRE: dfReturns is a data frame of monthly returns
#POST: prints data frame of performance in each period in lPastYears list
getLatestPerformance <- function(dfReturns, lPastYears=list('ALL'), ishtmlOutput = FALSE, showReturnsOnly = FALSE) {
    dfReturns <- dfReturns %>% data.frame(row.names = 1) %>% na.omit()

    hasRiskFreeReturns <- FALSE
    vRiskFreeRates <- c("LIBAUD","LIBGBP","LIBCAD","LIBCHF","LIBEUR","LIBJPY","LIBUSD","SARINR","TBILLS","TBILL_3M","LIBOR.USD","Cash")
    if(colnames(dfReturns)[1] %in% vRiskFreeRates) hasRiskFreeReturns <- TRUE

    firstDate <- as.Date(first(rownames(dfReturns))) ; lastDate <- as.Date(last(rownames(dfReturns)))
    firstYearMonth <- paste(lubridate::month(firstDate, label=T, abbr=T),lubridate::year(firstDate))
    lastYearMonth <- paste(lubridate::month(lastDate, label=T, abbr=T),lubridate::year(lastDate))

    ctr <- 1
    dfPerfFinal <- data.frame()
    numberOfMonths = nrow(dfReturns) ; numberOfAssets = ncol(dfReturns)
    for(yrs in lPastYears) {
        if(yrs=='ALL') mths = numberOfMonths else mths = yrs*12
        if(numberOfMonths >= mths) {
            dfReturnsPast <- tail(dfReturns, n=mths)
            firstDatePast <- as.Date(first(rownames(dfReturnsPast)))
            firstYearMonthPast <- paste(lubridate::month(firstDatePast, label=T, abbr=T), lubridate::year(firstDatePast))

            if(hasRiskFreeReturns) {
                dfReturnsAssets <- subset(dfReturnsPast, select=c(2:numberOfAssets))
                dfReturnsRiskFree <- dfReturnsPast[,1,drop=F]
            }
            else {
                dfReturnsAssets <- dfReturnsPast
                dfReturnsRiskFree <- data.frame(vector(mode='numeric', length=nrow(dfReturnsPast)))
                rownames(dfReturnsRiskFree)=rownames(dfReturnsAssets)
            }
            dfMaxDD <- maxDrawdown(dfReturnsAssets, invert = F) * 100
            if(is.null(dim(dfMaxDD))) {
                dim(dfMaxDD) <- c(1, 1)
                colnames(dfMaxDD) <- colnames(dfReturnsAssets)
                rownames(dfMaxDD) <- "Worst Drawdown"
            }

            dfPerf <- table.AnnualizedReturns(dfReturnsAssets, scale=12, Rf=dfReturnsRiskFree)
            dfPerf[c(1,2),] <- dfPerf[c(1,2),] * 100
            dfPerf <- rbind(dfPerf, dfMaxDD)
            dfPerf <- format(round(dfPerf, digits = 2), justify = 'right', nsmall = 2, scientific = F)
            dfPerf[1,] <- paste0(dfPerf[1,], "%")
            dfPerf[2,] <- paste0(dfPerf[2,], "%")
            dfPerf[4,] <- paste0(dfPerf[4,], "%")

            if(showReturnsOnly) {
                dfPerf <- dfPerf %>% head(1)
            }

            if(yrs=="ALL") rownames_prefix <- paste(firstYearMonth,"-",lastYearMonth)
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


#PRE: x is a numeric vector with arbitrary number of decimal places
#POST: returns a character vector with values upto d significant decimals
getSignificantDigits <- function(x, d=8) {
    format(round(x, d), nsmall=d, scientific=F)
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
