
# Database Connection String
getDatabaseConnectionString <- function() {
    service <- "SQL Server"
    passwd <- keyring::key_get(service, username = "nitin")
    connString <- glue::glue('driver={service};server=PREDATOR\\SQLEXPRESS;database=MarketData;Persist Security Info=True;username=nitin;password={passwd}')
}

# Read data for testing
dbReadTestData <- function() {
    dfReturns <- dbReadBenchmarksData(c("SP500TR","R2000TR","GEM","BTOP50"))
}

# Read Benchmarks Data
dbReadBenchmarksData <- function(vBenchmarks, tableName = "BenchmarksData", removeNAs = T) {
    benchmarksString <- paste(vBenchmarks, collapse = "','")
    queryString <- glue::glue("SELECT * FROM {tableName} WHERE SecurityID IN ('{benchmarksString}')")

    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)
    df <- sqlQuery(dbhandle, queryString)
    odbcClose(dbhandle)

    if(nrow(df) > 0) {
        df <- df %>% as_tibble() %>%
            tidyr::spread(SecurityID, MonthlyReturn) %>%
            mutate(MonthlyDate = as.Date(as.character(MonthlyDate))) %>%
            dplyr::rename(Date = MonthlyDate) %>%
            select(one_of(c("Date", vBenchmarks)))
    }

    if(removeNAs) df <- df %>% drop_na()
    return(df)
}


# Read Equities Data
dbReadEquitiesData <- function(vEquities, tableName = "EquitiesData", removeNAs = T) {
    equitiesString <- paste(vEquities, collapse = "','")
    queryString <- glue::glue("SELECT SecurityID, DailyDate, DailyReturn FROM {tableName} WHERE SecurityID IN ('{equitiesString}')")

    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)
    df <- sqlQuery(dbhandle, queryString, stringsAsFactors = F)
    odbcClose(dbhandle)

    if(nrow(df) > 0) {
        df <- df %>% as_tibble() %>%
            tidyr::spread(SecurityID, DailyReturn) %>%
            mutate(DailyDate = as.Date(as.character(DailyDate))) %>%
            dplyr::rename(Date = DailyDate) %>%
            select(one_of(c("Date", vEquities)))
    }

    if(removeNAs) df <- df %>% drop_na()
    return(df)
}


# Read Query Data
dbReadQueryData <- function(queryString) {
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)
    df <- sqlQuery(dbhandle, queryString, stringsAsFactors = F)
    odbcClose(dbhandle)
    return(df)
}


# Read Equities Universe
dbReadEquitiesUniverse <- function(strategyID, univID) {
    univQueryString <- paste0("EXEC usp_GetEquitiesUniverse '", strategyID, "','", univID, "'")
    dfUniverse <- dbReadQueryData(univQueryString)
    return(dfUniverse)
}


# Read IndexRawData
dbReadIndexRawData <- function(vSecurities, tableName = "IndexRawData", removeNAs = T) {
    securitiesString <- paste(vSecurities, collapse = "','")
    queryString <- glue::glue("SELECT * FROM {tableName} WHERE SecurityID IN ('{securitiesString}')")

    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)
    df <- sqlQuery(dbhandle, queryString)
    odbcClose(dbhandle)

    if(nrow(df) > 0) {
        df <- df %>% as_tibble() %>%
            tidyr::spread(SecurityID, IndexValue) %>%
            mutate(IndexDate = as.Date(as.character(IndexDate))) %>%
            dplyr::rename(Date = IndexDate) %>%
            select(one_of(c("Date", vSecurities)))
    }

    if(removeNAs) df <- df %>% drop_na()
    return(df)
}


# Read Futures Data
dbReadFuturesData <- function(secID, tableName = "FuturesData", removeNAs = T) {
    queryString <- glue::glue("SELECT * FROM {tableName} WHERE SecurityID = '{secID}'")

    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)
    df <- sqlQuery(dbhandle, queryString, stringsAsFactors = F)
    odbcClose(dbhandle)

    if(nrow(df) > 0) {
        df <- df %>% as_tibble() %>%
            mutate(DailyDate = as.Date(as.character(DailyDate))) %>%
            rename(Date = DailyDate)
    }

    if(removeNAs) df <- df %>% drop_na()
    return(df)
}


# Read Futures Universe
dbReadFuturesUniverse <- function(univID) {
    univQueryString <- paste0("EXEC usp_GetFuturesUniverse '", univID, "'")
    dfUniverse <- dbReadQueryData(univQueryString)
    return(dfUniverse)
}


# Read FXRates Data
dbReadFXRatesData <- function(vSecurities, tableName = "FXRatesData") {
    securitiesString <- paste(vSecurities, collapse = "','")
    queryString <- glue::glue("SELECT SecurityID, DailyDate, DailyFXRate FROM {tableName} WHERE SecurityID IN ('{securitiesString}')")

    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)
    df <- sqlQuery(dbhandle, queryString, stringsAsFactors = F)
    odbcClose(dbhandle)

    if(nrow(df) > 0) {
        df <- df %>%
            as_tibble() %>%
            tidyr::spread(SecurityID, DailyFXRate) %>%
            drop_na() %>%
            mutate(DailyDate = as.Date(as.character(DailyDate))) %>%
            rename(Date = DailyDate) %>%
            select(one_of(c("Date", vSecurities)))
    }
    return(df)
}


# Read RiskFreeRates Data
dbReadRiskFreeRatesData <- function(vSecurities, tableName = "RiskFreeRatesData") {
    securitiesString <- paste(vSecurities, collapse = "','")
    queryString <- glue::glue("SELECT SecurityID, DailyDate, DailyReturn FROM {tableName} WHERE SecurityID IN ('{securitiesString}')")

    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)
    df <- sqlQuery(dbhandle, queryString, stringsAsFactors = F)
    odbcClose(dbhandle)

    if(nrow(df) > 0) {
        df <- df %>%
            as_tibble() %>%
            tidyr::spread(SecurityID, DailyReturn) %>%
            drop_na() %>%
            mutate(DailyDate = as.Date(as.character(DailyDate))) %>%
            rename(Date = DailyDate) %>%
            select(one_of(c("Date", vSecurities)))
    }
    return(df)
}


# Read Portfolio Returns

dbReadPortfolioReturns <- function(dfStrategies) {
    vPrimaryKeyHeaders <- c("StrategyID", "UniverseID", "ParamCombID", "RebalanceFreq", "WeightMethod", "LevLong", "LevShort")
    colnames(dfStrategies) <- vPrimaryKeyHeaders

    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    dfMonthly <- NULL
    for(i in 1:nrow(dfStrategies)) {
        strategyID <- dfStrategies$StrategyID[i]
        univID <- dfStrategies$UniverseID[i]
        paramID <- dfStrategies$ParamCombID[i]
        rebalFreq <- dfStrategies$RebalanceFreq[i]
        wgtMethod <- dfStrategies$WeightMethod[i]
        levLongString <- dfStrategies$LevLong[i]
        levShortString <- dfStrategies$LevShort[i]

        secReturnsID <- paste(univID, paramID, wgtMethod, paste(levLongString, levShortString, sep = "_"), sep = ".")

        queryString <- paste0("SELECT PortfolioDate, SecurityReturn FROM StrategiesPortfolioData WHERE UPPER(StrategyID) = '",
                              toupper(strategyID),
                              "' AND UPPER(UniverseID) = '", toupper(univID),
                              "' AND UPPER(ParamCombID) = '", toupper(paramID),
                              "' AND UPPER(RebalanceFreq) = '", toupper(rebalFreq),
                              "' AND UPPER(WeightMethod) = '", toupper(wgtMethod),
                              "' AND UPPER(LevLong) = '", toupper(levLongString),
                              "' AND UPPER(LevShort) = '", toupper(levShortString),
                              "' AND UPPER(SecurityID) = 'PORTFOLIO'")

        df <- sqlQuery(dbhandle, queryString, stringsAsFactors = F)

        if(nrow(df) > 0) {
            df <- df %>%
                set_names(c("Date", secReturnsID)) %>%
                getMonthlyReturns(.)

            if(is.null(dfMonthly)) dfMonthly <- df
            else dfMonthly <- plyr::join(dfMonthly, df, by = "Date", type = "inner")
        }
    }

    odbcClose(dbhandle)

    if(nrow(dfMonthly) > 0) colnames(dfMonthly)[1] <- "Date"
    return(dfMonthly)
}


# Read Portfolio Weights
dbReadPortfolioWeights <- function(vStrategyParam, longOrShortSignal = c("LONG","SHORT")) {
    vPrimaryKeyHeaders <- c("StrategyID", "UniverseID", "ParamCombID", "RebalanceFreq", "WeightMethod")
    names(vStrategyParam) <- vPrimaryKeyHeaders

    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    strategyID <- vStrategyParam["StrategyID"]
    univID <- vStrategyParam["UniverseID"]
    paramID <- vStrategyParam["ParamCombID"]
    rebalFreq <- vStrategyParam["RebalanceFreq"]
    wgtMethod <- vStrategyParam["WeightMethod"]

    queryString <- paste0("SELECT RebalanceDate, SecurityID, Weight FROM StrategiesRebalanceData WHERE UPPER(StrategyID) = '", toupper(strategyID),
                          "' AND UPPER(UniverseID) = '", toupper(univID),
                          "' AND UPPER(ParamCombID) = '", toupper(paramID),
                          "' AND UPPER(RebalanceFreq) = '", toupper(rebalFreq),
                          "' AND UPPER(WeightMethod) = '", toupper(wgtMethod),
                          "'")
    if(longOrShortSignal[1]=="LONG") queryString <- paste(queryString, "AND Signal >= 0")
    else queryString <- paste(queryString, "AND Signal <= 0")

    dfWeights <- sqlQuery(dbhandle, queryString, stringsAsFactors = F)
    colnames(dfWeights) <- c("Date", "Security", "Weight")

    odbcClose(dbhandle)
    return(dfWeights)
}


# Read Strategies Returns
dbReadStrategiesReturns <- function(dfStrategies, removeNAs = T) {
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    dfMonthly <- NULL
    for(i in 1:nrow(dfStrategies)) {
        strategyID <- dfStrategies$StrategyID[i]
        univID <- dfStrategies$UniverseID[i]
        wgtMethod <- dfStrategies$WeightMethod[i]
        identifier <- dfStrategies$Identifier[i]

        queryString <- paste0("SELECT PortfolioDate, PortfolioReturn FROM StrategiesReturns WHERE UPPER(StrategyID) = '", toupper(strategyID),
                              "' AND UPPER(UniverseID) = '", toupper(univID),
                              "' AND UPPER(WeightMethod) = '", toupper(wgtMethod),
                              "'")

        df <- sqlQuery(dbhandle, queryString, stringsAsFactors = F)

        if(nrow(df) > 0) {
            df <- df %>%
                set_names(c("Date", identifier)) %>%
                getMonthlyReturns(.)

            if(is.null(dfMonthly)) dfMonthly <- df
            else dfMonthly <- plyr::join(dfMonthly, df, by = "Date", type = "full")
        }
    }

    odbcClose(dbhandle)

    if(nrow(dfMonthly) > 0) {
        colnames(dfMonthly)[1] <- "Date"
        dfMonthly <- dfMonthly %>% as_tibble() %>% arrange(Date)
        if(removeNAs) dfMonthly <- dfMonthly %>% drop_na()
    }
    return(dfMonthly)
}


# Read Strategies Weights
dbReadStrategiesWeights <- function(dfStrategies) {
    vPrimaryKeyHeaders <- c("StrategyID", "UniverseID","WeightMethod")
    colnames(dfStrategies) <- vPrimaryKeyHeaders

    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    dfMonthly <- NULL
    for(i in 1:nrow(dfStrategies)) {
        strategyID <- dfStrategies$StrategyID[i]
        univID <- dfStrategies$UniverseID[i]
        wgtMethod <- dfStrategies$WeightMethod[i]

        queryString <- paste0("SELECT RebalanceDate, SecurityID, Weight FROM StrategiesWeights WHERE UPPER(StrategyID) = '", toupper(strategyID),
                              "' AND UPPER(UniverseID) = '", toupper(univID),
                              "' AND UPPER(WeightMethod) = '", toupper(wgtMethod),
                              "'")

        df <- sqlQuery(dbhandle, queryString, stringsAsFactors = F)
        if(nrow(df) > 0) {
            df <- df %>%
                select(RebalanceDate, SecurityID, Weight) %>%
                dplyr::rename(Date = RebalanceDate)
        }
    }

    odbcClose(dbhandle)
    return(df)
}
