
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
        df <- df %>%
            tidyr::spread(SecurityID, MonthlyReturn) %>%
            mutate(Date = as.Date(as.character(MonthlyDate))) %>%
            arrange(Date) %>%
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
        df <- df %>%
            tidyr::spread(SecurityID, DailyReturn) %>%
            mutate(Date = as.Date(as.character(DailyDate))) %>%
            arrange(Date) %>%
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
dbReadIndexRawData <- function(vSecurities, tableName = "IndexRawData") {
    securitiesString <- paste(vSecurities, collapse = "','")
    queryString <- glue::glue("SELECT * FROM {tableName} WHERE SecurityID IN ('{securitiesString}')")

    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)
    df <- sqlQuery(dbhandle, queryString)
    odbcClose(dbhandle)

    if(nrow(df) > 0) {
        df <- df %>%
            tidyr::spread(SecurityID, IndexValue) %>%
            mutate(Date = as.Date(as.character(IndexDate))) %>%
            select(one_of(c("Date", vSecurities))) %>%
            arrange(Date) %>%
            filter(!(if_all(all_of(vSecurities), ~ is.na(.x)))) %>%
            as_tibble()
    }
    return(df)
}

# Convert IndexRawData to returns
dbReadIndexReturns <- function(vSecurities, tableName = "IndexRawData", startDate = "1920-01-01", endDate = "2100-01-01") {
    df <- dbReadIndexRawData(vSecurities, tableName = "IndexRawData")

    if(nrow(df) > 0) {
        df <- df %>%
            filter(Date >= startDate & Date <= endDate) %>%
            tidyr::gather(SecurityID, IndexValue, -Date) %>%
            drop_na(IndexValue) %>%
            group_by(SecurityID) %>%
            arrange(Date) %>%
            tq_mutate(select = IndexValue, mutate_fun = Delt, col_rename = "Return") %>%
            select(-IndexValue) %>%
            tidyr::spread(SecurityID, Return) %>%
            select(one_of(c("Date", vSecurities))) %>%
            filter(!(if_all(all_of(vSecurities), ~ is.na(.x)))) %>%
            as_tibble()
    }

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
        df <- df %>%
            mutate(DailyDate = as.Date(as.character(DailyDate))) %>%
            rename(Date = DailyDate) %>%
            arrange(Date) %>%
            as_tibble()
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
            tidyr::spread(SecurityID, DailyFXRate) %>%
            drop_na() %>%
            mutate(Date = as.Date(as.character(DailyDate))) %>%
            select(one_of(c("Date", vSecurities))) %>%
            arrange(Date) %>%
            as_tibble()
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
            tidyr::spread(SecurityID, DailyReturn) %>%
            drop_na() %>%
            mutate(Date = as.Date(as.character(DailyDate))) %>%
            select(one_of(c("Date", vSecurities))) %>%
            arrange(Date) %>%
            as_tibble()
    }
    return(df)
}



# Read Strategies Returns
dbReadStrategiesReturns <- function(dfStrategies, removeNAs = T, monthlyReturns = T) {
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    dfReturns <- NULL
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
            df <- df %>% set_names(c("Date", identifier))

            if(is.null(dfReturns)) dfReturns <- df
            else dfReturns <- dfReturns %>% full_join(df, by = "Date") %>% arrange(Date)
        }
    }

    odbcClose(dbhandle)

    if(nrow(dfReturns) > 0) {
        dfReturns <- dfReturns %>% arrange(Date) %>% mutate(Date = as.Date(Date))
        if(monthlyReturns) {
            dfReturns <- dfReturns %>% getMonthlyReturns(.)
            if(removeNAs) dfReturns <- dfReturns %>% drop_na()
        } else {
            dfReturns <- dfReturns %>% filter(!(if_all(where(is_bare_double), ~ is.na(.x)))) # remove rows where all values except Date are NAs
        }
    }
    return(dfReturns)
}


# Read Strategies Weights
dbReadStrategiesWeights <- function(dfStrategies) {
    vPrimaryKeyHeaders <- c("StrategyID", "UniverseID","WeightMethod")
    colnames(dfStrategies) <- vPrimaryKeyHeaders

    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

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
                select(Date = RebalanceDate, SecurityID, Weight) %>%
                arrange(Date)
        }
    }

    odbcClose(dbhandle)
    return(df)
}
