
# write Strategies returns data to DB
dbWriteStrategiesReturns <- function(df, tableName = "StrategiesReturns") {
    # set colnames as in database table
    vTblHeader <- c("StrategyID","UniverseID","WeightMethod","PortfolioDate","PortfolioReturn")
    colnames(df) <- vTblHeader

    # cast variables to datatypes as in database table
    df <- df %>%
        mutate(StrategyID = as.character(StrategyID),
               UniverseID = as.character(UniverseID),
               WeightMethod = as.character(WeightMethod),
               PortfolioDate = as.Date(as.character(PortfolioDate)),
               PortfolioReturn = as.numeric(as.character(PortfolioReturn)))

    # Write monthly returns to DB
    print(glue::glue("Updating {nrow(df)} rows in {tableName}"))

    # Open a connection to DB
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    # Create a temporary DB table
    colTypes <- c("varchar(20)","varchar(20)","varchar(5)","date","decimal(10,8)")
    names(colTypes) <- vTblHeader

    sqlSave(dbhandle, df, tablename = "tempTbl", rownames = F, safer = T, varTypes = colTypes, nastring = "NULL")

    # MERGE data in DB table
    queryString <- "EXEC usp_UpdateStrategiesReturns;"
    sqlQuery(dbhandle, query = queryString)

    # drop tempTbl table
    sqlDrop(dbhandle, "tempTbl")

    # Close DB connection
    odbcClose(dbhandle)
}


# write Strategies Weights to DB
dbWriteStrategiesWeights <- function(df, tableName = "StrategiesWeights") {
    # set colnames as in database table
    vTblHeader <- c("StrategyID","UniverseID","WeightMethod","SignalDate","RebalanceDate","SecurityID","Weight")
    colnames(df) <- vTblHeader

    # cast variables to datatypes as in database table
    df <- df %>%
        mutate(StrategyID = as.character(StrategyID),
               UniverseID = as.character(UniverseID),
               WeightMethod = as.character(WeightMethod),
               SignalDate = as.Date(as.character(SignalDate)),
               RebalanceDate = as.Date(as.character(RebalanceDate)),
               SecurityID = as.character(SecurityID),
               Weight = as.numeric(as.character(Weight)))

    # Write monthly returns to DB
    print(glue::glue("Updating {nrow(df)} rows in {tableName}"))

    # Open a connection to DB
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    # Create a temporary DB table
    colTypes <- c("varchar(20)","varchar(20)","varchar(5)","date","date","varchar(10)","decimal(5,4)")
    names(colTypes) <- vTblHeader

    sqlSave(dbhandle, df, tablename = "tempTbl", rownames = F, safer = T, varTypes = colTypes, nastring = "NULL")

    # MERGE data in DB table
    queryString <- "EXEC usp_UpdateStrategiesWeights;"
    sqlQuery(dbhandle, query = queryString)

    # drop tempTbl table
    sqlDrop(dbhandle, "tempTbl")

    # Close DB connection
    odbcClose(dbhandle)
}


# write Benchmarks data
dbWriteBenchmarksData <- function(df, tableName = "BenchmarksData") {
    # set colnames as in database table
    vTblHeader <- c("SecurityID","MonthlyDate","MonthlyReturn")
    colnames(df) <- vTblHeader

    # cast variables to datatypes as in database table
    df <- df %>%
        mutate(SecurityID = as.character(SecurityID),
               MonthlyDate = as.Date(as.character(MonthlyDate)),
               MonthlyReturn = as.numeric(as.character(MonthlyReturn)))

    # Write monthly returns to database
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    print(paste("Merging", nrow(df), "rows to", tableName))

    # Create a temporary DB table
    colTypes <- c("varchar(10)","date","decimal(11,8)")
    names(colTypes) <- c("SecurityID","MonthlyDate","MonthlyReturn")

    sqlSave(dbhandle, df, tablename = "tempTbl", rownames = F, safer = T, test = F, verbose = F, varTypes = colTypes)

    # MERGE data in DB table
    queryString <- "EXEC usp_MergeBenchmarksData;"
    sqlQuery(dbhandle, query = queryString)

    # drop tempTbl table
    sqlDrop(dbhandle, "tempTbl")

    # Close database connection
    odbcClose(dbhandle)
}


# write Equities data
dbWriteEquitiesData <- function(df, tableName = "EquitiesData") {
    # set colnames as in database table
    vTblHeader <- c("SecurityID","DailyDate","DailyReturn","PriceOpen","PriceHigh",
                    "PriceLow","PriceClose","Volume","PriceAdjClose")
    colnames(df) <- vTblHeader

    # cast variables to datatypes as in database table
    df <- df %>%
        mutate(SecurityID = as.character(SecurityID),
               DailyDate = as.Date(as.character(DailyDate)),
               DailyReturn = as.numeric(as.character(DailyReturn)),
               PriceOpen = as.numeric(as.character(PriceOpen)),
               PriceHigh = as.numeric(as.character(PriceHigh)),
               PriceLow = as.numeric(as.character(PriceLow)),
               PriceClose = as.numeric(as.character(PriceClose)),
               Volume = as.numeric(as.character(Volume)),
               PriceAdjClose = as.numeric(as.character(PriceAdjClose)))

    # Write daily returns to database
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    print(paste("Merging", nrow(df), "records to", tableName))

    # Create a temporary DB table
    colTypes <- c("varchar(10)", "date", "decimal(10,8)", "decimal(15,8)", "decimal(15,8)",
                  "decimal(15,8)", "decimal(15,8)", "bigint", "decimal(15,8)")
    names(colTypes) <- c("SecurityID", "DailyDate", "DailyReturn", "PriceOpen", "PriceHigh",
                         "PriceLow", "PriceClose", "Volume", "PriceAdjClose")

    # Create a temporary table
    sqlSave(dbhandle, df, tablename = "tempTbl", rownames = F, safer = T, verbose = F, varTypes = colTypes)

    # MERGE data in DB table
    queryString <- "EXEC usp_MergeEquitiesData;"
    # send Query to DB
    sqlQuery(dbhandle, query = queryString)

    # drop tempTbl table
    sqlDrop(dbhandle, "tempTbl")

    # Close database connection
    odbcClose(dbhandle)
}


# write Risk Free Rates Data
dbWriteRiskFreeRatesData <- function(df, tableName = "RiskFreeRatesData") {
    # set colnames as in database table
    vTblHeader <- c("SecurityID","DailyDate","DailyReturn","AnnualReturn")
    colnames(df) <- vTblHeader

    # cast variables to datatypes as in database table
    df <- df %>%
        mutate(SecurityID = as.character(SecurityID),
               DailyDate = as.Date(as.character(DailyDate)),
               DailyReturn = as.numeric(as.character(DailyReturn)))

    # Write daily returns to database
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    print(paste("Merging", nrow(df), "rows to", tableName))

    # Create a temporary table
    colTypes <- c("varchar(10)","date","decimal(13,12)","decimal(6,4)")
    names(colTypes) <- c("SecurityID","DailyDate","DailyReturn","AnnualReturn")

    sqlSave(dbhandle, df, tablename = "tempTbl", rownames = F, safer = T, test = F, verbose = F, varTypes = colTypes)

    # MERGE data in table
    queryString <- "EXEC usp_MergeRiskFreeRatesData;"
    sqlQuery(dbhandle, query = queryString)

    # drop tempTbl table
    sqlDrop(dbhandle, "tempTbl")

    # Close database connection
    odbcClose(dbhandle)
}


# write FX Rates Data
dbWriteFXRatesData <- function(df, tableName = "FXRatesData") {
    # set colnames as in database table
    vTblHeader <- c("SecurityID","DailyDate","DailyFXRate")
    colnames(df) <- vTblHeader

    # cast variables to datatypes as in database table
    df <- df %>%
        mutate(SecurityID = as.character(SecurityID),
               DailyDate = as.Date(as.character(DailyDate)),
               DailyFXRate = as.numeric(as.character(DailyFXRate)))

    # Write daily fx rates to database
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    print(paste("Merging", nrow(df), "rows to", tableName))

    # Create a temporary table
    colTypes <- c("varchar(10)","date","decimal(10,4)")
    names(colTypes) <- c("SecurityID","DailyDate","DailyFXRate")

    sqlSave(dbhandle, df, tablename = "tempTbl", rownames = F, safer = T, test = F, verbose = F, varTypes = colTypes)

    # MERGE data in table
    queryString <- "EXEC usp_MergeFXRatesData;"

    # Execute query
    sqlQuery(dbhandle, query = queryString)

    # drop tempTbl table
    sqlDrop(dbhandle, "tempTbl")

    # Close database connection
    odbcClose(dbhandle)
}


# write Index Raw Data
dbWriteIndexRawData <- function(df, tableName = "IndexRawData") {
    # set colnames as in database table
    vTblHeader <- c("SecurityID","IndexDate","IndexValue")
    colnames(df) <- vTblHeader

    # cast variables to datatypes as in database table
    df <- df %>%
        mutate(SecurityID = as.character(SecurityID),
               IndexDate = as.Date(as.character(IndexDate)),
               IndexValue = as.numeric(as.character(IndexValue)))

    # Write index values to database
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    print(paste("Merging", nrow(df), "rows to", tableName))

    # Create a temporary DB table
    colTypes <- c("varchar(10)","date","decimal(11,4)")
    names(colTypes) <- c("SecurityID","IndexDate","IndexValue")

    sqlSave(dbhandle, df, tablename = "tempTbl", rownames = F, safer = T, test = F, verbose = F, varTypes = colTypes)

    # MERGE data in DB table
    queryString <- "EXEC usp_MergeIndexRawData;"
    sqlQuery(dbhandle, query = queryString)

    # drop tempTbl table
    sqlDrop(dbhandle, "tempTbl")

    # Close database connection
    odbcClose(dbhandle)
}


# write Capital Gains Data
dbWriteCapitalGainsData <- function(df, tableName = "CapitalGainsData") {
    # set colnames as in database table
    vTblHeader <- c("SecurityID","DailyDate","CapitalGain")
    colnames(df) <- vTblHeader

    # cast variables to datatypes as in database table
    df <- df %>%
        mutate(SecurityID = as.character(SecurityID),
               DailyDate = as.Date(as.character(DailyDate)),
               CapitalGain = as.numeric(as.character(CapitalGain)))

    # Write index values to database
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    print(paste("Merging", nrow(df), "rows to", tableName))

    # Create a temporary DB table
    colTypes <- c("varchar(10)","date","decimal(11,8)")
    names(colTypes) <- c("SecurityID","DailyDate","CapitalGain")

    sqlSave(dbhandle, df, tablename = "tempTbl", rownames = F, safer = T, test = F, verbose = F, varTypes = colTypes)

    # MERGE data in DB table
    queryString <- "EXEC usp_MergeCapitalGainsData;"
    sqlQuery(dbhandle, query = queryString)

    # drop tempTbl table
    sqlDrop(dbhandle, "tempTbl")

    # Close database connection
    odbcClose(dbhandle)
}


# write Futures data
dbWriteFuturesData <- function(dfFut, secID, sectorID, tableName = "FuturesData") {

    d <- dfFut$Date ; dt <- as.Date(d)
    h <- as.numeric(dfFut$High) ; l <- as.numeric(dfFut$Low)
    s <- as.numeric(dfFut$Close) ; s1 <- c(NaN, s[-length(s)])
    vol <- as.numeric(dfFut$Volume) ; oi <- as.numeric(dfFut$Open.Interest)

    if(sectorID=="RATE") {
        rs <- (s-s1)/100 ; rs <- replace(rs, 1, 0)
        ih <- h ; il <- l ; ic <- s
    }
    else {
        rs <- (s/s1 - 1) ; rs <- replace(rs, 1, 0) ; prs <- rs*100 ; ic <- cumsum(prs)
        ic1 <- c(0, ic[-length(ic)])
        rh <- (h/s1 - 1) ; rh <- replace(rh, 1, 0) ; prh <- rh*100 ; ih <- prh+ic1
        rl <- (l/s1 - 1) ; rl <- replace(rl, 1, 0) ; prl <- rl*100 ; il <- prl+ic1
    }

    tr <- 1L + rs

    # compute daily returns
    df <- dfFut %>%
        unite("Contract", Symbol, Numeric.Delivery.Month, sep="_") %>%
        mutate(SecurityID = secID,
               Contract = replaceByMonthChar(Contract),
               DailyDate = dt,
               DailyReturn = getSignificantDigits(rs),
               IndexHigh = getSignificantDigits(ih),
               IndexLow = getSignificantDigits(il),
               IndexClose = getSignificantDigits(ic),
               Volume = vol,
               OpenInterest = oi) %>%
        select(SecurityID, Contract, DailyDate, DailyReturn, IndexHigh, IndexLow, IndexClose, Volume, OpenInterest)

    # Write daily returns to database
    connectionString <- getDatabaseConnectionString()
    dbhandle <- odbcDriverConnect(connectionString)

    print(paste("Merging", nrow(df), "records of", secID, "to", tableName))

    # Create a temporary DB table
    colTypes <- c("varchar(10)", "varchar(10)", "date", "decimal(10,8)",
                  "decimal(15,8)", "decimal(15,8)", "decimal(15,8)", "int", "int")
    names(colTypes) <- c("SecurityID", "Contract", "DailyDate", "DailyReturn",
                         "IndexHigh", "IndexLow", "IndexClose", "Volume", "OpenInterest")

    # Create a temporary table
    sqlSave(dbhandle, df, tablename = "tempTbl", rownames = F, safer = T, verbose = F, varTypes = colTypes)

    # MERGE data in DB table
    queryString <- "EXEC usp_MergeFuturesData;"
    # send Query to DB
    sqlQuery(dbhandle, query = queryString)

    # drop tempTbl table
    sqlDrop(dbhandle, "tempTbl")

    # Close database connection
    odbcClose(dbhandle)
}
