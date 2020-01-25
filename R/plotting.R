
# create a ggplot of cumulative returns and drawdowns
plotPerformance <- function(dfReturns, dfRecessions=NULL, plotMainTitle=NA, plotCtr=NA, vPalette = NA,
                            putRecessionsCaptions = TRUE, excludeDrawdowns = FALSE, legendPosition = NULL) {
    # PRE: dfReturns has Date in row names and daily/monthly returns in columns by security/portfolio
    # POST: converts daily returns into cumulative returns and plots on a log scale

    # exclude incomplete rows
    dfReturns <- dfReturns %>% drop_na()
    vStrategyNames <- names(dfReturns)[-1]
    vStrategyNames <- gsub(" ", ".", vStrategyNames)

    firstPerfDate <- as.Date(first(dfReturns$Date))
    lastPerfDate <- as.Date(last(dfReturns$Date))

    # determine how far apart the year labels will be placed
    dateBreaks <- getDateBreaks(dfReturns$Date)

    # set plot title
    plotTitle <- paste0("\n ", format(firstPerfDate,"%b %Y")," - ",format(lastPerfDate,"%b %Y"))
    if(!is.na(plotMainTitle)) {
        plotTitle <- paste(plotMainTitle, plotTitle)
    } else {
        plotTitle <- paste("Performance", plotTitle)
    }
    if(!is.na(plotCtr)) plotTitle <- paste("Plot", plotCtr, ":", plotTitle)

    # set plot caption
    captionTitle <- "Note: Grey shaded areas indicate Recessions"
    if(is.null(dfRecessions) | !putRecessionsCaptions) captionTitle <- NULL

    # set legend position
    if(is.null(legendPosition)) legendPosition <- c(0,1)

    # calculate cumulative returns
    dfCumReturns <- dfReturns %>%
        mutate_if(is_bare_double, function(Z) exp(cumsum(log(1 + Z)))) %>%
        mutate(Date = as.Date(Date)) %>%
        gather(Security, CumReturn, -Date) %>%
        mutate(Security = factor(Security, levels = vStrategyNames)) %>%
        arrange(Date)

    # compute drawdowns
    dfDrawdowns <- dfReturns %>%
        mutate_if(is_bare_double, function(Z) suppressWarnings(as.vector(timeSeries::drawdowns(timeSeries::as.timeSeries(Z))))) %>%
        mutate(Date = as.Date(Date)) %>%
        gather(Security, Drawdown, -Date) %>%
        mutate(Drawdown = as.numeric(Drawdown)) %>%
        mutate(Security = factor(Security, levels = vStrategyNames)) %>%
        arrange(Date)

    # plot cumulative returns
    plotPerf <- ggplot(dfCumReturns)

    # add Recession bands
    if(!is.null(dfRecessions)) {
        plotPerf <- plotAddRecessions(plotPerf, dfReturns, dfRecessions)
    }

    plotPerf <- plotPerf +
        geom_line(aes(x=Date, y=CumReturn, group=Security, color=Security), size=0.7) +
        scale_x_date(breaks=dateBreaks,date_minor_breaks="1 year",labels=date_format("%Y"),expand=c(0.05, 0)) +
        scale_y_log10(breaks=trans_breaks("log2", function(x) 2^x),
                      labels=trans_format("log2", function(x) format(2^x, digits=2))) +
        labs(title = plotTitle,
             y = "Cumulative Return") +
        annotate("text",x=lastPerfDate,y=0,label=ifelse(!excludeDrawdowns, sprintf("Copyright \U00A9 Nitin Gupta"), ""),
                 hjust=1,vjust=-1.5,col="black",cex=2.5,fontface="bold", alpha=0.4) +
        theme_light() +
        theme(axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_text(face="bold"),
              axis.text.y=element_text(hjust = 1.0)) +
        theme(plot.title=element_text(hjust = 0.5, vjust = 1, size=12)) +
        theme(legend.title=element_blank(),
              legend.text=element_text(face="bold", size=8),
              legend.key=element_rect(color="white"),
              legend.background=element_rect(color="gray", size=0.3, linetype="solid"),
              legend.justification=c(0,1),
              legend.position=legendPosition,
              legend.spacing=unit(0,"cm"))

    if(is.na(vPalette)) {
        plotPerf <- plotPerf + scale_color_quant_manual(palette = "withgrey")
    } else {
        plotPerf <- plotPerf + scale_color_manual(values = vPalette)
    }

    # plot drawdowns
    plotDD <- ggplot(dfDrawdowns)

    # add Recession bands
    if(!is.null(dfRecessions)) {
        plotDD <- plotAddRecessions(plotDD, dfReturns, dfRecessions, ymin = -Inf, ymax = 0)
    }

    plotDD <- plotDD +
        geom_line(aes(x=Date, y=Drawdown, group=Security, color=Security), size=0.7) +
        scale_x_date(breaks=dateBreaks,date_minor_breaks="1 year",labels=date_format("%Y"),expand=c(0.05, 0)) +
        scale_y_continuous(labels=percent_format(accuracy = 1), expand = c(0, 0)) +
        labs(y = "Drawdown", caption = captionTitle) +
        theme_light() +
        theme(axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_text(face="bold"),
              axis.text.y=element_text(hjust = 1.0),
              legend.position="none",
              plot.caption=element_text(size=rel(0.65)))

    if(is.na(vPalette)) {
        plotDD <- plotDD + scale_color_quant_manual(palette = "withgrey")
    } else {
        plotDD <- plotDD + scale_color_manual(values = vPalette)
    }

    # combine the two plots
    if(excludeDrawdowns) {
        return(plotPerf)
    } else {
        return(plot_grid(plotPerf, plotDD, nrow=2, align = "v", rel_heights = c(2,1)))
    }
}


# add Recession bands to ggplot
plotAddRecessions <- function(plotPerf, dfReturns, dfRecessions, ymin = 0, ymax = Inf) {
    dfRect <- getRecessionIntervals(dfReturns, dfRecessions)

    if(nrow(dfRect) > 0) {
        plotPerf <- plotPerf +
            geom_rect(data = dfRect, aes(xmin = date_from, xmax = date_to, ymin = ymin, ymax = ymax),
                      color = "lightgrey", alpha = 0.15, inherit.aes = F)
    }
    plotPerf
}


#################################################################################################
# Plot Rolling Excess Returns
plotRollingExcessReturns <- function(dfReturns, dfRecessions=NULL, coreStrategyName, baseBenchmarkName, rollingMonths, plotMainTitle=NA, plotCtr=NA) {
    dfExcessReturns <- getRollingAnnualizedReturns(dfReturns, rollingMonths) %>%
        mutate(ExcessReturn = !!sym(coreStrategyName) - !!sym(baseBenchmarkName))

    # determine first and last dates
    firstPerfDate <- as.Date(first(dfExcessReturns$Date))
    lastPerfDate <- as.Date(last(dfExcessReturns$Date))

    # determine how far apart the year labels will be placed
    dateBreaks <- getDateBreaks(dfExcessReturns$Date)

    # determine title text
    plotTitle <- paste0("\n ", format(firstPerfDate,"%b %Y")," - ",format(lastPerfDate,"%b %Y"))
    if(!is.na(plotMainTitle)) {
        plotTitle <- paste(plotMainTitle, plotTitle)
    } else {
        plotTitle <- paste("Rolling Annualized Performance", plotTitle)
    }

    if(!is.na(plotCtr)) plotTitle <- paste("Plot", plotCtr, ":", plotTitle)

    # plot rolling excess return
    plotExcessReturn <- ggplot(dfExcessReturns)

    if(!is.null(dfRecessions)) {
        plotExcessReturn <- plotAddRecessions(plotExcessReturn, dfExcessReturns, dfRecessions, ymin = -Inf, ymax = Inf)
    }

    plotExcessReturn <- plotExcessReturn +
        geom_bar(aes(x=Date, y=ExcessReturn, fill=(ExcessReturn < 0)), stat="identity") +
        scale_fill_quant_manual(palette = "pnl") +
        scale_x_date(breaks=dateBreaks,date_minor_breaks="1 year",labels=date_format("%Y"),expand=c(0.05, 0)) +
        scale_y_continuous(labels=percent_format(accuracy = 1)) +
        labs(title = plotTitle, y = "Excess Return") +
        annotate("text",x=lastPerfDate,y=0,label=sprintf("Copyright \U00A9 Nitin Gupta"),
                 hjust=1,vjust=-1.5,col="black",cex=2.5,fontface="bold", alpha=0.4) +
        theme_light() +
        theme(axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_text(face="bold"),
              axis.text.y=element_text(hjust = 1.0)) +
        theme(plot.title=element_text(hjust = 0.5, vjust = 1, size=12),
              plot.caption=element_text(size=rel(0.65))) +
        theme(legend.position="none")

    return(plotExcessReturn)
}


###############################################################################
# Plot Portfolio Weights
plotPortfolioWeights <- function(dfWeights, plotMainTitle = NA, plotCtr = NA) {
    vSecurities <- unique(dfWeights$SecurityID)

    # Determine Cash Weights
    dfWeightsFinal <- dfWeights %>%
        dplyr::filter(SecurityID != "Cash") %>%
        mutate(Date = as.Date(as.character(Date)),
               Weight = as.numeric(as.character(Weight))) %>%
        spread(SecurityID, Weight) %>%
        mutate(Cash = 1 - rowSums(.[-1], na.rm = T)) %>%
        gather(SecurityID, Weight, -Date) %>%
        arrange(Date, SecurityID)

    # Find unique security names and put Cash at the end
    vSecurities <- unique(dfWeightsFinal$SecurityID)
    vSecurities <- c(vSecurities[-which(vSecurities == "Cash")], "Cash")
    numSecurities <- length(vSecurities)

    firstPerfDate <- as.Date(min(dfWeightsFinal$Date))
    lastPerfDate <- as.Date(max(dfWeightsFinal$Date))

    # determine how far apart the year labels will be placed
    dateBreaks <- getDateBreaks(dfWeightsFinal$Date)

    # set plot title
    plotTitle <- paste0("\n ", format(firstPerfDate,"%b %Y")," - ", format(lastPerfDate,"%b %Y"))
    if(!is.na(plotMainTitle)) {
        plotTitle <- paste(plotMainTitle, plotTitle)
    } else {
        plotTitle <- paste("Portfolio Weights", plotTitle)
    }

    if(!is.na(plotCtr)) plotTitle <- paste("Plot", plotCtr, ":", plotTitle)

    # set color palette
    colorPalette <- "weights"
    if(length(vSecurities) > 11) colorPalette <- "main"

    plotWeights <- ggplot(dfWeightsFinal) +
        geom_bar(aes(x = Date, y = Weight, fill = factor(SecurityID, levels = vSecurities)),
                 stat = "identity", width = 100) +
        scale_fill_quant(palette = colorPalette) +
        scale_x_date(breaks=dateBreaks,date_minor_breaks="1 year",labels=date_format("%Y"),expand=c(0.05,0)) +
        scale_y_continuous(breaks = seq(0, 1, 0.1), labels=percent_format(accuracy = 1)) +
        labs(title = plotTitle, x = "") +
        theme_light() +
        theme(axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_text(face="bold"),
              axis.text.y=element_text(hjust = 1.0)) +
        theme(plot.title=element_text(hjust = 0.5, vjust = 1, size=12)) +
        theme(legend.title=element_blank(),
              legend.text=element_text(face="bold", size=8),
              legend.key=element_rect(color="white"),
              legend.background=element_rect(color="gray", size=0.3, linetype="solid"),
              legend.justification=c(0,1),
              legend.spacing=unit(0,"cm"))

    return(plotWeights)
}


# Plot Yearly Return Rankings
########################################################################
plotYearlyRankings <- function(dfReturns, outlineAsset=NA, plotCtr=NA) {
    dfCalReturns <- getCalendarReturns(dfReturns)
    vAssets <- colnames(dfCalReturns)
    numberOfAssets <- length(vAssets)

    dfCalReturns <- dfCalReturns %>%
        tibble::rownames_to_column(var = "Year") %>%
        mutate(Year = factor(Year)) %>%
        gather(Asset, Return, -Year) %>%
        group_by(Year) %>%
        arrange(desc(Return)) %>%
        mutate(Rank = factor(rank(-Return, ties.method="first")),
               Return = format(round(Return, digits = 2), justify = 'right', nsmall = 2, scientific = F)) %>%
        ungroup(Year) %>%
        arrange(Year, Rank) %>%
        mutate(Asset = factor(Asset, levels = vAssets))

    dfOutline <- NULL
    if(!is.na(outlineAsset)) {
        dfOutline <- dfCalReturns %>%
            dplyr::filter(Asset == outlineAsset)
    }

    plotTitle <- "Yearly Return Rankings"
    if(!is.na(plotCtr)) plotTitle <- paste("Plot", plotCtr, ":", plotTitle)

    plotRanking <- ggplot(dfCalReturns) +
        geom_tile(aes(x=Rank, y=Year, fill=Asset), color = "white") +
        geom_text(aes(x=Rank, y=Year, label=paste(Asset,Return,sep="\n")),
                  color="white", fontface="bold", size=2.6) +
        labs(title = plotTitle, x = "Rank", y = "") +
        scale_fill_quant(palette = "metro") +
        theme_classic() +
        theme(axis.line.x=element_blank(),
              axis.line.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank()) +
        theme(plot.title=element_text(hjust = 0.5, vjust = 1, size=12)) +
        theme(legend.position="none")

    if(!is.null(dfOutline)) {
        plotRanking <- plotRanking +
            geom_tile(data = dfOutline, aes(x=Rank, y=Year, fill=Asset), color = "black", size = 0.75) +
            geom_text(data = dfOutline, aes(x=Rank, y=Year, label=paste(Asset,Return,sep="\n")),
                      color="white", fontface="bold", size=2.6)
    }

    return(plotRanking)
}

# Plot Correlations
##############################################################################################################
plotCorrelations <- function(dfReturns, returnFrequency = c("monthly", "daily", "weekly"), outlineVar = NA, plotTitle = NULL) {

    df <- dfReturns

    if(is.null(plotTitle)) {
        firstPerfDate <- as.Date(first(dfReturns$Date))
        lastPerfDate <- as.Date(last(dfReturns$Date))
        plotTitle <- str_to_title(glue::glue("Correlations of {returnFrequency[1]} Returns"))
        plotTitle <- paste(plotTitle, "\n", format(firstPerfDate,"%b %Y"), "-", format(lastPerfDate,"%b %Y"))

        df <- dfReturns %>% select(-Date)
    }

    dfCor <- cor(df) %>%
        round(digits = 2) %>%
        reorderCorrelationMatrix() %>%
        tk_tbl(rename_index = "Var1") %>%
        gather(Var2, CorVal, -Var1) %>%
        mutate(Var1 = fct_inorder(Var1),
               Var1 = fct_rev(Var1),
               Var2 = factor(Var2, levels = levels(Var1)),
               Var2 = fct_rev(Var2))

    p <- ggplot(dfCor, aes(x = Var1, y = Var2, fill = CorVal)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(name = "correlation", low = "#D11141", mid = "#F8F5F5", high = "#00AEDB", midpoint = 0, limit = c(-1, 1)) +
        labs(title = plotTitle, x = "", y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        theme(plot.title=element_text(hjust = 0.5, vjust = 1, size=12)) +
        coord_fixed() +
        geom_text(aes(Var1, Var2, label = CorVal), color = "black", size = 3)

    if(!is.na(outlineVar) & outlineVar %in% as.character(dfCor$Var1)) {
        dfOutline <- dfCor %>% dplyr::filter(Var1 == outlineVar)

        p <- p + geom_tile(data = dfOutline, aes(x = Var2, y = Var1, group = Var1), color = "black", size = 0.75) +
            geom_tile(data = dfOutline, aes(x = Var1, y = Var2, group = Var2), color = "black", size = 0.75) +
            geom_text(aes(Var1, Var2, label = CorVal), color = "black", size = 3)
    }

    return(p)
}
