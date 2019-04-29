# Plot interactive Yearly Returns chart
plotYearlyRankings_hc <- function(dfReturns, outlineAsset = NA) {
    dfCalReturns <- getCalendarReturns(dfReturns)
    vAssets <- colnames(dfCalReturns)

    if(!is.na(outlineAsset)) {
        vAssets <- c(outlineAsset, vAssets[-which(vAssets == outlineAsset)])
    }

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

    # line colors
    vColors <- getPlotColors("metro", F, length(vAssets))

    # label and tooltip formats
    label_format <- '<span style=\"font-weight:bold\">{series.name}</span><br/>'
    tooltip_format <- '<span style=\"color:{series.color};font-weight:bold\">{series.name}</span>: <b>{point.Return}%</b><br/>'

    hchart(type = "heatmap", dfCalReturns, hcaes(x = Rank, y = Year, group = Asset)) %>%
        hc_colors(colors = vColors) %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, color = "#FFFFFF", shadow = F, format = label_format))) %>%
        hc_tooltip(pointFormat = tooltip_format, headerFormat = "") %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_title(text = "Yearly Return Rankings") %>%
        hc_legend(enabled = FALSE)
}


# Plot interactive Portfolio Weights chart
plotPortfolioWeights_hc <- function(dfWeights) {
    vSecurities <- unique(dfWeights$SecurityID)

    # Determine Cash Weights
    dfWeightsFinal <- dfWeights %>%
        tbl_df() %>%
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

    dfWeightsFinal <- dfWeightsFinal %>%
        mutate(SecurityID = factor(SecurityID, levels = vSecurities))

    # column colors
    vColors <- getPlotColors(palette = "weights", F, length(vSecurities))
    tooltip_format <- '<span style=\"color:{series.color};font-weight:bold\">{series.name}</span>: <b>{point.percentage:.1f}%</b><br/>'

    # plot portfolio weights
    hchart(type = "area", dfWeightsFinal, hcaes(x = Date, y = Weight, group = SecurityID)) %>%
        hc_chart(zoomType = "x") %>%
        hc_colors(colors = vColors) %>%
        hc_plotOptions(area = list(stacking = "percent", lineWidth = 1, marker = list(lineWidth = 1))) %>%
        hc_xAxis(type = "datetime", title = list(text = "")) %>%
        hc_yAxis(labels = list(format = "{value}%"), opposite = FALSE) %>%
        hc_tooltip(pointFormat = tooltip_format, shared = TRUE, backgroundColor = "#D3D3D3") %>%
        hc_title(text = "Portfolio Weights")
}


# Plot interactive Correlations chart
plotCorrelations_hc <- function(dfReturns, returnFrequency = c("monthly", "daily", "weekly")) {

    mCor <- cor(dfReturns[-1])
    vOrder <- corrplot::corrMatOrder(mCor, order = "hclust")
    mCor <- mCor[vOrder, vOrder]

    firstPerfDate <- as.Date(first(dfReturns$Date))
    lastPerfDate <- as.Date(last(dfReturns$Date))
    plotTitle <- str_to_title(glue::glue("Correlations of {returnFrequency[1]} Returns"))
    plotTitle <- paste(plotTitle, format(firstPerfDate,"%b %Y"), "-", format(lastPerfDate,"%b %Y"))

    pointFormatter <- JS("function(){ return Highcharts.numberFormat(this.point.value, 2); }")

    lColorStops <- list(list(0, "#D11141"),
                        list(0.5, "#F8F5F5"),
                        list(1, "#00AEDB"))

    hchart(mCor) %>%
        hc_colorAxis(stops = NULL) %>%
        hc_colorAxis(min = -1, max = 1, stops = lColorStops, reversed = FALSE) %>%
        hc_title(text = plotTitle) %>%
        hc_legend(align = "right", layout = "vertical", verticalAlign = "middle") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, formatter = pointFormatter)))
}


# Plot interactive Rolling Excess Returns
plotRollingExcessReturns_hc <- function(dfReturns, dfRecessions=NULL, coreStrategyName, baseBenchmarkName, rollingMonths) {
    dfExcessReturns <- getRollingAnnualizedReturns(dfReturns, rollingMonths) %>%
        mutate(ExcessReturn = !!sym(coreStrategyName) - !!sym(baseBenchmarkName),
               ExcessReturn = ExcessReturn * 100)
    xtExcessReturn <- dfExcessReturns %>% select(Date, ExcessReturn) %>% timetk::tk_xts(date_var = Date, silent = T)

    # determine title text
    plotTitle <- glue::glue("Rolling Annualized Excess {rollingMonths} Months Returns Over {baseBenchmarkName}")

    # tooltip format
    tooltip_format <- "<span style=\"font-weight:bold\">{series.name}</span>: <b>{point.y:.2f}%</b><br/>"

    # Zoom buttons
    lZoomButtons <- list(list(type = "ytd", text = "YTD"), list(type = "year", count = 1, text = "1y"),
                         list(type = "year", count = 5, text = "5y"), list(type = "year", count = 10, text = "10y"),
                         list(type = "year", count = 20, text = "20y"), list(type = "all", text = "All"))

    hcplot <- hchart(type = "area", dfExcessReturns, hcaes(x = Date, y = ExcessReturn),
                     name = "Excess Return", color = "#00CD00", negativeColor = "#CC0000") %>%
        hc_chart(zoomType = "x") %>%
        hc_rangeSelector(buttons = lZoomButtons, enabled = TRUE) %>%
        hc_xAxis(type = "datetime", title = list(text = "")) %>%
        hc_yAxis(labels = list(format = "{value}%"), opposite = FALSE) %>%
        hc_title(text = plotTitle) %>%
        hc_tooltip(pointFormat = tooltip_format, valueDecimals = 2)

    # add Recessions bands
    if(!is.null(dfRecessions)) {
        hcplot <- plotAddRecessions_hc(hcplot, dfExcessReturns, dfRecessions)
    }
    hcplot
}

hc_tooltip_sorted <- function(hc, ...) {
    # http://stackoverflow.com/a/16954666/829971
    hc %>%
        highcharter::hc_tooltip(
            shared = TRUE,
            formatter = JS(
                "function(tooltip){
                    function isArray(obj) {
                        return Object.prototype.toString.call(obj) === '[object Array]';
                    }

                    function splat(obj) {
                        return isArray(obj) ? obj : [obj];
                    }

                    var items = this.points || splat(this), series = items[0].series, s;

                    // sort the values
                    items.sort(function(a, b){
                        return ((a.y < b.y) ? -1 : ((a.y > b.y) ? 1 : 0));
                    });
                    items.reverse();

                    return tooltip.defaultFormatter.call(this, tooltip);
                }"
            )
        )
}

hc_tooltip_sorted_table <- function(hc, ...) {
    hc <- hc_tooltip_sorted(hc)

    if(length(list(...))) {
        hc <- highcharter:::.hc_opt(hc, "tooltip", ...)
    }

    hc
}

# Plot interactive Returns chart
plotReturns_hc <- function(dfReturns, dfRecessions = NULL, palette_name = "withgrey") {
    # exclude incomplete rows
    dfReturns <- dfReturns %>% drop_na()
    vStrategyNames <- names(dfReturns)[-1]
    vStrategyNames <- gsub(" ", ".", vStrategyNames)

    # compute cumulative returns
    dfCumReturns <- dfReturns %>% mutate_if(is_bare_double, function(Z) exp(cumsum(log(1 + Z))))
    xtCumReturns <- timetk::tk_xts(dfCumReturns, date_var = Date, silent = TRUE)

    # line colors
    vColors <- getPlotColors(palette_name, F, length(vStrategyNames))

    # Zoom buttons
    lZoomButtons <- list(list(type = "ytd", text = "YTD"), list(type = "year", count = 1, text = "1y"),
                         list(type = "year", count = 5, text = "5y"), list(type = "year", count = 10, text = "10y"),
                         list(type = "year", count = 20, text = "20y"), list(type = "all", text = "All"))

    # tooltips
    pointFormatter_perf <- paste0('<tr><td style="color: {series.color}; font-weight:bold">{series.name}: </td>',
                                  '<td style="text-align: right"><b>${point.change:.2f}</b></td></tr>')

    # plot cumulative returns
    hcplot <- highchart(type = "stock") %>%
        hc_chart(zoomType = "x") %>%
        hc_rangeSelector(buttons = lZoomButtons, enabled = TRUE,
                         buttonTheme = list(states = list(select = list(fill = "#3C8DBC", style = list(color = "#FFFFFF"))))) %>%
        hc_navigator(enabled = FALSE) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_xAxis(type = "datetime", title = list(text = "")) %>%
        hc_yAxis(type = "logarithmic", title = list(text = "Growth of $100"), labels = list(format = "${value}"), opposite = FALSE) %>%
        hc_legend(enabled = TRUE) %>%
        hc_tooltip(shared = TRUE, split = FALSE, useHTML = TRUE,
                   xDateFormat = "%b %Y",
                   headerFormat = "<center/>{point.key}<br><table>",
                   pointFormat = pointFormatter_perf,
                   footerFormat = "</table>")

    for(i in seq_along(vStrategyNames)) {
        strategyName <- vStrategyNames[i]
        hcplot <- hcplot %>%
            hc_add_series(xtCumReturns[,strategyName], name = strategyName, compare = "percent", compareBase = 100, marker = list(enabled = FALSE))
    }
    hcplot <- hcplot %>% hc_colors(vColors)

    # add Recessions bands
    if(!is.null(dfRecessions)) {
        hcplot <- plotAddRecessions_hc(hcplot, dfReturns, dfRecessions)
    }
    hcplot
}


# Plot interactive Performance chart
plotPerformance_hc <- function(dfReturns, dfRecessions = NULL, palette_name = "withgrey") {
    # exclude incomplete rows
    dfReturns <- dfReturns %>% drop_na()
    vStrategyNames <- names(dfReturns)[-1]
    vStrategyNames <- gsub(" ", ".", vStrategyNames)

    # compute cumulative returns
    dfCumReturns <- dfReturns %>% mutate_if(is_bare_double, function(Z) exp(cumsum(log(1 + Z))))
    xtCumReturns <- timetk::tk_xts(dfCumReturns, date_var = Date, silent = TRUE)

    # compute drawdowns
    dfDrawdowns <- dfReturns %>%
        mutate_if(is_bare_double, function(Z) suppressWarnings(as.vector(timeSeries::drawdowns(timeSeries::as.timeSeries(Z))))) %>%
        mutate_if(is_bare_double, function(Z) Z*100)
    xtDrawdowns <- timetk::tk_xts(dfDrawdowns, date_var = Date, silent = TRUE)

    # line colors
    vColors <- getPlotColors(palette_name, F, length(vStrategyNames))

    # Zoom buttons
    lZoomButtons <- list(list(type = "ytd", text = "YTD"), list(type = "year", count = 1, text = "1y"),
                         list(type = "year", count = 5, text = "5y"), list(type = "year", count = 10, text = "10y"),
                         list(type = "year", count = 20, text = "20y"), list(type = "all", text = "All"))

    # tooltips
    tooltip_format_dd <- "<span style=\"color:{series.color};font-weight:bold\">{series.name}</span>: <b>{point.y:.2f}%</b><br/>"
    tooltip_format_perf <- "<span style=\"color:{series.color};font-weight:bold\">{series.name}</span>: <b>${point.change:.2f}</b><br/>"
    # pointFormatter_perf <- JS("function() {
    #                           return '<span style=\"color:' + this.series.color + ';font-weight:bold\">'+ this.series.name +
    #                           '<b>: '+ Highcharts.numberFormat(this.change-100.0, 2) +'%' + '</b>';
    #                         }")

    # plot cumulative returns and drawdowns
    hcplot <- highchart(type = "chart") %>%
        hc_chart(zoomType = "x") %>%
        hc_yAxis_multiples(list(top="0%", height="70%", type="logarithmic", title=list(text="Growth of $100"), labels=list(format="${value}"),
                                opposite=FALSE, showFirstLabel=FALSE),
                           list(top="70%", height="30%", type="line", title=list(text="Drawdowns"), labels=list(format="{value}%"),
                                opposite=TRUE)) %>%
        hc_rangeSelector(buttons = lZoomButtons, enabled = TRUE) %>%
        hc_xAxis(type = "datetime") %>%
        hc_tooltip(split = TRUE) %>%
        hc_title(text = "Cumulative Performance") %>%
        hc_legend(enabled = TRUE)

    for(i in seq_along(vStrategyNames)) {
        strategyName <- vStrategyNames[i]
        hcplot <- hcplot %>%
            hc_add_series(xtCumReturns[,strategyName], yAxis = 0, name = strategyName, tooltip = list(pointFormat = tooltip_format_perf),
                          id = glue::glue("{strategyName}_perf"), compare = "percent", compareBase = 100, marker = list(enabled = FALSE))
    }
    for(i in seq_along(vStrategyNames)) {
        strategyName <- vStrategyNames[i]
        hcplot <- hcplot %>%
            hc_add_series(xtDrawdowns[,strategyName], yAxis = 1, name = strategyName, tooltip = list(pointFormat = tooltip_format_dd),
                          id = glue::glue("{strategyName}_dd"), linkedTo=glue::glue("{strategyName}_perf"), marker = list(enabled = FALSE))
    }
    hcplot <- hcplot %>% hc_colors(vColors)

    # add Recessions bands
    if(!is.null(dfRecessions)) {
        hcplot <- plotAddRecessions_hc(hcplot, dfReturns, dfRecessions)
    }
    hcplot
}


# add Recession bands to highstock chart
plotAddRecessions_hc <- function(hcplot, dfReturns, dfRecessions) {
    dfRect <- getRecessionIntervals(dfReturns, dfRecessions)

    # create Recession bands
    if(nrow(dfRect) > 0) {
        lBands = list()
        for(i in 1:nrow(dfRect)) {
            dfRect_span <- dfRect[i,]
            date_from <- dfRect_span$date_from %>% as.Date(tz = "UTC") %>% datetime_to_timestamp()
            date_to <- dfRect_span$date_to %>% as.Date(tz = "UTC") %>% datetime_to_timestamp()

            lBands[[i]] <- list(from = date_from, to = date_to, color = "#D3D3D3")
        }
        hcplot <- hcplot %>% hc_xAxis(plotBands = lBands)
    }
    hcplot
}

