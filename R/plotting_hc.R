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
plotPortfolioWeights_hc <- function(dfWeights, plotTitle = "Portfolio Weights") {
    vSecurities <- unique(dfWeights$SecurityID)

    # Determine Cash Weights
    dfWeightsFinal <- dfWeights %>%
        select(-SignalDate) %>%
        rename(Date = RebalanceDate) %>%
        dplyr::filter(str_to_upper(SecurityID) != "CASH") %>%
        mutate(Date = as.Date(as.character(Date)),
               Weight = as.numeric(as.character(Weight))) %>%
        group_by(Date, SecurityID) %>%
        summarise(Weight = sum(Weight, na.rm = T)) %>%
        ungroup() %>%
        spread(SecurityID, Weight) %>%
        mutate(Cash = 1 - rowSums(.[-1], na.rm = T)) %>%
        gather(SecurityID, Weight, -Date) %>%
        arrange(Date, SecurityID)

    # Find unique security names and put Cash at the end
    vSecurities <- unique(dfWeightsFinal$SecurityID)
    vSecurities <- c(vSecurities[-which(str_to_upper(vSecurities) == "CASH")], "Cash")

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
        hc_yAxis(labels = list(format = "{value}%"), opposite = FALSE, showFirstLabel = FALSE) %>%
        hc_tooltip(pointFormat = tooltip_format, shared = TRUE, backgroundColor = "#D3D3D3") %>%
        hc_title(text = plotTitle)
}


plotCorrelations_hc <- function(dfReturns, returnFrequency = c("monthly", "daily", "weekly"), plotTitle = NULL) {
    library(dplyr)
    library(highcharter)
    library(glue)

    df <- dfReturns
    plotCaption <- ""

    # Set the caption if Date column exists
    if ("Date" %in% colnames(df)) {
        df <- df %>% select(-Date)
        firstPerfDate <- as.Date(first(dfReturns$Date))
        lastPerfDate <- as.Date(last(dfReturns$Date))
        plotCaption <- paste(format(firstPerfDate, "%b %Y"), "-", format(lastPerfDate, "%b %Y"))
    }

    # Set the title if not provided
    if (is.null(plotTitle)) {
        plotTitle <- str_to_title(glue::glue("Correlations of {returnFrequency[1]} Returns"))
    }

    # Calculate correlation matrix and reorder it
    mCor <- cor(df)
    mCor <- reorderCorrelationMatrix(mCor)

    # Set diagonal values to 1.1 to represent them as a unique color
    diag(mCor) <- 1.01

    # Define color stops, with 1.1 mapped to gray
    lColorStops <- list(
        list(0, "#D11141"),    # Red for negative correlations
        list(0.5, "#F8F5F5"),  # White for zero correlation
        list(1, "#00AEDB"),    # Blue for positive correlations
        list(1.01, "grey")   # Gray for diagonal cells
    )

    # JavaScript formatter to hide text on diagonal cells
    pointFormatter <- JS("
        function() {
            return (this.point.value === 1.01) ? '' : Highcharts.numberFormat(this.point.value, 2);
        }
    ")

    tooltipFormatter <- JS("
        function() {
            return (this.point.x === this.point.y) ?
            (this.series.yAxis.categories[this.point.y] + ' ~ ' + this.series.xAxis.categories[this.point.x] + ': 1.00') :
            (this.series.yAxis.categories[this.point.y] + ' ~ ' + this.series.xAxis.categories[this.point.x] + ': ' +
            Highcharts.numberFormat(this.point.value, 2));
        }
    ")


    # Generate the heatmap
    hchart(mCor, "heatmap") %>%
        hc_colorAxis(
            type = "linear",
            min = -1,
            max = 1,  # Extend max to 1.1 to include diagonal cells
            stops = lColorStops,
            reversed = FALSE
        ) %>%
        hc_title(text = plotTitle) %>%
        hc_caption(text = plotCaption, align = "center") %>%
        hc_legend(align = "right", layout = "vertical", verticalAlign = "middle") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, formatter = pointFormatter))) %>%
        hc_tooltip(formatter = tooltipFormatter)
}


# Plot Correlations with confidence intervals in given time frames
plotCorrelationConfidenceIntervals_hc <- function(dfCor, plotTitle = "Correlations with confidence intervals") {

    vTimeFrames <- unique(dfCor$time_frame)
    vColors <- getPlotColors(palette = "main", n = length(vTimeFrames))

    point_tooltip_format <- paste0('<span style="color: {series.color}; font-weight:bold; text-align: right">{series.name} </span>',
                                   '<b>: {point.y}</b><br>')
    errorbar_tooltip_format <- paste0('<span style="color: {series.color}; font-weight:bold; text-align: right">{series.name} </span>',
                                      '<b>: {point.low} - {point.high}</b><br>')

    highchart() %>%
        hc_add_series(dfCor, type = "point", hcaes(x = predictor, y = estimate, group = time_frame),
                      color = vColors, tooltip = list(pointFormat = point_tooltip_format)) %>%
        hc_add_series(dfCor, type = "errorbar", hcaes(x = predictor, low = conf.low, high = conf.high, group = time_frame),
                      color = vColors, tooltip = list(pointFormat = errorbar_tooltip_format)) %>%
        hc_plotOptions(errorbar = list(pointWidth = 15)) %>%
        hc_xAxis(type = "categorical", categories = dfCor$predictor) %>%
        hc_yAxis(title = list(text = "Correlation")) %>%
        hc_tooltip(shared = TRUE, split = FALSE, useHTML = TRUE, crosshairs = TRUE, valueDecimals = 2, headerFormat = "{point.key}<br>") %>%
        hc_chart(inverted = TRUE) %>%
        hc_title(text = plotTitle)
}


# Plot interactive Rolling Excess Returns
plotRollingExcessReturns_hc <- function(dfReturns, dfRecessions=NULL, coreStrategyName, baseBenchmarkName, rollingMonths) {
    dfExcessReturns <- getRollingAnnualizedReturns(dfReturns, rollingMonths) %>%
        mutate(ExcessReturn = !!sym(coreStrategyName) - !!sym(baseBenchmarkName),
               ExcessReturn = ExcessReturn * 100)

    # determine title text
    plotTitle <- glue::glue("{coreStrategyName} Rolling Annualized Excess {rollingMonths} Months Returns Over {baseBenchmarkName}")

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
                'function(tooltip){
                    function isArray(obj) {
                        return Object.prototype.toString.call(obj) === "[object Array]";
                    }

                    function splat(obj) {
                        return isArray(obj) ? obj : [obj];
                    }

                    var items = this.points || splat(this), series = items[0].series, s;

                    // sort the values
                    items.sort(function(a, b){
                        return ((a.percentage < b.percentage) ? -1 : ((a.percentage > b.percentage) ? 1 : 0));
                    });
                    items.reverse();

                    return tooltip.defaultFormatter.call(this, tooltip);
                }'
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


# Plot interactive line chart
plotLines_hc <- function(dfIndicators, dfRecessions = NULL, palette_name = "withgrey") {
    # exclude incomplete rows
    dfIndicators <- dfIndicators %>% drop_na()
    vColNames <- names(dfIndicators)[-1]
    vColNames <- gsub(" ", ".", vColNames)

    # compute cumulative returns
    xtIndicators <- timetk::tk_xts(dfIndicators, date_var = Date, silent = TRUE)

    # line colors
    vColors <- getPlotColors(palette_name, F, length(vColNames))

    # Zoom buttons
    lZoomButtons <- list(list(type = "ytd", text = "YTD"), list(type = "year", count = 1, text = "1y"),
                         list(type = "year", count = 5, text = "5y"), list(type = "year", count = 10, text = "10y"),
                         list(type = "year", count = 20, text = "20y"), list(type = "year", count = 50, text = "50y"),
                         list(type = "all", text = "All"))

    # tooltips
    pointFormatter_perf <- paste0('<tr><td style="color: {series.color}; font-weight:bold">{series.name}: </td>',
                                  '<td style="text-align: right"><b>{point.y:.2f}%</b></td></tr>')

    # plot cumulative returns
    hcplot <- highchart(type = "stock") %>%
        hc_chart(zoomType = "x") %>%
        hc_rangeSelector(buttons = lZoomButtons, enabled = TRUE,
                         buttonTheme = list(states = list(select = list(fill = "#3C8DBC", style = list(color = "#FFFFFF"))))) %>%
        hc_navigator(enabled = FALSE) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_xAxis(type = "datetime", title = list(text = "")) %>%
        hc_yAxis(type = "percent", labels = list(format = "{value}%"), opposite = FALSE) %>%
        hc_legend(enabled = TRUE) %>%
        hc_tooltip(shared = TRUE, split = FALSE, useHTML = TRUE, sort = TRUE, table = TRUE,
                   xDateFormat = "%b %Y",
                   pointFormat = pointFormatter_perf)

    for(i in seq_along(vColNames)) {
        colName <- vColNames[i]
        hcplot <- hcplot %>%
            hc_add_series(xtIndicators[,colName], name = colName, marker = list(enabled = FALSE))
    }
    hcplot <- hcplot %>% hc_colors(vColors)

    # add Recessions bands
    if(!is.null(dfRecessions)) {
        hcplot <- plotAddRecessions_hc(hcplot, dfIndicators, dfRecessions)
    }
    hcplot
}


# Plot interactive Returns chart
plotReturns_hc <- function(dfReturns, dfRecessions = NULL, returnFrequency = c("monthly", "daily", "weekly"), palette_name = "withgrey") {
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
                                  '<td style="text-align: right"><b>${point.change:.0f}</b></td></tr>')

    date_formatter <- ifelse(returnFrequency[1] == "daily", "%A, %b %e, %Y", "%b %Y")

    tooltip_formatter <- sprintf("
                function() {
                    var s = '<b>' + Highcharts.dateFormat('%s', this.x) + '</b>';
                    s += '<table>';

                    var performancePoints = [];

                    this.points.forEach(function(point) {
                            performancePoints.push(point);
                    });

                    // Sort the performance points by change value
                    performancePoints.sort(function(a, b) {
                        return b.point.change - a.point.change;
                    });

                    if (performancePoints.length > 0) {
                        s += '<tr><td colspan=\"2\" style=\"font-weight:bold\">Growth of $100:</td></tr>';
                        performancePoints.forEach(function(point) {
                            s += '<tr><td style=\"font-weight:bold; color:' + point.series.color + '\">' + point.series.name + ': </td>' +
                                 '<td style=\"text-align: right\">' + (point.point.change.toFixed(0) ? ('$' + point.point.change.toFixed(0)) : point.y.toFixed(0)) + '</td></tr>';
                        });
                    }

                    s += '</table>';
                    return s;
                }
            ", date_formatter)

    # plot cumulative returns
    hcplot <- highchart(type = "chart") %>%
        hc_chart(zoomType = "x") %>%
        hc_rangeSelector(buttons = lZoomButtons, enabled = TRUE,
                         buttonTheme = list(states = list(select = list(fill = "#3C8DBC", style = list(color = "#FFFFFF"))))) %>%
        hc_navigator(enabled = FALSE) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_xAxis(type = "datetime", title = list(text = "")) %>%
        hc_yAxis(type = "logarithmic", title = list(text = "Growth of $100"), labels = list(format = "${value}"), showFirstLabel = FALSE, opposite = FALSE) %>%
        hc_legend(enabled = TRUE) %>%
        hc_tooltip(
            shared = TRUE,
            useHTML = TRUE,
            formatter = JS(tooltip_formatter)
        )

    for(i in seq_along(vStrategyNames)) {
        strategyName <- vStrategyNames[i]
        hcplot <- hcplot %>%
            hc_add_series(xtCumReturns[,strategyName], name = strategyName,
                          compare = "percent", compareBase = 100,
                          tooltip = list(pointFormat = pointFormatter_perf),
                          marker = list(enabled = FALSE))
    }
    hcplot <- hcplot %>% hc_colors(vColors)

    # add Recessions bands
    if(!is.null(dfRecessions)) {
        hcplot <- plotAddRecessions_hc(hcplot, dfReturns, dfRecessions)
    }
    hcplot
}


# Plot interactive Performance chart
plotPerformance_hc <- function(dfReturns, dfRecessions = NULL,
                               returnFrequency = c("monthly", "daily", "weekly"),
                               palette_name = "withgrey", plotTitle = "Cumulative Performance") {

    dfReturns <- dfReturns %>% drop_na()
    vStrategyNames <- names(dfReturns)[-1]
    vStrategyNames <- gsub(" ", ".", vStrategyNames)

    dfCumReturns <- dfReturns %>% as_tibble() %>% mutate_if(is_bare_double, function(Z) exp(cumsum(log(1 + Z))))
    xtCumReturns <- timetk::tk_xts(dfCumReturns, date_var = Date, silent = TRUE)

    dfDrawdowns <- dfReturns %>% as_tibble() %>%
        mutate_if(is_bare_double, function(Z) suppressWarnings(as.vector(PerformanceAnalytics::Drawdowns(Z)))) %>%
        mutate_if(is_bare_double, function(Z) Z * 100)
    xtDrawdowns <- timetk::tk_xts(dfDrawdowns, date_var = Date, silent = TRUE)

    vColors <- getPlotColors(palette_name, F, length(vStrategyNames))
    lZoomButtons <- list(list(type = "ytd", text = "YTD"),
                         list(type = "year", count = 1, text = "1y"),
                         list(type = "year", count = 5, text = "5y"),
                         list(type = "year", count = 10, text = "10y"),
                         list(type = "year", count = 20, text = "20y"),
                         list(type = "all", text = "All"))

    pointFormatter_perf <- paste0("<tr><td style=\"color: {series.color}; font-weight:bold\">{series.name}: </td>",
                                  "<td style=\"text-align: right\"><b>${point.change:.0f}</b></td></tr>")

    pointFormatter_dd <- paste0("<tr><td style=\"color: {series.color}; font-weight:bold\">{series.name}: </td>",
                                "<td style=\"text-align: right\"><b>{point.y:.2f}%</b></td></tr>")

    tooltip_formatter <- paste0("
                function() {
                    var s = '<b>' + Highcharts.dateFormat('%A, %b %e, %Y', this.x) + '</b>';
                    s += '<table>';

                    var performancePoints = [];
                    var drawdownPoints = [];

                    this.points.forEach(function(point) {
                        if (point.series.options.yAxis === 0) {
                            performancePoints.push(point);
                        } else if (point.series.options.yAxis === 1) {
                            drawdownPoints.push(point);
                        }
                    });

                    // Sort the performance points by change value
                    performancePoints.sort(function(a, b) {
                        return b.point.change - a.point.change;
                    });

                    // Sort the drawdown points by y value
                    drawdownPoints.sort(function(a, b) {
                        return b.y - a.y;
                    });

                    if (performancePoints.length > 0) {
                        s += '<tr><td colspan=\"2\" style=\"font-weight:bold\">Growth of $100:</td></tr>';
                        performancePoints.forEach(function(point) {
                            s += '<tr><td style=\"font-weight:bold; color:' + point.series.color + '\">' + point.series.name + ': </td>' +
                                 '<td style=\"text-align: right\">' + (point.point.change.toFixed(0) ? ('$' + point.point.change.toFixed(0)) : point.y.toFixed(0)) + '</td></tr>';
                        });
                    }

                    if (drawdownPoints.length > 0) {
                        s += '<tr><td colspan=\"2\" style=\"font-weight:bold\">Drawdowns:</td></tr>';
                        drawdownPoints.forEach(function(point) {
                            s += '<tr><td style=\"font-weight:bold; color:' + point.series.color + '\">' + point.series.name + ': </td>' +
                                 '<td style=\"text-align: right\">' + point.y.toFixed(2) + '%</td></tr>';
                        });
                    }

                    s += '</table>';
                    return s;
                }
            ")

    hcplot <- highchart(type = "chart") %>%
        hc_chart(zoomType = "x") %>%
        hc_yAxis_multiples(
            list(top = "0%", height = "70%", type = "logarithmic",
                 title = list(text = "Growth of $100"), labels = list(format = "${value}"),
                 opposite = FALSE, showFirstLabel = FALSE),
            list(top = "70%", height = "30%", type = "line",
                 title = list(text = "Drawdowns"), labels = list(format = "{value}%"), opposite = TRUE)
        ) %>%
        hc_rangeSelector(buttons = lZoomButtons, enabled = TRUE) %>%
        hc_xAxis(type = "datetime") %>%
        hc_tooltip(
            shared = TRUE,
            useHTML = TRUE,
            formatter = JS(tooltip_formatter)
        ) %>%
        hc_title(text = plotTitle) %>%
        hc_legend(enabled = TRUE)

    # Add cumulative returns series
    for (i in seq_along(vStrategyNames)) {
        strategyName <- vStrategyNames[i]
        hcplot <- hcplot %>% hc_add_series(
            xtCumReturns[, strategyName],
            yAxis = 0, name = strategyName,
            tooltip = list(pointFormat = pointFormatter_perf),
            id = glue::glue("{strategyName}_perf"),
            compare = "percent", compareBase = 100,
            marker = list(enabled = FALSE)
        )
    }

    # Add drawdowns series
    for (i in seq_along(vStrategyNames)) {
        strategyName <- vStrategyNames[i]
        hcplot <- hcplot %>% hc_add_series(
            xtDrawdowns[, strategyName],
            yAxis = 1, name = strategyName,
            tooltip = list(pointFormat = pointFormatter_dd),
            id = glue::glue("{strategyName}_dd"),
            linkedTo = glue::glue("{strategyName}_perf"),
            marker = list(enabled = FALSE)
        )
    }

    hcplot <- hcplot %>% hc_colors(vColors)

    if (!is.null(dfRecessions)) {
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

