fit <- lm(mpg ~ wt + hp, data = mtcars)
fit2 <- lm(mpg ~ wt, data = mtcars)
dfPred <- data.frame(car = rownames(mtcars), mpg = mtcars$mpg, pred = unname(fitted(fit)))
dfBoth <- rbind(data.frame(dfPred, model = "wt + hp"),
                data.frame(dfPred[1:2], pred = unname(fitted(fit2)), model = "wt"))

label_layer <- function(p) {
    Filter(function(l) inherits(l$geom, "GeomLabel"), p$layers)
}

metrics_of <- function(f) {
    sprintf("RMSE = %s, R\u00b2 = %.3f",
            prettyNum(signif(sqrt(mean(residuals(f)^2)), 3), big.mark = ","),
            summary(f)$r.squared)
}

test_that("eda_plot_actual_vs_predicted builds", {
    expect_buildable(eda_plot_actual_vs_predicted(dfPred, "mpg", "pred"))
    expect_buildable(eda_plot_actual_vs_predicted(dfPred, "mpg", "pred", feature_id = "car", nLabel = 0))
    expect_buildable(eda_plot_actual_vs_predicted(dfBoth, "mpg", "pred", feature_model = "model",
                                                  feature_id = "car", xLog = TRUE, yLog = TRUE))
})

test_that("the subtitle shows RMSE and R-squared", {
    p <- eda_plot_actual_vs_predicted(dfPred, "mpg", "pred")
    expect_equal(p$labels$subtitle, metrics_of(fit))
    expect_equal(p$labels$subtitle, "RMSE = 2.47, R\u00b2 = 0.827")
    expect_equal(p$labels$title, "Actual v/s predicted mpg")
})

test_that("each model gets a panel and a line in the subtitle", {
    p <- eda_plot_actual_vs_predicted(dfBoth, "mpg", "pred", feature_model = "model")
    expect_s3_class(p$facet, "FacetWrap")
    expect_equal(levels(p$data$model), c("wt + hp", "wt"))
    expect_equal(p$labels$subtitle,
                 paste0("wt + hp: ", metrics_of(fit), "\nwt: ", metrics_of(fit2)))
})

test_that("the largest errors are labelled in each panel", {
    p <- eda_plot_actual_vs_predicted(dfBoth, "mpg", "pred", feature_model = "model",
                                      feature_id = "car", nLabel = 3)
    expect_length(label_layer(p), 1)
    dfLabel <- label_layer(p)[[1]]$data
    expect_equal(nrow(dfLabel), 6)
    for(m in c("wt + hp", "wt")) {
        f <- if(m == "wt") fit2 else fit
        worst <- rownames(mtcars)[order(abs(residuals(f)), decreasing = TRUE)[1:3]]
        expect_setequal(dfLabel$id[dfLabel$model == m], worst)
    }

    # row numbers are used without feature_id
    p <- eda_plot_actual_vs_predicted(dfPred, "mpg", "pred", nLabel = 1)
    expect_equal(label_layer(p)[[1]]$data$id, unname(which.max(abs(residuals(fit)))))

    p <- eda_plot_actual_vs_predicted(dfPred, "mpg", "pred", nLabel = 0)
    expect_length(label_layer(p), 0)
})

test_that("the 45-degree line spans actual and predicted values", {
    p <- eda_plot_actual_vs_predicted(dfPred, "mpg", "pred")
    line <- p$layers[[1]]$data
    expect_equal(range(line$v), range(c(dfPred$mpg, dfPred$pred)))
    expect_equal(p$layers[[1]]$aes_params$linetype, "dashed")
})

test_that("labelsScale formats axes and RMSE, and log scales apply", {
    p <- eda_plot_actual_vs_predicted(dfPred, "mpg", "pred", labelsScale = scales::label_number(accuracy = 0.1),
                                      xLog = TRUE, yLog = TRUE)
    expect_true(startsWith(p$labels$subtitle, "RMSE = 2.5,"))
    expect_equal(p$scales$get_scales("x")$get_transformation()$name, "log-10")
    expect_equal(p$scales$get_scales("y")$get_transformation()$name, "log-10")
})

test_that("rows with missing values are dropped", {
    df <- dfPred
    df$pred[1:2] <- NA
    p <- eda_plot_actual_vs_predicted(df, "mpg", "pred")
    expect_equal(nrow(p$data), 30)
    expect_buildable(p)
})
