test_that("palettes are available by name", {
    pals <- get_palettes()
    expect_named(pals, c("main", "withgrey", "pnl", "weights", "metro", "greyscale", "heatmap"))
    expect_equal(pals$metro, c("#D11141", "#00B159", "#00AEDB", "#F37735", "#FFC425"))
})

test_that("quant_pal interpolates and reverses", {
    expect_equal(quant_pal("metro")(5), get_palettes()$metro)
    expect_equal(quant_pal("heatmap", reverse = TRUE)(2), c("#D11141", "#00AEDB"))
    expect_length(quant_pal("pnl")(7), 7)
    expect_equal(quant_pal()(1), "#CC0000")
    expect_error(quant_pal("nope"))
})

test_that("plot_colors returns hex codes, interpolating when needed", {
    expect_equal(plot_colors("pnl", n = 2), c("#00CD00", "#CC0000"))
    expect_equal(plot_colors("pnl", reverse = TRUE, n = 1), "#CC0000")
    expect_length(plot_colors("metro", n = 9), 9)
    expect_match(plot_colors("main", n = 3), "^#[0-9A-F]{6}$")
})

test_that("convert_mm_to_pts converts points to millimetres", {
    expect_equal(convert_mm_to_pts(72.27), 25.4)
    expect_equal(convert_mm_to_pts(11), 11 / ggplot2::.pt, tolerance = 1e-3)
})

test_that("discrete scales map levels to palette colors", {
    df <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = g, fill = g)) +
        ggplot2::geom_point(shape = 21) +
        scale_color_quant(palette = "metro") +
        scale_fill_quant(palette = "metro", reverse = TRUE)
    b <- ggplot2::ggplot_build(p)$data[[1]]
    expect_equal(b$colour, quant_pal("metro")(3))
    expect_equal(b$fill, quant_pal("metro", reverse = TRUE)(3))
    expect_identical(scale_colour_quant, scale_color_quant)
})

test_that("continuous scales use a gradient", {
    expect_s3_class(scale_fill_quant("heatmap", discrete = FALSE), "ScaleContinuous")
    expect_s3_class(scale_color_quant("heatmap", discrete = FALSE), "ScaleContinuous")
})

test_that("manual scales default to palette values", {
    df <- data.frame(x = 1:2, y = 1:2, g = c("up", "down"))
    p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = g)) +
        ggplot2::geom_point() +
        scale_color_quant_manual(palette = "pnl")
    expect_setequal(ggplot2::ggplot_build(p)$data[[1]]$colour, c("green3", "#CC0000"))
})

test_that("test_pal draws without error", {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    expect_no_error(test_pal("metro"))
})
