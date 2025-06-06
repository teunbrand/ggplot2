test_that("labels match breaks, even when outside limits", {
  sc <- scale_y_continuous(breaks = 1:4, labels = 1:4, limits = c(1, 3))

  expect_equal(sc$get_breaks(), 1:4)
  expect_equal(sc$get_labels(), 1:4)
  expect_equal(sc$get_breaks_minor(), c(1, 1.5, 2, 2.5, 3))
})

test_that("labels match breaks", {
  expect_snapshot(scale_x_discrete(breaks = 1:3, labels = 1:2), error = TRUE)
  expect_snapshot(scale_x_continuous(breaks = 1:3, labels = 1:2), error = TRUE)
})

test_that("labels don't have to match null breaks", {
  expect_silent(check_breaks_labels(breaks = 1:3, labels = NULL))
  expect_silent(check_breaks_labels(breaks = NULL, labels = 1:2))
})

test_that("labels don't have extra spaces", {
  labels <- c("a", "abc", "abcdef")

  sc1 <- scale_x_discrete(limits = labels)
  sc2 <- scale_fill_discrete(limits = labels)

  expect_equal(sc1$get_labels(), labels)
  expect_equal(sc2$get_labels(), labels)
})

test_that("out-of-range breaks are dropped", {

  # Limits are explicitly specified, automatic labels
  sc <- scale_x_continuous(breaks = 1:5, limits = c(2, 4))
  bi <- sc$break_info()
  expect_equal(bi$labels, as.character(2:4))
  expect_equal(bi$major, c(0, 0.5, 1))
  expect_equal(bi$major_source, 2:4)

  # Limits and labels are explicitly specified
  sc <- scale_x_continuous(breaks = 1:5, labels = letters[1:5], limits = c(2, 4))
  bi <- sc$break_info()
  expect_equal(bi$labels, letters[2:4])
  expect_equal(bi$major, c(0, 0.5, 1))
  expect_equal(bi$major_source, 2:4)

  # Limits are specified, and all breaks are out of range
  sc <- scale_x_continuous(breaks = c(1,5), labels = letters[c(1,5)], limits = c(2, 4))
  bi <- sc$break_info()
  expect_length(bi$labels, 0)
  expect_length(bi$major, 0)
  expect_length(bi$major_source, 0)

  # limits aren't specified, automatic labels
  # limits are set by the data
  sc <- scale_x_continuous(breaks = 1:5)
  sc$train_df(data_frame(x = 2:4))
  bi <- sc$break_info()
  expect_equal(bi$labels, as.character(2:4))
  expect_equal(bi$major_source, 2:4)
  expect_equal(bi$major, c(0, 0.5, 1))

  # Limits and labels are specified
  sc <- scale_x_continuous(breaks = 1:5, labels = letters[1:5])
  sc$train_df(data_frame(x = 2:4))
  bi <- sc$break_info()
  expect_equal(bi$labels, letters[2:4])
  expect_equal(bi$major_source, 2:4)
  expect_equal(bi$major, c(0, 0.5, 1))

  # Limits aren't specified, and all breaks are out of range of data
  sc <- scale_x_continuous(breaks = c(1,5), labels = letters[c(1,5)])
  sc$train_df(data_frame(x = 2:4))
  bi <- sc$break_info()
  expect_length(bi$labels, 0)
  expect_length(bi$major, 0)
  expect_length(bi$major_source, 0)
})

test_that("no minor breaks when only one break", {
  sc1 <- scale_x_discrete(limits = "a")
  sc2 <- scale_x_continuous(limits = c(1, 1))

  expect_length(sc1$get_breaks_minor(), 0)
  expect_length(sc2$get_breaks_minor(), 0)
})

init_scale <- function(...) {
  sc <- scale_x_discrete(...)
  sc$train(factor(1:100))
  expect_length(sc$get_limits(), 100)
  sc
}

test_that("discrete labels match breaks", {

  sc <- init_scale(breaks = 0:5 * 10)
  expect_length(sc$get_breaks(), 5)
  expect_length(sc$get_labels(), 5)
  expect_equal(sc$get_labels(), sc$get_breaks(), ignore_attr = TRUE)

  sc <- init_scale(breaks = 0:5 * 10, labels = letters[1:6])
  expect_length(sc$get_breaks(), 5)
  expect_length(sc$get_labels(), 5)
  expect_equal(sc$get_labels(), letters[2:6])

  sc <- init_scale(breaks = 0:5 * 10, labels =
    function(x) paste(x, "-", sep = ""))
  expect_equal(sc$get_labels(), c("10-", "20-", "30-", "40-", "50-"))

  pick_5 <- function(x) sample(x, 5)
  sc <- init_scale(breaks = pick_5)
  expect_length(sc$get_breaks(), 5)
  expect_length(sc$get_labels(), 5)
})

test_that("scale breaks work with numeric log transformation", {
  sc <- scale_x_continuous(limits = c(1, 1e5), transform = transform_log10())
  expect_equal(sc$get_breaks(), c(0, 2, 4)) # 1, 100, 10000
  expect_equal(sc$get_breaks_minor(), c(0, 1, 2, 3, 4, 5))
})

test_that("continuous scales with no data have no breaks or labels", {
  sc <- scale_x_continuous()

  expect_equal(sc$get_breaks(), numeric())
  expect_equal(sc$get_labels(), character())
  expect_equal(sc$get_limits(), c(0, 1))
})

test_that("discrete scales with no data have no breaks or labels", {
  sc <- scale_x_discrete()

  expect_equal(sc$get_breaks(), numeric())
  expect_equal(sc$get_labels(), character())
  expect_equal(sc$get_limits(), c(0, 1))
})

test_that("passing continuous limits to a discrete scale generates a warning", {
  expect_snapshot_warning(scale_x_discrete(limits = 1:3))
})

test_that("suppressing breaks, minor_breask, and labels works", {
  expect_null(scale_x_continuous(breaks = NULL, limits = c(1, 3))$get_breaks())
  expect_null(scale_x_discrete(breaks = NULL, limits = c("one", "three"))$get_breaks())
  expect_null(scale_x_continuous(minor_breaks = NULL, limits = c(1, 3))$get_breaks_minor())

  expect_null(scale_x_continuous(labels = NULL, limits = c(1, 3))$get_labels())
  expect_null(scale_x_discrete(labels = NULL, limits = c("one", "three"))$get_labels())

  # date, datetime
  lims <- as.Date(c("2000/1/1", "2000/2/1"))
  expect_null(scale_x_date(breaks = NULL, limits = lims)$get_breaks())
  # NA is defunct, should throw error
  expect_snapshot(
    scale_x_date(breaks = NA, limits = lims)$get_breaks(),
    error = TRUE
  )
  expect_null(scale_x_date(labels = NULL, limits = lims)$get_labels())
  expect_snapshot(
    scale_x_date(labels = NA, limits = lims)$get_labels(),
    error = TRUE
  )
  expect_null(scale_x_date(minor_breaks = NULL, limits = lims)$get_breaks_minor())
  expect_snapshot(
    scale_x_date(minor_breaks = NA, limits = lims)$get_breaks_minor(),
    error = TRUE
  )

  # date, datetime
  lims <- as.POSIXct(c("2000/1/1 0:0:0", "2010/1/1 0:0:0"))
  expect_null(scale_x_datetime(breaks = NULL, limits = lims)$get_breaks())
  expect_snapshot(
    scale_x_datetime(breaks = NA, limits = lims)$get_breaks(),
    error = TRUE
  )
  expect_null(scale_x_datetime(labels = NULL, limits = lims)$get_labels())
  expect_snapshot(
    scale_x_datetime(labels = NA, limits = lims)$get_labels(),
    error = TRUE
  )
  expect_null(scale_x_datetime(minor_breaks = NULL, limits = lims)$get_breaks_minor())
  expect_snapshot(
    scale_x_datetime(minor_breaks = NA, limits = lims)$get_breaks_minor(),
    error = TRUE
  )
})

test_that("scale_breaks with explicit NA options (deprecated)", {
  # NA is defunct, should throw error
  expect_error(scale_x_continuous(breaks = NA))
  expect_error(scale_y_continuous(breaks = NA))
  expect_error(scale_alpha_continuous(breaks = NA))
  expect_error(scale_size_continuous(breaks = NA))
  expect_error(scale_fill_continuous(breaks = NA))
  expect_error(scale_colour_continuous(breaks = NA))
})

test_that("breaks can be specified by names of labels", {
  labels <- setNames(LETTERS[1:4], letters[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = labels)
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), LETTERS[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = rev(labels))
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), LETTERS[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = labels[1:2])
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), c("A", "B", "c", "d"))

  s <- scale_x_discrete(limits = letters[1:4], labels = labels[3:4])
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), c("a", "b", "C", "D"))

  s <- scale_x_discrete(limits = letters[1:3], labels = labels)
  expect_equal(as.vector(s$get_breaks()), letters[1:3])
  expect_equal(as.vector(s$get_labels()), LETTERS[1:3])
})

test_that("only finite or NA values for breaks for transformed scales (#871)", {
  sc <- scale_y_continuous(limits = c(0.01, 0.99), transform = "probit",
                           breaks = seq(0, 1, 0.2))
  breaks <- sc$break_info()$major_source
  expect_true(all(is.finite(breaks) | is.na(breaks)))
})

test_that("minor breaks are transformed by scales", {
  sc <- scale_y_continuous(limits = c(1, 100), transform = "log10",
    minor_breaks = c(1, 10, 100))

  expect_equal(sc$get_breaks_minor(), c(0, 1, 2))
})

test_that("continuous limits accepts functions", {
  p <- ggplot(mpg, aes(class, hwy)) +
    scale_y_continuous(limits = function(lims) (c(lims[1] - 10, lims[2] + 100)))

  expect_equal(
    get_panel_scales(p)$y$get_limits(),
    c(range(mpg$hwy)[1] - 10, range(mpg$hwy)[2] + 100)
  )
})

test_that("equal length breaks and labels can be passed to ViewScales with limits", {

  test_scale <- scale_x_continuous(
    breaks = c(0, 20, 40),
    labels = c("0", "20", "40"),
    limits = c(10, 30)
  )

  expect_identical(test_scale$get_breaks(), c(0, 20, 40))
  expect_identical(test_scale$get_labels(), c(c("0", "20", "40")))

  test_view_scale <- view_scale_primary(test_scale)
  expect_identical(test_view_scale$get_breaks(), c(NA, 20, NA))
  expect_identical(test_view_scale$get_labels(), c(c("0", "20", "40")))

  # ViewScale accepts the limits in the opposite order (#3952)
  test_view_scale_rev <- view_scale_primary(test_scale, limits = rev(test_scale$get_limits()))
  expect_identical(test_view_scale_rev$get_breaks(), c(NA, 20, NA))
  expect_identical(test_view_scale_rev$get_labels(), c(c("0", "20", "40")))
})

test_that("break names are returned as labels", {

  sc <- scale_x_continuous(breaks = c(A = 10, B = 20, C = 30))
  sc$train(c(10, 30))
  expect_equal(sc$get_labels(), c("A", "B", "C"))

  sc <- scale_x_discrete(breaks = c(foo = "A", bar = "B", qux = "C"))
  sc$train(c(LETTERS[1:3]))
  expect_equal(sc$get_labels(), c("foo", "bar", "qux"))
})

# Visual tests ------------------------------------------------------------

test_that("minor breaks draw correctly", {
  df <- data_frame(
    x_num = c(1, 3),
    x_chr = c("a", "b"),
    x_date = as.Date("2012-2-29") + c(0, 100),
    x_log = c(1, 1e4),
    y = c(1, 3)
  )
  theme <- theme_test() +
    theme(
      panel.grid.major = element_line(colour = "grey30", linewidth = 0.5),
      panel.grid.minor = element_line(colour = "grey70")
    )

  p <- ggplot(df, aes(x_num, y)) +
    geom_blank() +
    scale_x_continuous(breaks = 1:3, minor_breaks = c(1.25, 2.75)) +
    scale_y_continuous(breaks = 1:3, minor_breaks = c(1.25, 2.75)) +
    labs(x = NULL, y = NULL) +
    theme
  expect_doppelganger("numeric", p)
  expect_doppelganger("numeric-polar", p + coord_polar())

  expect_doppelganger("numeric-log",
    ggplot(df, aes(x_log, x_log)) +
      scale_x_continuous(transform = transform_log2()) +
      scale_y_log10() +
      labs(x = NULL, y = NULL) +
      theme
  )
  expect_doppelganger("numeric-exp",
    ggplot(df, aes(x_num, x_num)) +
      scale_x_continuous(transform = transform_exp(2)) +
      scale_y_continuous(transform = transform_exp(2)) +
      labs(x = NULL, y = NULL) +
      theme
  )

  expect_doppelganger("character",
    ggplot(df, aes(x_chr, y)) +
      geom_blank() +
      labs(x = NULL, y = NULL) +
      theme
  )

  expect_doppelganger("date",
    ggplot(df, aes(x_date, y)) +
      geom_blank() +
      scale_x_date(
        labels = scales::label_date("%m/%d"),
        breaks = scales::date_breaks("month"),
        minor_breaks = scales::date_breaks("week")
      ) +
      labs(x = NULL, y = NULL) +
      theme
  )
})

test_that("scale breaks can be removed", {
  dat <- data_frame(x = 1:3, y = 1:3)

  expect_doppelganger("no x breaks",
    ggplot(dat, aes(x = x, y = y)) + geom_point() + scale_x_continuous(breaks = NULL)
  )
  expect_doppelganger("no y breaks",
    ggplot(dat, aes(x = x, y = y)) + geom_point() + scale_y_continuous(breaks = NULL)
  )
  expect_doppelganger("no alpha breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, alpha = x)) + geom_point() + scale_alpha_continuous(breaks = NULL)
  )
  expect_doppelganger("no size breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, size = x)) + geom_point() + scale_size_continuous(breaks = NULL)
  )
  expect_doppelganger("no fill breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, fill = x)) + geom_point(shape = 21) + scale_fill_continuous(breaks = NULL)
  )
  expect_doppelganger("no colour breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, colour = x)) + geom_point() + scale_colour_continuous(breaks = NULL)
  )
})

test_that("functional limits work for continuous scales", {
  limiter <- function(by) {
    function(limits) {
      low <- floor(limits[1] / by) * by
      high <- ceiling(limits[2] / by) * by
      c(low, high)
    }
  }

  expect_doppelganger(
    "functional limits",
    ggplot(mpg, aes(class)) + geom_bar(aes(fill = drv)) + scale_y_continuous(limits = limiter(50))
  )
})

test_that("limits are squished to transformation domain", {
  # Breaks should not be calculated on ranges outside domain #980
  sc1 <- scale_x_sqrt()
  sc2 <- scale_x_sqrt()
  sc3 <- scale_x_reverse(breaks = 1:9) # Test for #4858

  sc1$train(c(0, 10))
  sc2$train(c(-10, 10))
  sc3$train(c(0, -10)) # training expects transformed input

  expect_equal(sc1$get_breaks(), sc2$get_breaks())
  expect_equal(sc2$get_breaks()[1], 0)
  expect_equal(sc3$get_breaks(), -1:-9)
})
