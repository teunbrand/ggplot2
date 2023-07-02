
test_that("partial scales throw appropriate errors", {

  # Fine nothing to update
  expect_null(scale_partial())

  # Unnamed arguments
  expect_snapshot_error(scale_partial(1, 2, 3))

  # Duplicated arguments
  expect_snapshot_error(
    scale_partial(limits = c(1, 10), limits = c(2, 10), breaks = 1, breaks = 2)
  )

  # No shenanigans with aesthetics
  expect_snapshot_error(
    scale_partial(list(), name = "foobar")
  )
})

test_that("partial scales can be updated", {

  # Partial scales
  xscale1 <- scale_x("foobar", limits = c(0, 10))
  xscale2 <- scale_x(limits = c(0, 1))

  # Plot with partial scale
  plot1 <- ggplot() + xscale1

  # Check that partial scale lives in the ScalesList
  expect_equal(
    plot1$scales$get_scales("x")$params,
    list(name = "foobar", limits = c(0, 10))
  )
  expect_s3_class(
    plot1$scales$get_scales("x"),
    "ScalePartial"
  )

  # Update scale with xscale2
  plot2 <- plot1 + xscale2

  # Check for successful update
  expect_equal(
    plot2$scales$get_scales("x")$params,
    list(limits = c(0, 1), name = "foobar")
  )

  # Check that we haven't changed the state of plot1
  expect_equal(
    plot1$scales$get_scales("x")$params,
    list(name = "foobar", limits = c(0, 10))
  )
  # Check that we haven't changed the state of xscale1
  expect_equal(xscale1$params, list(name = "foobar", limits = c(0, 10)))
  # Check that we haven't changed the state of xscale2
  expect_equal(xscale2$params, list(limits = c(0, 1)))

  # Add default scales
  plot2$scales$add_missing(c("x", "y"), env = current_env())

  # Check default scale is successfully updated
  new <- plot2$scales$get_scales("x")
  expect_s3_class(new, "ScaleContinuousPosition")
  expect_equal(new$name, "foobar")
  expect_equal(new$limits, c(0, 1))
})

test_that("partial scale input is checked as valid fields", {

  # Check nonsense is warned about
  p <- ggplot() + scale_x(foo = "bar", limits = c(0, 1))
  expect_snapshot_warning(
    p$scales$add_missing(c("x", "y"), env = current_env())
  )

  # Check incompatible arguments
  p <- ggplot() + scale_x(breaks = c(1, 2), labels = c("A", "B", "C"))
  expect_snapshot_error(
    p$scales$add_missing(c("x", "y"), env = current_env())
  )

  # No continuous limits to discrete scales
  expect_snapshot_warning(
    p <- ggplot() + scale_x(limits = c(0, 10)) + scale_x_discrete()
  )
})


