test_that(".onAttach does not modify the random stream", {
  set.seed(42)
  x <- runif(5)
  set.seed(42)
  .onAttach()
  expect_equal(runif(5), x)
})

test_that("No graphics device has been opened", {
  # This test needs to remain the very last test.
  # If any of the tests has opened a graphics device without closing it,
  # this test should fail.
  #
  # You may look for the following suspects:
  # * `plot()` or `print()`. These can often be replaced by `ggplotGrob()` if
  #   not actually testing drawn plot output.
  # * Unit conversions or measurements, like `width_cm()` or
  #   `grid::convertUnit()`. These open graphics devices which needs to be
  #   closed as well. You can use the `local_graphics_device()` helper.

  expect_equal(dev.cur(), 1L, ignore_attr = "names")
})
