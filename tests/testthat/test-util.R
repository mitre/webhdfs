context("util functions")

test_that("hdfs_timestamp_to_posix works as intended", {
  ts <- 1489039200390

  expect_equal(hdfs_timestamp_to_posix(ts),
               as.POSIXct(ts/1000, tz = "UTC", origin = "1970-01-01", format = "%s"))
})

test_that("format_return works with data.frame", {
  dat_in <- data.frame(x = 1:5)

  dat_out <- webhdfs:::format_return(dat_in, "data.frame")
  expect_equal(dat_out, dat_in)
  expect_equal(class(dat_out), "data.frame")
})

test_that("format_return works with data.table", {
  dat_in <- data.table::data.table(x = 1:5)

  dat_out <- webhdfs:::format_return(dat_in, "data.table")
  expect_equal(dat_out, dat_in)
  expect_true("data.table" %in% class(dat_out))
})

test_that("format_return works with tibble", {
  dat_in <- tibble::tibble(x = 1:5)

  dat_out <- webhdfs:::format_return(dat_in, "tbl")
  expect_equal(dat_out, dat_in)
  expect_true("tbl" %in% class(dat_out))
})

test_that("format_return works with something else", {
  dat_in <- data.frame(x = 1:5)

  dat_out <- webhdfs:::format_return(dat_in, "matrix")
  expect_equal(dat_out, as.matrix(dat_in))
  expect_equal(class(dat_out), "matrix")
})

