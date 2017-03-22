context("settings")

test_that("setting webhdfs port works", {

  port_in <- "50070"

  expect_silent(set_webhdfs_port(port_in))
  expect_silent({port_out <- get_webhdfs_port()})
  expect_equal(port_out, port_in)
})


test_that("webhdfs port is unchanged by initial setting of return type", {

  # set port
  port <- "50070"
  set_webhdfs_port(port)

  # set return type
  set_return_type("data.frame")
  expect_equal(get_return_type(), "data.frame")

  # port is as expected
  expect_equal(get_webhdfs_port(), port)
})



test_that("webhdfs port is unchanged by changing a previous setting of return type", {

  # set port
  port <- "50070"
  set_webhdfs_port(port)

  # set return type
  set_return_type("data.frame")
  expect_equal(get_return_type(), "data.frame")

  # port is as expected
  expect_equal(get_webhdfs_port(), port)

  # set different return type
  set_return_type("data.table")
  expect_equal(get_return_type(), "data.table")

  # port is still as expected
  expect_equal(get_webhdfs_port(), port)
})


