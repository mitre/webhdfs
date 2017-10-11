context("settings")

test_that("setting webhdfs port works", {

  port_in <- "50070"

  expect_silent(set_webhdfs_port(port_in))
  expect_silent({port_out <- get_webhdfs_port()})
  expect_equal(port_out, port_in)
})


test_that("setting webhdfs suffix works", {

  suffix_in <- "webhdfs/v1"

  expect_silent(set_webhdfs_suffix(suffix_in))
  expect_silent({suffix_out <- get_webhdfs_suffix()})
  expect_equal(suffix_out, suffix_in)
})


test_that("setting namenode url works with single namenode", {

  url_in <- "http://nn1.server.domain"

  expect_silent(set_name_node_url(url_in))
  expect_silent({url_out <- get_name_node_url()})
  expect_equal(url_out, url_in)
})


test_that("setting namenode url works with two namenodes", {

  url_in <- c("http://nn1.server.domain", "http://nn2.server.domain")

  expect_silent(set_name_node_url(url_in))
  expect_silent({url_out <- get_name_node_url()})
  expect_equal(url_out, url_in)
})


test_that("setting cluster works", {

  cluster_in <- "dummy cluster name"

  expect_silent(set_cluster(cluster_in))
  expect_silent({cluster_out <- get_cluster()})
  expect_equal(cluster_out, cluster_in)
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


