context("settings")

test_that("setting webhdfs port works", {

  port_in <- "1234"

  expect_silent(set_webhdfs_port(port_in))
  expect_silent({port_out <- get_webhdfs_port()})
  expect_equal(port_out, port_in)
})


test_that("resetting cache works", {
  port_in <- "5678"
  set_webhdfs_port(port_in)

  # return value is as expected
  port_out <- get_webhdfs_port()
  expect_equal(port_out, port_in)

  # attempting to get throws an error
  reset_cache()
  expect_error({get_webhdfs_port()},
               "The required setting 'webhdfs.cluster.webhdfs.port' is not set.")
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
  library(clusterconf)

  cluster_in <- "dummy cluster name"

  with_mocked_bindings(
    expect_silent(set_cluster(cluster_in)),

    # mockup valid response from active namenode
    get_cluster_configs = function(x, ...) {
      return("dummy")
    }
  )

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


test_that("setting user works", {

  user_in <- "user1"

  expect_silent(set_user(user_in))
  expect_silent({user_out <- get_user()})
  expect_equal(user_out, user_in)
})


test_that("guess user works", {

  expected_user <- ifelse(.Platform$OS.type=="windows",
                          tolower(Sys.getenv("USERNAME")),
                          tolower(Sys.getenv("USER")))

  expect_equal(guess_user(), expected_user)
})

test_that("set_user guesses the user if not provided", {
  expected_user <- ifelse(.Platform$OS.type=="windows",
                          tolower(Sys.getenv("USERNAME")),
                          tolower(Sys.getenv("USER")))

  expect_silent(set_user())
  expect_silent({user_out <- get_user()})
  expect_equal(user_out, expected_user)
})


test_that("set return type works", {

  return_type_in <- "dummy.type"

  with_mocked_bindings(
    {
      expect_silent(set_return_type(return_type_in))
      expect_silent({return_type_out <- get_return_type()})
      expect_equal(return_type_in, return_type_out)
    },

    is_supported_return_type = function(x) TRUE
  )

  # reset to default
  set_return_type("data.frame")
})


test_that("set return type throws error with invalid input", {
  expect_error(set_return_type("dummy.type"),
               "The return type options are")
})


test_that("setter is referenced correctly by get function error message", {
  expect_error(get_name_node_url(),
               "set the parameter manually using the '[\\w]+' function", perl = TRUE)
})


test_that("default return type works", {
  expect_equal(get_return_type(), "data.frame")
})


test_that("cluster config port and suffix settings get used properly", {
  # no value to start
  reset_cache()
  expect_error({get_webhdfs_port()},
               "The required setting 'webhdfs.cluster.webhdfs.port' is not set.")
  expect_error({get_webhdfs_suffix()},
               "The required setting 'webhdfs.cluster.webhdfs.suffix' is not set.")

  # setup test cluster config
  tmp_config_fn <- tempfile(fileext = ".yaml")
  tmp_config_contents <- '
default:
  webhdfs:
    port: "9876"
    suffix: "webhdfs/v1"
'
  cat(tmp_config_contents, file = tmp_config_fn)
  set_cluster("test", tmp_config_fn)

  # test port and suffix again
  expect_equal(get_webhdfs_port(), "9876")
  expect_equal(get_webhdfs_suffix(), "webhdfs/v1")
})


test_that("SSL verification is used by default", {
  # no value to start
  reset_cache()
  expect_true({get_ssl_verify()})
})


test_that("turning off SSL verification works", {
  # no value to start
  reset_cache()

  expect_silent({set_ssl_verify(FALSE)})
  expect_false({get_ssl_verify()})
})
