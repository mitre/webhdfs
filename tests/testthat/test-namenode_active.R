context("namenode_utilities")

test_that("standby namenode is detected correctly", {
  library(httr)

  with_mock(
    # mockup exception error from standby
    `httr::content` = function(x, ...) {
      return(
        paste0(
          "{\"RemoteException\":{\"exception\":\"StandbyException\",",
          "\"javaClassName\":\"org.apache.hadoop.ipc.StandbyException\",",
          "\"message\":\"Operation category READ is not supported in state standby\"}}"
        )
      )},

    expect_false(is_namenode_active("http://fakeurl"))
  )
})


test_that("active namenode is detected correctly", {
  library(httr)

  with_mock(
    # mockup valid response from active namenode
    `httr::content` = function(x, ...) {
      return("{\"FileStatuses\":{\"FileStatus\":[\n\n]}}\n")
      },

    expect_true(is_namenode_active("http://fakeurl"))
  )
})




test_that("get_webhdfs_url works with single namenode", {
  # TODO
})


test_that("get_webhdfs_url works with array of namenodes", {
  # TODO
})

