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



test_that("is_namenode_active gives helpful error message when WebHDFS is not enabled on requested url", {
  library(httr)

  with_mock(
    # mockup exception error from standby
    `httr::content` = function(x, ...) {
      return(
        paste0(
          '<html>\n<head>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\"/>\n',
          '<title>Error 404 NOT_FOUND</title>\n</head>\n<body><h2>HTTP ERROR 404</h2>\n',
          '<p>Problem accessing /webhdfs/v1/. Reason:\n<pre>    NOT_FOUND</pre></p><hr />'
        )
      )},

    expect_error(is_namenode_active("http://fakeurl"),
                 "WebHDFS is not available at the requested url")
  )
})



test_that("get_webhdfs_url works with single namenode", {
  # TODO
})


test_that("get_webhdfs_url works with array of namenodes", {
  # TODO
})

