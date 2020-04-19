context("namenode_utilities")

test_that("standby namenode is detected correctly", {
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
  with_mock(
    # mockup valid response from active namenode
    `httr::content` = function(x, ...) {
      return("{\"FileStatuses\":{\"FileStatus\":[\n\n]}}\n")
      },

    expect_true(is_namenode_active("http://fakeurl"))
  )
})



test_that("is_namenode_active gives helpful error message when WebHDFS is not enabled on requested url", {
  with_mock(
    # mockup 404 error from invalid webhdfs server
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
  reset_cache()

  # mockup relevant cluster config settings and namenode test
  with_mock(
    get_name_node_url = function() "http://nn1.server.domain",
    get_webhdfs_port = function() "50070",
    get_webhdfs_suffix = function() "webhdfs/v1",
    is_namenode_active = function(x) TRUE,
    expect_equal("http://nn1.server.domain:50070/webhdfs/v1",
                 get_webhdfs_url()),
    .env = "webhdfs"
  )
})


test_that("get_webhdfs_url works with array of namenodes", {
  reset_cache()

  # mockup relevant cluster config settings and namenode test
  with_mock(
    get_name_node_url = function() c("http://nn1.server.domain",
                                     "http://nn2.server.domain"),
    get_webhdfs_port = function() "50070",
    get_webhdfs_suffix = function() "webhdfs/v1",
    is_namenode_active = function(x) TRUE,
    expect_equal("http://nn1.server.domain:50070/webhdfs/v1",
                 get_webhdfs_url()),
    .env = "webhdfs"
  )
})


test_that("get_webhdfs_url works with an inactive namenode", {
  reset_cache()

  # mockup relevant cluster config settings and namenode test
  with_mock(
    get_name_node_url = function() "http://nn1.server.domain",
    get_webhdfs_port = function() "50070",
    get_webhdfs_suffix = function() "webhdfs/v1",
    is_namenode_active = function(x) FALSE,
    expect_warning(get_webhdfs_url(),
                   "No active namenodes"),
    .env = "webhdfs"
  )
})

