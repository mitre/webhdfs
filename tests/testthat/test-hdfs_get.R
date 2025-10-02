context("hdfs_get_put")


test_that("hdfs_get works with GETFILESTATUS", {
  # TODO
})


test_that("hdfs_get works with LISTSTATUS and valid path", {
  with_mocked_bindings(
    {
      expect_silent({dat <- hdfs_get("/", "LISTSTATUS")})
      expect_equal(dim(dat), c(2, 13))
    },

    # mockup exception error when file not found
    get_url_content = function(x, ...) {
      return(
        paste0(
          "{\"FileStatuses\":{\"FileStatus\":[\n{\"accessTime\":0,\"blockSize\":0,\"childrenNum\":1,",
          "\"fileId\":31815,\"group\":\"hdfs\",\"length\":0,\"modificationTime\":1435144867119,\"owner\":\"hdfs\",",
          "\"pathSuffix\":\"db\",\"permission\":\"755\",\"replication\":0,\"storagePolicy\":0,\"type\":\"DIRECTORY\"},\n",
          "{\"accessTime\":0,\"blockSize\":0,\"childrenNum\":21,\"fileId\":31813,\"group\":\"hdfs\",\"length\":0,",
          "\"modificationTime\":1495825848496,\"owner\":\"hdfs\",\"pathSuffix\":\"raw\",\"permission\":\"755\",",
          "\"replication\":0,\"storagePolicy\":0,\"type\":\"DIRECTORY\"}\n]}}\n"
        )
      )},
    get_webhdfs_url = function() "http://fakeurl"
  )
})


test_that("hdfs_get throws warning with invalid path", {
  with_mocked_bindings(
    expect_warning(hdfs_get("/", "LISTSTATUS")),

    # mockup exception error when file not found
    get_url_content = function(x, ...) {
      return(
        paste0(
          "{\"RemoteException\":{\"exception\":\"FileNotFoundException\",",
          "\"javaClassName\":\"java.io.FileNotFoundException\",",
          "\"message\":\"File /dummy/ does not exist.\"}}"
        )
      )},
    get_webhdfs_url = function() "http://fakeurl",
  )
})



test_that("hdfs_get returns the expected data type", {
# TODO


})

