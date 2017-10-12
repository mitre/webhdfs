context("hdfs_get_put")


test_that("hdfs_get works with GETFILESTATUS", {
  # TODO
})



test_that("hdfs_get works with LISTSTATUS and valid path", {
  library(httr)

  with_mock(
    # mockup exception error when file not found
    `httr::content` = function(x, ...) {
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
    get_webhdfs_url = function() "http://fakeurl",
    expect_silent({dat <- hdfs_get("/", "LISTSTATUS")}),
    expect_equal(dim(dat), c(2, 13)),
    .env = "webhdfs"
  )
})


test_that("hdfs_get throws warning with invalid path", {

  library(httr)

  with_mock(
    # mockup exception error when file not found
    `httr::content` = function(x, ...) {
      return(
        paste0(
          "{\"RemoteException\":{\"exception\":\"FileNotFoundException\",",
          "\"javaClassName\":\"java.io.FileNotFoundException\",",
          "\"message\":\"File /dummy/ does not exist.\"}}"
        )
      )},
    get_webhdfs_url = function() "http://fakeurl",
    expect_warning(hdfs_get("/")),
    .env = "webhdfs"
  )

})



test_that("hdfs_get returns the expected data type", {
# TODO


})

