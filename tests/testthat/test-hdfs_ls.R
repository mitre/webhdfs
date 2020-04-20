context("hdfs_ls")

test_that("clean_liststatus_columns works with data.frame", {

  sample_ls_result <- as.data.frame(matrix(ncol = 13, nrow = 1))
  colnames(sample_ls_result) <- c("accessTime", "blockSize", "childrenNum", "fileId", "group",
                                  "length", "modificationTime", "owner", "pathSuffix", "permission",
                                  "replication", "storagePolicy", "type")

  expect_silent({dat <- clean_liststatus_columns(sample_ls_result)})
  expect_equal(dim(dat), c(1,8))
  expect_equal(names(dat), c("pathSuffix", "childrenNum", "length", "group",
                             "modificationTime", "owner", "permission", "type"))
})


test_that("clean_liststatus_columns works with data.table", {

  sample_ls_result <- as.data.frame(matrix(ncol = 13, nrow = 1))
  colnames(sample_ls_result) <- c("accessTime", "blockSize", "childrenNum", "fileId", "group",
                                  "length", "modificationTime", "owner", "pathSuffix", "permission",
                                  "replication", "storagePolicy", "type")
  data.table::setDT(sample_ls_result)

  expect_silent({dat <- clean_liststatus_columns(sample_ls_result)})
  expect_equal(dim(dat), c(1,8))
  expect_equal(names(dat), c("pathSuffix", "childrenNum", "length", "group",
                             "modificationTime", "owner", "permission", "type"))

})


test_that("clean_liststatus_columns works with tbl", {

  sample_ls_result <- as.data.frame(matrix(ncol = 13, nrow = 1))
  colnames(sample_ls_result) <- c("accessTime", "blockSize", "childrenNum", "fileId", "group",
                                  "length", "modificationTime", "owner", "pathSuffix", "permission",
                                  "replication", "storagePolicy", "type")
  sample_ls_result <- tibble::as_tibble(sample_ls_result)

  expect_silent({dat <- clean_liststatus_columns(sample_ls_result)})
  expect_equal(dim(dat), c(1,8))
  expect_equal(names(dat), c("pathSuffix", "childrenNum", "length", "group",
                             "modificationTime", "owner", "permission", "type"))
})


# TODO: test combinations of recursion and concise parameters

# TODO: test that recursion works with empty sub-directories

# TODO: test that ls works with and without wildcards


test_that("hdfs_ls converts time fields", {
  with_mock(
    hdfs_get = function(path, operation, user = get_user(),
                        return_type = get_return_type(), handler = NULL) {
      structure(list(accessTime = c(0L, 0L, 0L),
                     blockSize = c(0L, 0L, 0L),
                     childrenNum = c(12L, 21L, 3L),
                     fileId = c(2652700L, 2652939L, 11312866L),
                     group = c("test", "test", "test"),
                     length = c(0L, 0L, 0L),
                     modificationTime = c(1539712265888, 1539712482283, 1587281681146),
                     owner = c("dummy", "dummy", "dummy"),
                     pathSuffix = c("derived", "raw", "tmp"),
                     permission = c("755", "755", "755"),
                     replication = c(0L, 0L, 0L),
                     storagePolicy = c(0L, 0L, 0L),
                     type = c("DIRECTORY", "DIRECTORY", "DIRECTORY")),
                class = "data.frame",
                row.names = c(NA, -3L))
    },
    expect_equal(hdfs_ls("/data/")$modificationTime,
                 hdfs_timestamp_to_posix(c(1539712265888, 1539712482283, 1587281681146)))
  )
})

test_that("hdfs_ls optionally returns a concise format", {
  with_mock(
    hdfs_get = function(path, operation, user = get_user(),
                        return_type = get_return_type(), handler = NULL) {
      structure(list(accessTime = c(0L, 0L, 0L),
                     blockSize = c(0L, 0L, 0L),
                     childrenNum = c(12L, 21L, 3L),
                     fileId = c(2652700L, 2652939L, 11312866L),
                     group = c("test", "test", "test"),
                     length = c(0L, 0L, 0L),
                     modificationTime = c(1539712265888, 1539712482283, 1587281681146),
                     owner = c("dummy", "dummy", "dummy"),
                     pathSuffix = c("derived", "raw", "tmp"),
                     permission = c("755", "755", "755"),
                     replication = c(0L, 0L, 0L),
                     storagePolicy = c(0L, 0L, 0L),
                     type = c("DIRECTORY", "DIRECTORY", "DIRECTORY")),
                class = "data.frame",
                row.names = c(NA, -3L))
    },
    expect_equal(dim(hdfs_ls("/data/", concise = TRUE)),
                 c(3, 8))
  )
})

