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
  sample_ls_result <- dplyr::as.tbl(sample_ls_result)

  expect_silent({dat <- clean_liststatus_columns(sample_ls_result)})
  expect_equal(dim(dat), c(1,8))
  expect_equal(names(dat), c("pathSuffix", "childrenNum", "length", "group",
                             "modificationTime", "owner", "permission", "type"))


})


# TODO: test combinations of recursion and concise parameters

# TODO: test that recursion works with empty sub-directories
