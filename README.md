
webhdfs
=======

[![Build Status](https://travis-ci.org/mitre/webhdfs.svg?branch=master)](https://travis-ci.org/mitre/webhdfs) [![Coverage Status](https://codecov.io/gh/mitre/webhdfs/branch/master/graph/badge.svg)](https://codecov.io/github/mitre/webhdfs?branch=master)

Provide an interface to WebHDFS operations by leveraging configuration details from the `clusterconf` package. This includes functions to facilitate listing directory contents, creating, renaming, or deleting directories. These common commands have corresponding shortcut functions in this package (e.g. `hdfs_ls`, `hdfs_makedir`, `hdfs_rename`, `hdfs_delete`).

For WebHDFS operations that do not have corresponding functions in this package, it is possible to issue many of those commands through the lower-level functions `hdfs_get` and `hdfs_put`.

Requirements
------------

This package dynamically builds the URL for WebHDFS services based on cluster settings for namenode, WebHDFS port number, and suffix. Specifically, the settings `webhdfs.cluster.nn.url`, `webhdfs.cluster.webhdfs.port`, and `webhdfs.cluster.webhdfs.suffix` must be defined. It is convenient to do this through the [`clusterconf`](https://mitre.github.io/clusterconf/) package, although it also possible to set the necessary variables manually through the `set_name_node_url`, `set_webhdfs_port`, and `set_webhdfs_suffix` functions provided in this package.

Note that the namenode setting (`webhdfs.cluster.nn.url`) is a character array of length one or more. When the WebHDFS URL is built, the `get_webhdfs_url` function dynamically checks the provided namenodes for one that is active. If the Hadoop cluster is configured with [high availability through namenode failover](https://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/HDFSHighAvailabilityWithNFS.html), one namenode is active and one is in standby mode at all times. Providing both URLs in the namenode setting protects the user from having to know which one is active at the time of the WebHDFS request.

Other settings are expected to be a character array of length one.

Installation
------------

The easiest way is to install the latest development version from GitHub, for example with the `devtools` package.

``` r
devtools::install_github("mitre/webhdfs")
```

Usage
-----

Here is a simple example to get up and running with this package, where `my cluster name` corresponds to a HDFS cluster whose configuration is defined using the protocol of the `clusterconf` package.

``` r
library(webhdfs)
set_cluster("my cluster name")
hdfs_ls("/data/")
```

The above example requires that a `clusterconf` package for `my cluster name` (e.g. `clusterconf.myclustername`) exists and has already been installed. If such a package does not exist, the same capability can be achieved manually by setting the name node url(s) as shown below. Suppose that my cluster has its primary namenode at `mycluster-nn1.mydomain.com` and its backup namenode at `mycluster-nn2.mydomain.com`. (Also assume that WebHDFS is enabled at the default port `50070` with the standard URL suffix `webhdfs/v1`.) Then the following commands will reproduce the capability of the block above.

``` r
library(webhdfs)
set_name_node_url(c("http://mycluster-nn1.mydomain.com",
                    "http://mycluster-nn2.mydomain.com"))
hdfs_ls("/data/")
```
