WebHDFS
=======

[![Bamboo Build Status](https://pandafood.mitre.org/plugins/servlet/wittified/build-status/CGRP-WEBHDFSM)](https://pandafood.mitre.org/browse/CGRP-WEBHDFSM) [![Test Coverage](https://img.shields.io/badge/Test%20Coverage-16%25-red.svg)](https://pandafood.mitre.org/artifact/CGRP-WEBHDFSM/JOB1/build-latest/covr_coverage_report/covr_report.html)

Provide an interface to WebHDFS operations by leveraging configuration details from the `clusterconf` package. This includes functions to facilitate listing directory contents, creating, renaming, or deleting directories. These common commands have corresponding shortcut functions in this package (e.g. `hdfs_ls`, `hdfs_makedir`, `hdfs_rename`, `hdfs_delete`).

For WebHDFS operations that do not have corresponding functions in this package, it is possible to issue many of those commands through the lower-level functions `hdfs_get` and `hdfs_put`.

Requirements
------------

This package dynamically builds the URL for WebHDFS services based on cluster settings for namenode, WebHDFS port number, and suffix. Specifically, the settings `webhdfs.cluster.nn.url`, `webhdfs.cluster.webhdfs.port`, and `webhdfs.cluster.webhdfs.suffix` must be defined. It is convenient to do this through the `clusterconf` package, although it also possible to set the necessary variables manually through the `set_namenode_url`, `set_webhdfs_port`, and `set_webhdfs_suffix` functions provided in this package.

Installation
------------

The easiest way is to install the latest stable version from MCRAN. Detailed instructions for this are [available](http://mcran.mitre.org/#install-using-install.packages-from-r-preferred) or you can use the snippet below.

``` r
install.packages("webhdfs", 
                 repos=c("https://artifacts.mitre.org/artifactory/cran1-remote",
                         "http://mcran.mitre.org"))
```

Usage
-----

Here is a simple example to get up and running with this package, where `my cluster name` corresponds to a HDFS cluster whose configuration is defined using the protocol of the `clusterconf` package.

``` r
library(webhdfs)
set_cluster("my cluster name")
hdfs_ls("/data/")
```
