# webhdfs 0.1.0

* Bug fix on error message when required setting is not available
* Add default values for WebHDFS port and suffix for URL
* Initial version released to GitHub

# webhdfs 0.0.9

* Bugfix on setting webhdfs configuration for port and url suffix
* Increased test code coverage 

# webhdfs 0.0.8

* Add hdfs_open function

# webhdfs 0.0.7

* Fix broken link in readme to full code coverage report

# webhdfs 0.0.6

* Adjust readme so pkgdown displays code coverage

# webhdfs 0.0.5

* Start tracking code coverage

# webhdfs 0.0.4

* Bug fix to build default user name when making, renaming, and deleting directories 
* Bug fix where testing for an active namenode may fail for secure clusters requiring user name for GET commands 
* Add additional function examples
* Update docs to resolve linking issues

# webhdfs 0.0.3

* Bug fix to avoid needing to set return type after changing clusters

# webhdfs 0.0.2

* Use httr instead of RCurl to support additional secure server configurations
* Use user name in WebHDFS GET commands to support additional secure server configurations.  (It was using user name already for PUT commands.)
* Bug fix on hdfs_ls recursion with return type data.frame

# webhdfs 0.0.1

* Initial version released to MCRAN

