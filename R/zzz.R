##' Load the gWidgetsWWW2 userR2012 talk
##'
##' @return loads the talk in a local browser
##' @export
load_useR_talk <- function() {
  require(gWidgetsWWW2)
  
  d <- system.file("talk", package="userR2012_talk")
  tmp <- tempdir()
  files <- list.files(d)
  make_dir <- function(d) sprintf("%s%s%s", d, .Platform$file.sep, files)
  file.copy(make_dir(d), make_dir(tmp))
  old_d <- getwd()
  on.exit(setwd(old_d))
  
  setwd(tmp)
  gWidgetsWWW2:::load_app("pres.R")

}

.onLoad <- function(libname, pkgname) {
  message("Loading gWidgetsWWW2 talk, patience please it can take a bit of time to generate each page ...." )
  load_useR_talk()
}
