# handy stuff

R.version  # Info on running R version
rstudioapi::getVersion()  # Info on running RStudio version
sessionInfo()  # Session info
.packages(all.available=T)  # Have a look at all available packages
.libPaths()  # Get the paths 2 R modules and packages
ls(getNamespace('sys'), all.names=T)  # Have a look at all namespaces in a package
ls()  # Get all namespaces in a specified environment
rm(list=ls())  # Clear all namespaces in the current environment
readline('What\'s ur goal 4 today? ')  # Get user input from the console
rstudioapi::askForPassword('What\'s the secret?')  # Ask with a popup in RStudio
rstudioapi::sendToConsole('419 * 2', execute=F)  # Send code to the RStudio console
args(readline)  # Get the argument list of a function

inBando <- function(dir=NULL) {
  # Checks whether u r in the specified working directory
  # @param {character} dir Name of directory to be checked 4
  # @return {bool} Do input and cwd match?
  if (missing(dir)) stop('no input!')
  return(grepl(paste0(dir, '$'), getwd(), ignore.case=T))
}

gothub <- function(remote, flname=sub('^.*/([^/]*)$', '\\1', remote), open=T) {
  # Download and open a .R or .Rmd file from a Github repo with 1 R function call!
  # Let below be all valid inputs [@param remote]
  #   'https://github.com/chiefBiiko/rockets/blob/master/server.R'
  #   'https://raw.githubusercontent.com/chiefBiiko/rockets/master/server.R'
  #   'https://github.com/chiefBiiko/rockets/raw/master/server.R'
  # @param {character} remote URI in 1 of the formats above
  # @param {character} flname Destination file name, is destfile 4 base::download.file()
  # @param {bool} open Should the file be opened in an editor window?
  # @return {character} Name of downloaded file
  stopifnot(!missing(remote), is.character(remote), grepl('^https', remote))
  if (grepl('raw.+\\.r(md)?$', remote, ignore.case=T)) {
    uri <- remote
  } else if (grepl('blob.+\\.r(md)?$', remote, ignore.case=T)) {
    uri <- sub('blob', 'raw', remote)
  } else { stop('Can\'t handle remote URI.') }
  dlcd <- download.file(uri, flname)
  if (dlcd != 0) stop('Download error.')
  if (open == T) file.edit(flname)
  return(flname)
}