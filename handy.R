# handy stuff

# R.version  # Info on running R version
# getRversion()  # Version info only
# rstudioapi::getVersion()  # Info on running RStudio version
# sessionInfo()  # Session info
# options()  # Get a named list of several global options

# getOption('stringsAsFactors')  # Get the value of a specific global option
# options(stringsAsFactors=F)  # Set the value of a global option

# .libPaths()  # Get the paths 2 R modules and packages
# .packages(all.available=T)  # Have a look at all available packages
# ls(getNamespace('sys'), all.names=T)  # Have a look at all namespaces in a package

# ls()  # Get all namespaces in the current environment
# ls(envir=globalenv())  # Get all namespaces in a specified environment
# rm(list=ls())  # Clear all namespaces in the current environment

# args(readline)  # Get the argument list of a function
# formals(readline)  # same thing just ugly
# body(source)  # Have a look at a function's body
# environment(source)  # Get the parent environment of a function

# readLines(con)  # Read lines of text from a connection
# readline('Continue? [y/n]')  # Get user input from the console
# rstudioapi::askForPassword('What\'s the secret?')  # Ask with a popup in RStudio
# rstudioapi::sendToConsole('419 * 2', execute=F)  # Send code to the RStudio console

# dirname(list.files()[1])  # Get pre the last path separator
# basename(getwd())  # Get past the last path separator

# cat('hello moon\n', file='moon.txt')  # Write to a file
# cat('hello dolphins\n', file='moon.txt', append=T)  # Append to a file
# file.edit('moon.txt')  # Open a file in an editor window
# dir.create('moon')  # Create a directory
# file.copy('moon.txt','moon/moon.txt')  # Copy a file
# file.exists('moon/moon.txt')  # Check if a file exists
# file.remove(c('moon.txt', 'moon/moon.txt'))  # Delete files
# unlink('moon', recursive=T)  # Delete a directory
# dir.exists('moon')  # Check if a directory exists

inBando <- function(bando=NULL) {
  # Checks whether u r in the specified working directory.
  # @param {character} bando Name of directory to be checked 4
  # @return {bool} Do input and cwd match?
  stopifnot(!missing(bando), is.character(bando), bando != '')
  return(grepl(paste0(bando, '$'), getwd(), ignore.case=T))
}

# Destructuring, unpacking magic: g(a, b, c) %=% c(77, 99, 36)
'%=%' <- function(l, r) UseMethod('%=%')  # Generic form
g <- function(...) structure(as.list(substitute(list(...))), class='lbunch')  # Caster
'%=%.lbunch' <- function(l, r) {  # Destructuring operator
  # Destructures items of the right operand with the namespaces provided on the left.
  stopifnot(length(l) > 1)
  if (length(r) > length(l) - 1) {
    warning('Right has more args than left. Only first ', length(l) - 1, ' used.')
  } else if (length(l) - 1 > length(r)) {
    warning('Left has more args than right. Right will be repeated.')
    r <- rep(r, ceiling((length(l) - 1) / length(r)))[1:(length(l) - 1)]
  }
  for (item in 2:length(l)) {  # g() value is first, counts as 1 length, ignoring it
    assign(as.character(l[[item]]), as.list(r)[[item - 1]], pos=1L)
  }
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

# Yolo lambdas
# g(foo, bar) %=% c(2, 3)  # unpacking magic
# library(magrittr)
# foo %>% {foo * 7}  # single argument, single statement
# c(foo, bar) %>% {foo + bar}  # multi arguments, single statement
# c(foo, bar) %>% {x <- foo + bar; x * x}  # multi arguments, multi statements

# 10 %% 7  # Modulo
# 10 %/% 7  # Integer division
# x %*% y  # Matrix multiplication