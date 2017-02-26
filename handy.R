# handy stuff

# R.version  # Info on running R version
# getRversion()  # Version info only
# rstudioapi::getVersion()  # Info on running RStudio version
# sessionInfo()  # Session info
# options()  # Get a named list of several global options
# .packages(all.available=T)  # Have a look at all available packages
# .libPaths()  # Get the paths 2 R modules and packages
# ls(getNamespace('sys'), all.names=T)  # Have a look at all namespaces in a package
# ls()  # Get all namespaces in the current environment
# ls(envir=globalenv())  # Get all namespaces in a specified environment
# rm(list=ls())  # Clear all namespaces in the current environment
# args(readline)  # Get the argument list of a function
# formals(readline)  # same thing just ugly
# body(source)  # Have a look at a function's body
# environment(source)  # Get the parent environment of a function
# readline('What\'s ur goal 4 today? ')  # Get user input from the console
# rstudioapi::askForPassword('What\'s the secret?')  # Ask with a popup in RStudio
# rstudioapi::sendToConsole('419 * 2', execute=F)  # Send code to the RStudio console

# 10 %% 7  # Modulo
# 10 %/% 7  # Integer division
# x %*% y  # Matrix multiplication

# Easy string concatenation
'%+%' <- function(a, b) UseMethod('%+%')  # Generic concat operator
'%+%.character' <- function(a, b) paste0(a, b)  # String concat operator
'sakawa' %+% ' spirit' %+% ' gives strength'  # 419

'%?%' <- function(a, b) {
  # Ternary operator 4 R ... !!!
  # @param {bool} a Expression that evaluates 2 a logical
  # @param {character} b Vector of length 2; if a is T, 1st item is evaluated,
  #                      otherwise the 2nd
  # @examples
  #   (0 > 1) %?% c('x <- TRUE', 'x <- FALSE')
  #   (100**3 == 1e6) %?% c('{x <- T; y <- "cool"; z <- "****"}', 'x <- F')
  #   {p <- 15; p < 17.5} %?% c('message("proceeding")', 'warning("2 high")')
  stopifnot(is.logical(a), is.character(b), length(b) == 2)
  if (a) {
    eval(parse(text=b[1]), parent.frame(1))
  } else if (!a) {
    eval(parse(text=b[2]), parent.frame(1))
  }
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

# Yolo lambdas
g(foo, bar) %=% c(2, 3)  # unpacking magic
foo %>% {foo * 7}  # single argument, single statement
c(foo, bar) %>% {foo + bar}  # multi arguments, single statement
c(foo, bar) %>% {x <- foo + bar; x * x}  # multi arguments, multi statements

inBando <- function(bando=NULL) {
  # Checks whether u r in the specified working directory.
  # @param {character} bando Name of directory to be checked 4
  # @return {bool} Do input and cwd match?
  if (missing(bando) || bando == '') stop('No input!')
  return(grepl(paste0(bando, '$'), getwd(), ignore.case=T))
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