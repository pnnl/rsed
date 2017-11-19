##' Delete or replace an entire line
##'
##' @export
##' @param stream A character vector, each element typically (but not necessarily) containing the text
##' from a single line in a file, which can be generated via \code{\link{readLines}}.
##' 
##' @param at A vector of integers or a character string that designates where \code{replacement}
##' is placed in \code{stream}.  If \code{at} is numeric, it designates the lines (or elements) in \code{stream}
##' that will be replaced with \code{replacement}.  The numeric value(s) of \code{at} must be in
##' \code{[1:length(stream)]}. If \code{at} is a
##' character string, the lines in \code{stream} that contain the string \code{at} are replaced with
##' \code{replacement}. 
##'
##' @param replacement A character string of length 1 (vectors not supported), that will be inserted
##' to replace the entire line. Or, if \code{replacement = NULL}, the entire line
##' is deleted.
##'
##' @param warn If \code{TRUE}, warning messages are produced if replacement fails due to mispecifification
##' of \code{at}.
##'
##' @param \dots Additional named arguments to \code{\link{grep}}, which are applicable if \code{at} is a character string.
##' In other words, \code{\link{grep}} is used to search for the instances of \code{at}.
##'
##' @return The new \code{stream} with the replacements. If the replacement fails because \code{after} is
##' specified incorrectly, \code{stream} is returned unchanged.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{sed_insert}}, \code{\link{sed_substitute}}, \code{\link{sed_comment}}, \code{\link{streamEdit}}
##'
##' @keywords misc
##'
##' @examples
##'################################################################################
##'# Let's create an example stream we can edit
##'################################################################################
##'stream <- c("Here's a line",
##'            "Here's a line we'll delete",
##'            "Filler line",
##'            "A line we'll delete",
##'            "A line we'll entirely replace",
##'            "The last line")
##'as.stream(stream)
##'
##'# Here's a deletion of lines 1 and 2 using line numbers
##'stream <- sed_replace(stream, 1:2, NULL)
##'stream
##'
##'# Here's a line deletion using a search string
##'stream <- sed_replace(stream, "A line we'll delete", NULL)
##'stream
##'
##'# A line replacement using line numbers
##'stream <- sed_replace(stream, 2, "A new filler line")
##'stream
##' 
##'# Here's a line replacement with a search string
##'stream <- sed_replace(stream, "entirely", "A replacement for the line")
##'stream
##'
##'# And we can replace multiple lines too
##'stream <- sed_replace(stream, "line", "All the same")
##'stream

sed_replace <- function(stream, at, replacement, warn = FALSE, ...) {

  # Basic checks
  Smisc::stopifnotMsg(is.character(stream),
                      "'stream' must be a character vector",
                      if (is.character(at)) {
                        length(at) == 1
                      } else is.numeric(at),
                      "'at' must be a character string or a vector of whole numbers",
                      if (!is.null(replacement)) {
                        is.character(replacement)
                      } else TRUE,
                      "'replacement' must be NULL or a character string",
                      is.logical(warn) & (length(warn) == 1),
                      "'warn' must be TRUE or FALSE")

  # If it's character, figure out lines where replacement should take place
  if (is.character(at)) {

    pattern <- at
    at <- grep(at, stream, ...)

    # If match was not found, just return the string
    if (!length(at)) {

      if (warn) {
        warning("The pattern '", pattern, "' was not found in 'stream'.  No changes were made.")
      }

      return(as.stream(stream))

    }

  }

  # Otherwise, verify it's in the acceptable range
  else if (!all(at %in% 1:length(stream))) {

    if (warn) {
      warning("'at' must be an integer in [1, length(stream)] or a single character string. No changes were made.")
    }

    return(as.stream(stream))
    
  }

  # Initialize the outstream
  outStream <- stream
  
  # If a string was provided
  if (!is.null(replacement)) {

    # If the replacement is length 1, then insert directly
    if (length(replacement) == 1) {
      outStream[at] <- replacement
    }

    # If the replacement is longer than one element
    else {
      stop("'replacement' with length greater than 1 is not (yet) supported")
    }
  }
  # If NULL was provided
  else {
    outStream <- outStream[-at]
  }

  # Return the edited stream
  return(as.stream(outStream))

} # sed_replace
