##' Insert one or more lines
##'
##' @details \code{sed_insert} only accomodates a single insertion point.  Multiple lines may be inserted, but only one
##' insertion point is allowed, which is why \code{length(insertion)} must be 1.  To make insertions at multiple locations,
##' \code{sed_insert} can be called repeatedly on the same string as needed.
##' 
##' @export
##' @param stream A character vector, each element typically (but not necessarily) containing the text
##' from a single line in a file, which can be generated via \code{\link{readLines}}.
##'
##' @param insertion A character vector that will be inserted into the stream after element \code{after}.
##' Each element in the vector would correspond to a separate line in the file.
##'
##' @param after An integer or character string that designates where \code{insertion} is added to \code{stream}.
##' If \code{after} is numeric, it designates the line (or element) number in \code{stream} after which the
##' \code{insertion} will be placed. The numeric value of \code{after} must be in \code{[0:length(stream)]}.
##' To make an insertion at the very beginning of \code{stream}, use \code{after = 0}.  If \code{after} is a
##' character string, the insertion is placed after the first element in \code{stream} that contains the string,
##' where matching is obtained using \code{\link{grep}}.
##'
##' @param warn If \code{TRUE}, warning messages are produced if insertion fails due to mispecifification
##' of \code{after}.
##'
##' @param \dots Additional named arguments to \code{\link{grep}}, which are applicable if \code{after} is a character string.
##' In other words, \code{\link{grep}} is used to search for the first instance of \code{after}.
##'
##' @return The new \code{stream} with the insertions added. If the insertion fails because \code{after} is
##' specified incorrectly, \code{stream} is returned unchanged.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{sed_replace}}, \code{\link{sed_substitute}}, \code{\link{sed_comment}}, \code{\link{streamEdit}}
##'
##' @keywords misc
##'
##' @examples
##'################################################################################
##'# Let's create an example stream we can edit
##'################################################################################
##'stream <- c("Here's a line",
##'            "And another line",
##'            "Line after which we'll insert a string",
##'            "A line after which we'll insert another string",
##'            "A final line")
##'as.stream(stream)
##' 
##'# Insert a string using line numbers
##'stream <- sed_insert(stream, after = 3, "Here's the first insertion")
##'stream
##'
##'# Insert a stream by searching for a string
##'stream <- sed_insert(stream,
##'                     c("Here's the second insertion",
##'                       "",
##'                       "Another line of the second insertion after the blank line"),
##'                     after = "insert another")
##'stream

sed_insert <- function(stream, after, insertion, warn = FALSE, ...) {

  # Basic checks
  Smisc::stopifnotMsg(is.character(stream),
                      "'stream' must be a character vector",
                      (length(after) == 1) & (is.character(after) | is.numeric(after)),
                      "'after' must be a whole number in [0, length(stream)] or a single character string",
                      is.character(insertion),
                      "'insertion' must be a character vector",
                      is.logical(warn) & (length(warn) == 1),
                      "'warn' must be TRUE or FALSE")

  # Get the length of the stream
  ls <- length(stream)

  # If it's character, figure out the first line after which the insertion should take place
  if (is.character(after)) {

    pattern <- after
    after <- grep(after, stream, ...)[1]

    # If match was not found, just return the string
    if (!length(after)) {

      if (warn) {
        warning("The pattern provided, `after = '", pattern, "'` was not found in 'stream'. No changes were made.")
      }

      return(as.stream(stream))

    }

  }

  # Otherwise, verify it's in the acceptable range
  else if (!(after %in% 0:ls)) {

    if (warn) {
      warning("'after' must be an integer in [0, length(stream)] or a single character string. No changes were made.")
    }
    
    return(as.stream(stream))
      
  }

  # Append before
  if (after == 0) {
    outStream <- c(insertion, stream)
  }

  # Append after
  else if (after == ls) {
    outStream <- c(stream, insertion)
  }

  # Insert in the middle
  else {

    # Cut the string in two and insert
    s1 <- stream[1:after]
    s2 <- stream[(after + 1):ls]
    outStream <- c(s1, insertion, s2)

  }

  # Return the inserted stream
  return(as.stream(outStream))

} # sed_insert


