##' Substitute one string for another
##'
##' @export
##' @param stream A character vector, each element typically (but not necessarily) containing the text
##' from a single line in a file, which can be generated via \code{\link{readLines}}.
##' 
##' @param pattern A character string containing the regular expression that will be used to identify
##' which the elements (or lines) in \code{stream} that will be substituted using \code{sed_substitute}.
##'
##' @param replacement A character string (vectors not supported), that will be substituted
##' for the \code{pattern}.  Setting \code{replacement = ""} will remove the characters matched
##' to \code{pattern}.
##'
##' @param every A logical indicating whether every instance of \code{pattern} in each line should be
##' substituted with \code{replacement}, in which case \code{\link{gsub}} is used.
##' If \code{every = FALSE}, only the first instance of \code{pattern} in each line is substituted, in which case
##' \code{\link{sub}} is used.
##'
##' @param warn If \code{TRUE}, warning messages are produced if substitution fails due to mispecifification
##' of \code{pattern}.
##'
##' @param \dots Additional named arguments to \code{\link{grepl}} and \code{\link{gsub}} or \code{\link{sub}}.  \code{\link{grepl}}
##' is used to verify whether \code{pattern} is present in \code{stream}.  \code{\link{gsub}} or \code{\link{sub}} are used
##' to perform the substitution, depending on the value of \code{every}.
##'
##' @return The new \code{stream} with the substitions. If the substitution fails because \code{pattern} is not found,
##' \code{stream} is returned unchanged.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{sed_insert}}, \code{\link{sed_replace}}, \code{\link{sed_comment}}, \code{\link{streamEdit}}
##' 
##' @keywords misc
##'
##' @examples
##'################################################################################
##'# Let's create an example stream we can edit
##'################################################################################
##'stream <- c("Here's a line",
##'            "A line where we'll make a substitution",
##'            "A line where we'll delete 'this'",
##'            "Several nonsense nonsense repeated strings nonsense",
##'            "Another nonsense line with nonsense repeated strings",
##'            "The last line")
##'as.stream(stream)
##'
##'# Here's a deletion within the line
##'stream <- sed_substitute(stream, " 'this'", "")
##'stream
##' 
##'# Here's a substitution of text
##'stream <- sed_substitute(stream, "substitution", "correction")
##'stream
##'
##' # Show the difference between 'every = TRUE' and 'every = FALSE'
##' sed_substitute(stream, " nonsense", "", every = TRUE)
##' sed_substitute(stream, " nonsense", "", every = FALSE)

sed_substitute <- function(stream, pattern, replacement, every = TRUE, warn = FALSE, ...) {

  # Basic checks
  Smisc::stopifnotMsg(is.character(stream),
                      "'stream' must be a character vector",  
                      is.character(pattern) & (length(pattern) == 1),
                      "'pattern' must be a character string",
                      is.character(replacement) & (length(replacement) == 1),
                      "'replacement' must be a character string",
                      is.logical(every) & (length(every) == 1),
                      "'every' must be TRUE or FALSE",
                      is.logical(warn) & (length(warn) == 1),
                      "'warn' must be TRUE or FALSE")

  # If the pattern isn't present
  if (!any(grepl(pattern, stream, ...))) {

    if (warn) {
      warning("The pattern '", pattern, "' was not found in 'stream'. No changes made.")
    }

    outStream <- stream

  }
  # Otherwise the pattern is present
  else {
    if (every) {
      outStream <- gsub(pattern, replacement, stream, ...)
    }
    else {
      outStream <- sub(pattern, replacement, stream, ...)
    }
  }

  return(as.stream(outStream))
  
} # sed_substitute
