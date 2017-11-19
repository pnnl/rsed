##' Add or remove comment characters to a line
##'
##' @details Some languages provide a way to comment multiple lines of code with a single pair of beginning and ending commenting
##' symbols.  However, \code{sed_comment} only adds/removes comment symbols to/from a single lines.
##' When comments are added by \code{sed_comment}, the comment symbols are placed at the beginning (and, if applicable, at the end) of
##' the selected lines. When comments are removed, the first instances of the beginning (and, if applicable, ending) comment symbols
##' are removed from the selected lines.
##' 
##' @export
##' @param stream A character vector, each element typically (but not necessarily) containing the text
##' from a single line in a file, which can be generated via \code{\link{readLines}}.
##'
##' @param at A vector of integers or a character string that designates the line(s) that will be commented.
##' If \code{at} is numeric, it designates the line(s) (or elements) in \code{stream} that will be commented.
##' The numeric value(s) of \code{at} must be in \code{[1:length(stream)]}. If \code{at} is a
##' character string, the line(s) in \code{stream} that contain the string \code{at} are commented.
##'
##' @param add A logical indicating whether comments are added to a single
##' line (\code{TRUE}), or removed (\code{FALSE}).
##'
##' @param type A character string uniquely indicating the programming language: R, C, Java, html, tex, and SAS.
##' Customized commenting can be achieved by providing a character vector of length 2, where \code{type[1]} designates
##' the beginning comment character and \code{type[2]} designates the ending comment character.
##'
##' @param warn If \code{TRUE}, warning messages are produced if commenting (or uncommenting) fails due to mispecifification
##' of \code{at}.
##'
##' @param \dots Additional named arguments to \code{\link{grep}}, which are applicable if \code{at} is a character string.
##' In other words, \code{\link{grep}} is used to search for the instances of \code{at}.
##'
##' @return The new \code{stream} with the commented (or uncommented) lines. If the commenting fails because \code{at} is
##' specified incorrectly, \code{stream} is returned unchanged.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{sed_insert}}, \code{\link{sed_replace}}, \code{\link{sed_substitute}}, \code{\link{streamEdit}}
##'
##' @keywords misc
##'
##' @examples
##'######################################################################
##'# Let's create a stream to demonstrate the commenting symbol in each
##'# language
##'######################################################################
##'demoStream <- c("An R comment",
##'                "A C comment",
##'                "An html comment",
##'                "A tex comment",
##'                "A SAS comment",
##'                "A custom comment")
##'
##'a <- sed_comment(demoStream, "R", type = "R")
##'a <- sed_comment(a, "C", type = "C")
##'a <- sed_comment(a, "html", type = "h")
##'a <- sed_comment(a, "tex", type = "t")
##'a <- sed_comment(a, "SAS", type = "S")
##'a <- sed_comment(a, "custom", type = c("&", ";"))
##'
##'# Compare before and after
##'as.stream(demoStream)
##'a
##'
##'######################################################################
##'# Various examples
##'######################################################################
##'aStream <- c("Here's a line to comment",
##'             "# A line to uncomment",
##'             "  <!-- Another commented line --> ",
##'             "And some comments * embedded in the line ;")
##'as.stream(aStream)
##'
##'# Comment the first line in C style
##'stream <- sed_comment(aStream, "to comment", type = "C")
##'
##'# Comment the first line with a custom style
##'a <- sed_comment(aStream, "to comment", type = c("&&", "##"))
##'a
##'
##'# Remove the custom comments
##'a <- sed_comment(a, 1, add = FALSE, type = c("&&", "##"))
##'a
##'
##'# Remove the R comment from the 2nd line
##'a <- sed_comment(a, 2, add = FALSE, type = "R")
##'a
##'
##'# Remove the html comments
##'a <- sed_comment(a, "Another", add = FALSE, type = "html")
##'a
##'
##'# Remove the SAS comments
##'sed_comment(a, "embedded", add = FALSE, type = "SAS")
##'
##'# Comment every line in Java style
##'b <- sed_comment(aStream, "comment", type = "Java")
##'b
##'
##'# Remove the Java comments from the second and fourth lines
##'sed_comment(b, c(2, 4), add = FALSE, type = "Java")

sed_comment <- function(stream, at, add = TRUE,
                        type = c("R", "C", "Java", "html", "tex", "SAS"),
                        warn = FALSE, ...) {

  # Basic checks
  Smisc::stopifnotMsg(is.character(stream),
                      "'stream' must be a character vector",
                      if (is.character(at)) {
                        length(at) == 1
                      } else is.numeric(at),
                      "'at' must be a character string or a vector of whole numbers",
                      is.logical(add) & (length(add) == 1),
                      "'add' must be TRUE or FALSE",
                      is.character(type),
                      "'type' must be one of 'R', 'C', 'Java', 'html', 'tex', or 'SAS' or a customized 2-vector of comment characters",
                      is.logical(warn) & (length(warn) == 1),
                      "'warn' must be TRUE or FALSE")

  # If it's character, figure out lines where commenting should take place
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
      warning("'at' must be an integer in [1, length(stream)] or a single character string. No change were made.")
    }

    return(as.stream(stream))
    
  }

  # Customized comment characters
  if (length(type) == 2) {

    beginComment <- type[1]
    endComment <- type[2]

  }
  else {

    # Match the type argument to the acceptable values
    type <- match.arg(type)

    # Figure out which comment characters will be applied to beginning and end of lines
    beginComment <- switch(type,
                           "R" = "#",
                           "C" = "//",
                           "Java" = "//",
                           "html" = "<!--",
                           "tex" = "%",
                           "SAS" = "*")

    endComment <- switch(type,
                         "R" = "",
                         "C" = "",
                         "Java" = "",
                         "html" = "-->",
                         "tex" = "",
                         "SAS" = ";")
  }

  # Initialize the outstream
  outStream <- stream

  # Comment the appropriate lines
  if (add) {
    outStream[at] <- paste(beginComment, stream[at], endComment, sep = "")
  }
  # Remove the comments from lines if comments exist
  else {

    # Remove the first instance of the beginning comment character
    str <- sub(beginComment, "", stream[at], fixed = TRUE)

    # Remove the first instance of the ending comment symbol
    if (nchar(endComment)) {
      str <- sub(endComment, "", str, fixed = TRUE)
    }

    # Insert into the outStream
    outStream[at] <- str

  } # Removing comments

  # Return the edited stream
  return(as.stream(outStream))

} # sed_comment
