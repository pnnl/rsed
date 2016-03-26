##' High-level wrapper for stream editing functions
##'
##' High-level wrapper for stream editing functions (\code{\link{sed_insert}}, \code{\link{sed_replace}},
##' \code{\link{sed_substitute}}, and \code{\link{sed_comment}}) to execute any number of sequential
##' insertion, replacement, substitution, or commenting steps.
##'
##' @details
##' One (and only one) of \code{inFile} or \code{stream} must be specified.
##' 
##' If \code{inFile} and \code{outFile} are the same, a backup copy of \code{inFile} is made by appending
##' "~" to the end of the filename, e.g., if the original file were \file{aFile.txt}, the backup would be
##' \file{aFile.txt~}.
##'
##' The value of \code{warn} in \code{streamEdit} is passed to the worker functions
##' (\code{\link{sed_insert}}, \code{\link{sed_replace}}, \code{\link{sed_substitute}}, and \code{\link{sed_comment}})
##' unless the \code{warn} argument is specified for a command in \code{commandList}, in which case, for that
##' particular command, the locally supplied value of \code{warn} takes precedence.
##'
##' @export
##' @param stream A character vector, each element typically (but not necessarily) containing the text
##' from a single line in a file, which can be generated via \code{\link{readLines}}.
##'
##' @param commandList A list that designates the insertion, replacement, substitution, or commenting commands
##' that will be performed on \code{stream} (or the stream obtained from \code{inFile}).
##' The list must have names corresponding to unique abbreviations of
##' "insert", "replace", "substitute", and/or "comment".  Each element in \code{commandList} must also be
##' a list with names and values that correspond to the arguments of \code{\link{sed_insert}}, \code{\link{sed_replace}},
##' \code{\link{sed_substitute}}, and/or \code{\link{sed_comment}} respectively.  See Examples.
##'
##' @param inFile A character string designating a file that is to be read (using \code{\link{readLines}}
##' and will become a \code{stream},
##' where each line of the file is mapped to a single element in the character vector \code{stream}.
##' 
##' @param outFile A character string designating a file to which the resulting, edited stream will be
##' written using \code{\link{writeLines}}. If \code{outFile = NULL}, no file is written.
##'
##' @param warn A logical that, when \code{TRUE}, issues a worning if the insertion, replacement, subsitution, or
##' commenting cannot be performed. See Details.
##'
##' @return Invisibly returns the edited stream, and writes the stream to a file if \code{outFile} is supplied.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{sed_insert}}, \code{\link{sed_replace}}, \code{\link{sed_substitute}}, \code{\link{sed_comment}}
##'
##' @keywords misc
##'
##' @examples
##'################################################################################
##'# Let's create an example stream we can edit
##'################################################################################
##'cat("Here's a line\n",
##'    "Line after which we'll insert a string\n",
##'    "A line we'll delete\n",
##'    "A line we'll replace with something else\n",
##'    "A line where we'll make a substitution\n",
##'    "A line we'll comment\n",
##'    "The last line\n",
##'    sep = "", file = "tmpTest_streamEdit.txt")
##'
##'# Read the file into a 'stream'
##'s <- readLines("tmpTest_streamEdit.txt")
##'
##'################################################################################
##'# Excecute a series of commands 'manually', using the individual worker functions
##'################################################################################
##' 
##'s <- sed_insert(s, after = 3, insertion = "Here's an insertion")
##'s <- sed_replace(s, at = "delete", replacement = NULL)
##'s <- sed_replace(s, at = "we'll replace", replacement = "The replacement", fixed = TRUE)
##'s <- sed_substitute(s, pattern = "make a substitution", replacement = "have a party")
##'s <- sed_comment(s, at = "comment", type = "html")
##'s
##'
##'################################################################################
##'# Now execute these same commands using a single call to streamEdit(), along
##'# with reading the input file and writing the output file
##'################################################################################
##'
##'# Build the list of commands
##'comList <- list(
##' 
##'  # i for 'insert', arguments for sed_insert()
##'  i = list(after = 3, insertion = "Here's an insertion"),
##' 
##'  # r for 'replace', arguments for sed_replace()
##'  r = list(at = "delete", replacement = NULL),
##' 
##'  # r for 'replace', arguments for sed_replace()
##'  r = list(at = "we'll replace", replacement = "The replacement", fixed = TRUE),
##'
##'  # s for 'substitute', arguments for sed_substitute()
##'  s = list(pattern = "make a substitution", replacement = "have a party"),
##' 
##'  # c for 'comment', arguments for sed_comment()
##'  c = list(at = "comment", type = "html")
##' 
##' )
##' 
##'s1 <- streamEdit(comList, inFile = "tmpTest_streamEdit.txt",
##'                 outFile = "tmpTest_streamEdit1.txt")
##'s1
##' 
##'# Compare the results
##'identical(s, s1)
##'
##'# Remove the files
##'unlink(c("tmpTest_streamEdit.txt", "tmpTest_streamEdit1.txt"))

streamEdit <- function(commandList, stream = NULL, inFile = NULL, outFile = NULL, warn = FALSE) {

  # One or the other of 'stream' and 'inFile' must be specified
  if (sum(is.null(stream), is.null(inFile)) != 1) {
    stop("One (and only one) of 'stream' or 'inFile' must be specified")
  }

  # Verify we have a list in the commandList, and that it has acceptable values
  if (!is.list(commandList)) {
    stop("'commandList' must be a list. See help(streamEdit)")
  }

  # Verify the names are valid
  if (!all(substr(tolower(names(commandList)), 1, 1) %in% c("i", "r", "s", "c"))) {
    stop("Names of 'commandList' must be strings that uniquely identify 'insert', 'replace',\n",
         "'substitute', or 'comment'")
  }

  # Verify that sub list is also a list
  if (!all(unlist(lapply(commandList, is.list)))) {
    stop("Each element in 'commandList' must be a list with names corresponding to the arguments of the\n",
         "intended command function (sed_insert(), sed_replace(), sed_substitute(), or sed_comment())")
  }

  # Read the file
  if (!is.null(inFile)) {
    stream <- readLines(inFile)
  }

  # Assign the function names
  commands <- unlist(lapply(substr(tolower(names(commandList)), 1, 1),
                            function(x) switch(x,
                                               "i" = "sed_insert",
                                               "r" = "sed_replace",
                                               "s" = "sed_substitute",
                                               "c" = "sed_comment")))

  # Initialize the outStream with the stream
  outStream <- stream

  # Now execute the command list the stream until we're ready to return the output
  for (i in 1:length(commandList)) {

    cmdList <- commandList[[i]]
    cmd <- commands[i]

    # If a value for warn is provided in the command list, leave it alone.  Otherwise,
    # set it to the overall value of warn when streamEdit() was called
    if (!("warn" %in% names(cmdList))) {
      cmdList <- c(cmdList, list(warn = warn))
    }

    # Execute the commands on the stream
    outStream <- do.call(cmd, c(list(stream = outStream), cmdList))

  }

  # Write the output
  if (!is.null(outFile)) {

    # Make a backup copy of the inFile if the outFile is the same
    if (!is.null(inFile)) {
      if (inFile == outFile) {
        file.copy(inFile, paste(inFile, "~", sep = ""))
      }
    }

    writeLines(outStream, con = outFile)

  }

  # Invisibly return the stream
  invisible(as.stream(outStream))

} # streamEdit
