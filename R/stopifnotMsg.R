## Check multiple conditions and return coresponding error messages.  
##
## A more flexible version of \code{\link{stopifnot}} that allows you to control the error message returned by
## each condition that doesn't test true. This came from the Smisc package.
##
## @param \dots Pairs of logical conditions and error messages. See Examples.
##
## @param level Whole number indicating how far back in the call stack the error should be attributed to.
## \code{level = 1} goes back to the calling function, \code{level = 2} goes back 2 levels, etc.
## See examples.
##
## @return A call to \code{\link{stop}} with the error messages from each condition that was \code{FALSE}.  If all
## conditions are \code{TRUE}, returns \code{NULL}.
##
## @author Landon Sego

stopifnotMsg <- function(..., level = 1) {

  levelGood <- if (is.numeric(level)) {
                 (level > 0) & (level %% 1 == 0)
               } else FALSE

  if (!levelGood) {
    stop("'level' must be a whole number: 1, 2, 3, ...")
  }

  # Gather inputs to a list
  inputs <- list(...)
  n <- length(inputs)

  # The length of the list should be even
  if ((n %% 2) != 0) {
    stop("The inputs must consist of pairs of logical conditions and a corresponding error text string, ",
         "and therefore the total number of inputs must be an even number.")
  }

  # Split the conditions and the text strings into two lists
  cond <- inputs[seq(1, n, by = 2)]
  msgs <- inputs[seq(2, n, by = 2)]

  # Verify that cond are all logical
  if (!all(unlist(lapply(cond, is.logical)))) {
    stop("All the odd numbered inputs must resolve to logicals (TRUE/FALSE)")
  }

  # Verify that msgs are all character
  if (!all(unlist(lapply(msgs, is.character)))) {
    stop("All the even numbered inputs must resolve to character strings")      
  }

  # Identify which elements of cond are false
  failedIndexes <- which(!unlist(cond))

  if (length(failedIndexes)) {

    # Get the call to the function from which stopifnotMsg() was called
    fn <- deparse(sys.calls()[[max(1, sys.nframe() - level)]])

    # Gather a messages for the stop
    msg <- paste(c(fn, unlist(msgs)[failedIndexes]), collapse = "\n")
  
    # Issue the stop()
    stop(msg, call. = FALSE)
  
  }
  else {
    return(NULL)
  }
  
} # stopifnotMsg()
