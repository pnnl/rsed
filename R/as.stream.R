##' Converts a character vector to an object of class "stream"
##'
##' @export
##' @param obj The character vector to be converted to a "stream" object
##'
##' @return The same character vector, only it now inherits from class "stream"
##'
##' @author Landon Sego
##'
##' @examples
##' aStream <- c("string1", "string2", "string3")
##' aStream
##'
##' # Note how it prints after we convert it to a stream object:
##' as.stream(aStream)

as.stream <- function(obj) {
    
  Smisc::stopifnotMsg(is.character(obj),
                     "'obj' must be a character vector")

  if (!inherits(obj, "stream")) {
    class(obj) <- c("stream", class(obj))
  }
  
  return(obj)
  
} # as.stream()


##' @method print stream
##'
##' @describeIn as.stream Prints an object of class "stream" by removing quotes from the strings and printing each element
##' of the character vector on its own line.
##'
##' @param x The "stream" object to be printed.
##'
##' @param \dots Ignored
##'
##' @export

print.stream <- function(x, ...) {

  # Indexes
  nums <- paste("[", 1:length(x), "]", sep = "")

  # Iteratively add leading spaces until all indexes have same number of characters
  maxn <- max(nchar(nums))
  i <- 0
  
  while ((any(nchar(nums) < maxn)) & (i < 1000)) {
    nums <- ifelse(nchar(nums) == maxn, nums, paste(" ", nums, sep = ""))
    i <- i + 1
  }

  # Print the results, one line at a time, padding the indexes with spaces on the left
  cat(paste(paste(nums, x), collapse = "\n"), "\n", sep = "")
    
} # print.stream()

