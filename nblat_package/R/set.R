#' @export
as_set = function(set) {
  class(set) <- 'set'
  set
}

empty_set = function() {
  as_set(numeric(0))
}

#' @method print set
#' @export
print.set <- function(set, width=NA){
  set_string_repr <- paste(set,collapse=",")

  if (!is.na(width)) {
    n <- nchar(set_string_repr)+2
    if (n < width) {
      cat( paste(replicate(width-n," "),collapse="") )
    }
  }
  cat('{')
  cat(set_string_repr)
  cat('}')
}
