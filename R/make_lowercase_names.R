#' @export
make_lowercase_names <- function(x) {
  UseMethod("make_lowercase_names", x)
}

#' @export
make_lowercase_names.default <- function(x){
  names(x) <- tolower(names(x))
  x
}

#' @export
make_lowercase_names.data.table <- function(x){
  setnames(x, tolower(names(x)))
  x
}
