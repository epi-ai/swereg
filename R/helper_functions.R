#' @export
min_with_infinite_as_na <- function(x, na.rm=T){
  suppressWarnings(retval <- min(x, na.rm=na.rm))
  retval[is.infinite(retval)] <- NA
  return(retval)
}

#' @export
max_with_infinite_as_na <- function(x, na.rm=T){
  suppressWarnings(retval <- max(x, na.rm=na.rm))
  retval[is.infinite(retval)] <- NA
  return(retval)
}

#' @export
as_logical_min_with_infinite_as_na <- function(x, na.rm=T){
  as.logical(min_with_infinite_as_na(x, na.rm=na.rm))
}

#' @export
as_logical_max_with_infinite_as_na <- function(x, na.rm=T){
  as.logical(max_with_infinite_as_na(x, na.rm=na.rm))
}

#' @export
first_non_na <- function(x){
  dplyr::first(na.omit(x))
}

#' @export
last_non_na <- function(x){
  dplyr::last(na.omit(x))
}

