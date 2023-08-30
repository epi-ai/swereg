#' COVID-19 data for total age/sex in Norway (2020 border).
#'
#' @export
add_annual <- function(
  skeleton,
  data,
  id_name,
  isoyear
  ){

  data[, isoyear := isoyear]
  nam_left <- names(data)[!names(data) %in% c(id_name, "isoyear")]
  nam_right <- nam_left

  for(i in seq_along(nam_left)){
    if(nam_left[i] %in% names(skeleton)) nam_right[i] <- paste0("i.",nam_left[i])
  }

  nam_left <- paste0(nam_left,collapse='","')
  nam_left <- paste0('"',nam_left, '"')
  nam_right <- paste0(nam_right,collapse=',')
  txt <- paste0('skeleton[data,on = c("id==',id_name,'","isoyear"),c(',nam_left,'):=.(',nam_right,')]')
  eval(parse(text = txt))
}
