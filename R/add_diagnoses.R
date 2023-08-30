#' COVID-19 data for total age/sex in Norway (2020 border).
#'
#' @export
add_diagnoses <- function(
  skeleton,
  diag_and_surgeries,
  id_name,
  diags = list(
    "icd10_F64_0" = c("^F640"),
    "icd10_F64_89" = c("^F6489"),
    "icd10_F64_089" = c("^F640", "^F648", "^F649")
  )
  ){

  variables_containing_icd_codes <- c(
    "HDIA",
    stringr::str_subset(names(diag_and_surgeries), "^DIA"),
    stringr::str_subset(names(diag_and_surgeries), "^EKOD")
  )

  for(i in seq_along(diags)){
    nam <- names(diags)[i]
    diag_and_surgeries[, (nam) := FALSE]

    for(ii in variables_containing_icd_codes) for(iii in diags[[i]]){
      diag_and_surgeries[stringr::str_detect(get(ii), iii), (nam):=TRUE]
    }
  }

  diag_and_surgeries[, isoyearweek := cstime::date_to_isoyearweek_c(INDATUM)]
  min_isoyearweek <- min(skeleton[is_isoyear==FALSE]$isoyearweek)
  diag_and_surgeries[isoyearweek<min_isoyearweek, isoyearweek := paste0(cstime::date_to_isoyear_c(INDATUM),"-**")]

  nam <- names(diags)
  txt <- paste0("reduced <- diag_and_surgeries[, .(", paste0(names(diags),"=as.logical(max(",names(diags),"))", collapse=", "),"), keyby=.(",id_name,", isoyearweek)]")
  eval(parse(text = txt))

  nam_left <- paste0(nam,collapse='","')
  nam_left <- paste0('"',nam_left, '"')
  nam_right <- paste0(nam,collapse=',')
  txt <- paste0('skeleton[reduced,on = c("id==',id_name,'","isoyearweek"),c(',nam_left,'):=.(',nam_right,')]')
  eval(parse(text = txt))

  for(i in nam){
    skeleton[is.na(get(i)), (i) := FALSE]
  }
}
