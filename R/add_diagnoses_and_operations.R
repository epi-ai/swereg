#' COVID-19 data for total age/sex in Norway (2020 border).
#'
#' @export
add_cods <- function(
    skeleton,
    dataset,
    id_name,
    cods = list(
      "icd10_F64_0" = c("^F640"),
      "icd10_F64_89" = c("^F6489"),
      "icd10_F64_089" = c("^F640", "^F648", "^F649")
    )
){
  add_diagnoses_or_operations_or_cods(
    skeleton = skeleton,
    dataset = dataset,
    id_name = id_name,
    diags_or_ops_or_cods = cods,
    type = "cods"
  )
}

#' COVID-19 data for total age/sex in Norway (2020 border).
#'
#' @export
add_diagnoses <- function(
    skeleton,
    dataset,
    id_name,
    diags = list(
      "icd10_F64_0" = c("^F640"),
      "icd10_F64_89" = c("^F6489"),
      "icd10_F64_089" = c("^F640", "^F648", "^F649")
    )
){
  add_diagnoses_or_operations_or_cods(
    skeleton = skeleton,
    dataset = dataset,
    id_name = id_name,
    diags_or_ops_or_cods = diags,
    type = "diags"
  )
}

#' COVID-19 data for total age/sex in Norway (2020 border).
#'
#' @export
add_operations <- function(
    skeleton,
    dataset,
    id_name,
    ops = list(
      "op_afab_mastectomy"= c(
        "HAC10",
        "HAC20",
        "HAC99",
        "HAC15"
      ),
      "op_afab_breast_reconst_and_other_breast_ops" = c(
        "HAD20",
        "HAD30",
        "HAD35",
        "HAD99",
        "HAE99"
      ),
      "op_afab_penis_test_prosth" = c(
        "KFH50",
        "KGV30",
        "KGW96",
        "KGH96"
      ),
      "op_afab_internal_genital" = c(
        "LCD00",
        "LCD01",
        "LCD04",
        "LCD10",
        "LCD11",
        "LCD96",
        "LCD97"
      ),
      "op_afab_colpectomy" = c(
        "LED00"
      ),
      "op_amab_breast_reconst_and_other_breast_ops" = c(
        "HAD00",
        "HAD10",
        "HAD99",
        "HAE00",
        "HAE20",
        "HAE99"
      ),
      "op_amab_reconst_vag" = c(
        "LEE10",
        "LEE40",
        "LEE96",
        "LFE10",
        "LFE96"
      ),
      "op_amab_penis_amp" = c(
        "KGC10"
      ),
      "op_amab_larynx" = c(
        "DQD40"
      )
    )
){
  add_diagnoses_or_operations_or_cods(
    skeleton = skeleton,
    dataset = dataset,
    id_name = id_name,
    diags_or_ops_or_cods = ops,
    type = "ops"
  )
}


add_diagnoses_or_operations_or_cods <- function(
    skeleton,
    dataset,
    id_name,
    diags_or_ops_or_cods,
    type
){
  stopifnot(type %in% c("diags", "ops", "cods"))

  if(type == "diags"){
    variables_containing_codes <- c(
      stringr::str_subset(names(dataset), "^HDIA"),
      stringr::str_subset(names(dataset), "^hdia"),
      stringr::str_subset(names(dataset), "^DIA"),
      stringr::str_subset(names(dataset), "^EKOD"),
      stringr::str_subset(names(dataset), "^ICDO10")
    )
    dataset[, isoyearweek := cstime::date_to_isoyearweek_c(indatum)]
    min_isoyearweek <- min(skeleton[is_isoyear==FALSE]$isoyearweek)
    dataset[isoyearweek<min_isoyearweek, isoyearweek := paste0(cstime::date_to_isoyear_c(indatum),"-**")]

  } else if(type == "ops") {
    variables_containing_codes <- c(
      stringr::str_subset(names(dataset), "^OP"),
      stringr::str_subset(names(dataset), "^op")
    )
    dataset[, isoyearweek := cstime::date_to_isoyearweek_c(indatum)]
    min_isoyearweek <- min(skeleton[is_isoyear==FALSE]$isoyearweek)
    dataset[isoyearweek<min_isoyearweek, isoyearweek := paste0(cstime::date_to_isoyear_c(indatum),"-**")]
  } else if(type == "cods") {
    variables_containing_codes <- c(
      stringr::str_subset(names(dataset), "^MORSAK"),
      stringr::str_subset(names(dataset), "^morsak")
    )
    dataset[, isoyearweek := cstime::date_to_isoyearweek_c(dodsdat)]
    min_isoyearweek <- min(skeleton[is_isoyear==FALSE]$isoyearweek)
    dataset[isoyearweek<min_isoyearweek, isoyearweek := paste0(cstime::date_to_isoyear_c(dodsdat),"-**")]
  } else stop("")

  for(i in seq_along(diags_or_ops_or_cods)){
    nam <- names(diags_or_ops_or_cods)[i]

    dataset[, (nam) := FALSE]
    dataset[, XXX_EXCLUDE := FALSE]

    for(ii in variables_containing_codes) for(iii in diags_or_ops_or_cods[[i]]){
      # check to see if it is an EXCLUSION factor or not
      if(stringr::str_detect(iii, "^!")){
        iii <- stringr::str_remove(iii, "!")
        iii <- paste0("^", iii)
        dataset[stringr::str_detect(get(ii), iii), XXX_EXCLUDE :=TRUE]
      } else {
        iii <- paste0("^", iii)
        dataset[stringr::str_detect(get(ii), iii), (nam):=TRUE]
      }
    }
    dataset[, (nam) := get(nam)==TRUE & XXX_EXCLUDE==FALSE]
    dataset[, XXX_EXCLUDE := NULL]
  }



  nam <- names(diags_or_ops_or_cods)
  txt <- paste0("reduced <- dataset[, .(", paste0(nam,"=as.logical(max(",nam,"))", collapse=", "),"), keyby=.(",id_name,", isoyearweek)]")
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

