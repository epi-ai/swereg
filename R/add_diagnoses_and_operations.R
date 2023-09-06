#' COVID-19 data for total age/sex in Norway (2020 border).
#'
#' @export
add_diagnoses <- function(
    skeleton,
    diagnoses_and_operations,
    id_name,
    diags = list(
      "icd10_F64_0" = c("^F640"),
      "icd10_F64_89" = c("^F6489"),
      "icd10_F64_089" = c("^F640", "^F648", "^F649")
    )
){
  add_diagnoses_or_operations(
    skeleton = skeleton,
    diagnoses_and_operations = diagnoses_and_operations,
    id_name = id_name,
    diags_or_ops = diags,
    type = "diags"
  )
}

#' COVID-19 data for total age/sex in Norway (2020 border).
#'
#' @export
add_operations <- function(
    skeleton,
    diagnoses_and_operations,
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
  add_diagnoses_or_operations(
    skeleton = skeleton,
    diagnoses_and_operations = diagnoses_and_operations,
    id_name = id_name,
    diags_or_ops = ops,
    type = "ops"
  )
}


add_diagnoses_or_operations <- function(
    skeleton,
    diagnoses_and_operations,
    id_name,
    diags_or_ops,
    type
){
  stopifnot(type %in% c("diags", "ops"))

  if(type == "diags"){
    variables_containing_codes <- c(
      stringr::str_subset(names(diagnoses_and_operations), "^HDIA"),
      stringr::str_subset(names(diagnoses_and_operations), "^hdia"),
      stringr::str_subset(names(diagnoses_and_operations), "^DIA"),
      stringr::str_subset(names(diagnoses_and_operations), "^EKOD")
    )
  } else {
    variables_containing_codes <- c(
      stringr::str_subset(names(diagnoses_and_operations), "^OP"),
      stringr::str_subset(names(diagnoses_and_operations), "^op")
    )
  }

  for(i in seq_along(diags_or_ops)){
    nam <- names(diags_or_ops)[i]

    diagnoses_and_operations[, (nam) := FALSE]
    diagnoses_and_operations[, XXX_EXCLUDE := FALSE]

    for(ii in variables_containing_codes) for(iii in diags_or_ops[[i]]){
      # check to see if it is an EXCLUSION factor or not
      if(stringr::str_detect(iii, "^!")){
        iii <- stringr::str_remove(iii, "!")
        iii <- paste0("^", iii)
        diagnoses_and_operations[stringr::str_detect(get(ii), iii), XXX_EXCLUDE :=TRUE]
      } else {
        iii <- paste0("^", iii)
        diagnoses_and_operations[stringr::str_detect(get(ii), iii), (nam):=TRUE]
      }
    }
    diagnoses_and_operations[, (nam) := get(nam)==TRUE & XXX_EXCLUDE==FALSE]
    diagnoses_and_operations[, XXX_EXCLUDE := NULL]
  }

  diagnoses_and_operations[, isoyearweek := cstime::date_to_isoyearweek_c(INDATUM)]
  min_isoyearweek <- min(skeleton[is_isoyear==FALSE]$isoyearweek)
  diagnoses_and_operations[isoyearweek<min_isoyearweek, isoyearweek := paste0(cstime::date_to_isoyear_c(INDATUM),"-**")]

  nam <- names(diags_or_ops)
  txt <- paste0("reduced <- diagnoses_and_operations[, .(", paste0(nam,"=as.logical(max(",nam,"))", collapse=", "),"), keyby=.(",id_name,", isoyearweek)]")
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

