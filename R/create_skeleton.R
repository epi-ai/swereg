#' COVID-19 data for total age/sex in Norway (2020 border).
#'
#' @export
create_skeleton <- function(
  ids,
  date_min,
  date_max
  ){

  # isoyears
  skeleton_isoyear <- expand.grid(
    id = ids,
    isoyear = 1900:cstime::date_to_isoyear_n(as.Date(date_min)-1),
    stringsAsFactors = FALSE
  ) %>% setDT()
  skeleton_isoyear[, isoyearweek := paste0(isoyear,"-**")]
  skeleton_isoyear[, is_isoyear := TRUE]

  # isoyearweeks
  isoyearweeks <- seq.Date(
    as.Date(date_min),
    as.Date(date_max),
    1
  ) %>%
    cstime::date_to_isoyearweek_c() %>%
    unique()

  skeleton_isoyearweek <- expand.grid(
    id = ids,
    isoyearweek = isoyearweeks,
    stringsAsFactors = FALSE
  ) %>% setDT()
  skeleton_isoyearweek[, isoyear := cstime::isoyearweek_to_isoyear_n(isoyearweek)]
  skeleton_isoyearweek[, is_isoyear := FALSE]

  skeleton <- rbindlist(list(skeleton_isoyear, skeleton_isoyearweek), use.names=T)
  # skeleton[, isoyearweeksun := cstime::isoyearweek_to_last_date(isoyearweek)]

  setcolorder(skeleton, c("id", "isoyear", "isoyearweek", "is_isoyear"))
  setorder(skeleton, id, isoyearweek)

  return(skeleton)
}
