x2023_mht_lmed_categorize_product_names <- function(x){
  x[, product_category := fcase(
        stringr::str_detect(produkt, 'Oestring') , 'A3',
        stringr::str_detect(produkt, 'Vagidonna') , 'A3',
        stringr::str_detect(produkt, 'Vagifem') , 'A3',
        stringr::str_detect(produkt, 'Vagirux') , 'A3',


        stringr::str_detect(produkt, 'Blissel') , 'A4',
        stringr::str_detect(produkt, 'Estrokad') , 'A4',
        stringr::str_detect(produkt, 'Ovesterin') , 'A4',
        stringr::str_detect(produkt, 'Gelistrol') , 'A4',

        stringr::str_detect(produkt, 'Divigel') , 'A1',
        stringr::str_detect(produkt, 'Estradot') , 'A1',
        stringr::str_detect(produkt, 'Estrogel') , 'A1',
        stringr::str_detect(produkt, 'Lenzetto') , 'A1',
        stringr::str_detect(produkt, 'Evorel') , 'A1',
        stringr::str_detect(produkt, 'Oesclim') , 'A1',
        stringr::str_detect(produkt, 'Climara') , 'A1',
        stringr::str_detect(produkt, 'Evopad') , 'A1',
        stringr::str_detect(produkt, 'FemSeven') , 'A1',

        stringr::str_detect(produkt, 'Progynon') , 'A2',
        stringr::str_detect(produkt, 'Femanest') , 'A2',


        stringr::str_detect(produkt, 'Oestriol') , 'A5',
        stringr::str_detect(produkt, 'Premarina') , 'A6',
        stringr::str_detect(produkt, 'Delestrogen') , 'A7',
        stringr::str_detect(produkt, 'Neofollin') , 'A7',


        stringr::str_detect(produkt, 'Estalis') , 'B1',
        stringr::str_detect(produkt, 'Estalis Sekvens') , 'B1',

        stringr::str_detect(produkt, 'Activelle') , 'B2',
        stringr::str_detect(produkt, 'Cliovelle') , 'B2',
        stringr::str_detect(produkt, 'Eviana') , 'B2',
        stringr::str_detect(produkt, 'Femanor') , 'B2',
        stringr::str_detect(produkt, 'Evorel') , 'B2',
        stringr::str_detect(produkt, 'Noresmea') , 'B2',
        stringr::str_detect(produkt, 'Kliogest') , 'B2',



        stringr::str_detect(produkt, 'Indivina') , 'B3',
        stringr::str_detect(produkt, 'Duova') , 'B3',
        stringr::str_detect(produkt, 'Premelle') , 'B3',
        stringr::str_detect(produkt, 'Premelle sekvens') , 'B3',


        stringr::str_detect(produkt, 'Femostonconti') , 'B4',

        stringr::str_detect(produkt, 'Climodien') , 'B5',

        stringr::str_detect(produkt, 'Angemin') , 'B6',

        stringr::str_detect(produkt, 'Sequidot') , 'B7',

        stringr::str_detect(produkt, 'Femasekvens') , 'B8',
        stringr::str_detect(produkt, 'Trisekvens') , 'B8',
        stringr::str_detect(produkt, 'Novofem') , 'B8',

        stringr::str_detect(produkt, 'Divina') , 'B9',
        stringr::str_detect(produkt, 'Trivina') , 'B9',


        stringr::str_detect(produkt, 'Femoston') , 'B11',


        stringr::str_detect(produkt, 'Cyclabil') , 'B11'

        ,
        stringr::str_detect(produkt, 'Crinone') , 'C1',
        stringr::str_detect(produkt, 'Cyclogest') , 'C1',
        stringr::str_detect(produkt, 'Lugesteron') , 'C1',
        stringr::str_detect(produkt, 'Lutinus') , 'C1',
        stringr::str_detect(produkt, 'Utrogest') , 'C1',
        stringr::str_detect(produkt, 'Utrogestan') , 'C1',
        stringr::str_detect(produkt, 'Progesteron') , 'C1',
        stringr::str_detect(produkt, 'Extempore') , 'C1',



        stringr::str_detect(produkt, 'Visanne') , 'C3',
        stringr::str_detect(produkt, 'Desogestrel') , 'C3',
        stringr::str_detect(produkt, 'Cerazette') , 'C3',
        stringr::str_detect(produkt, 'Azalia') , 'C3',
        stringr::str_detect(produkt, 'Gestrina') , 'C3',
        stringr::str_detect(produkt, 'Velavel') , 'C3',
        stringr::str_detect(produkt, 'Vinelle') , 'C3',
        stringr::str_detect(produkt, 'Zarelle') , 'C3',
        stringr::str_detect(produkt, 'Slinda') , 'C3',

        stringr::str_detect(produkt, 'Primolut') , 'C4',
        stringr::str_detect(produkt, '^Provera') , 'C4',
        stringr::str_detect(produkt, 'Duphaston') , 'C4',
        stringr::str_detect(produkt, 'Orgametril') , 'C4',
        stringr::str_detect(produkt, 'Gestapuran') , 'C4',
        stringr::str_detect(produkt, 'Duphaston') , 'C5',

        stringr::str_detect(produkt, '^Depo-Provera') , 'D1',
        stringr::str_detect(produkt, '^Depo-Progevera') , 'D1',
        stringr::str_detect(produkt, 'Nexplanon') , 'D2',
        stringr::str_detect(produkt, 'Implanon') , 'D2',
        stringr::str_detect(produkt, 'Follistrel') , 'D2',
        stringr::str_detect(produkt, 'Jadelle') , 'D3',

        stringr::str_detect(produkt, 'Mini-Pe') , 'D4',
        stringr::str_detect(produkt, 'Exlutena') , 'D5',
        stringr::str_detect(produkt, 'Jaydess') , 'E1',
        stringr::str_detect(produkt, 'Kyleena') , 'E1',
        stringr::str_detect(produkt, 'Mirena') , 'E1',
        stringr::str_detect(produkt, 'Livial') , 'F1',
        stringr::str_detect(produkt, 'Tibelia') , 'F1',
        stringr::str_detect(produkt, 'Tibocina') , 'F1',
        stringr::str_detect(produkt, 'Tibolon Aristo') , 'F1',
        stringr::str_detect(produkt, 'Tibolon Mylan') , 'F1',

        stringr::str_detect(produkt, 'Tibolon Orifarm') , 'F1',
        stringr::str_detect(produkt, 'Boltin') , 'F1',
        stringr::str_detect(produkt, 'Duavive') , 'G1',

        stringr::str_detect(produkt, 'Nebido') , 'H1',
        stringr::str_detect(produkt, 'Testogel') , 'H1',
        stringr::str_detect(produkt, 'Undestor') , 'H1',
        stringr::str_detect(produkt, 'Testoviron-Depot-250') , 'H1',
        stringr::str_detect(produkt, 'Intrinsa') , 'H1',
        stringr::str_detect(produkt, 'Testavan') , 'H1',
        stringr::str_detect(produkt, 'Testim') , 'H1',
        stringr::str_detect(produkt, 'Testoviron Depot') , 'H1',
        stringr::str_detect(produkt, 'Testoviron') , 'H1',
        stringr::str_detect(produkt, 'Tostran') , 'H1',
        stringr::str_detect(produkt, 'Tostrex') , 'H1'
    )
  ]
}

x2023_mht_apply_lmed_categories_to_skeleton <- function(skeleton, LMED){
  product_categories <- c(
    "A1", "A2", "A3", "A4", "A5", "A6", "A7",
    "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
    "C1", "C2", "C3", "C4", "C5",
    "D1", "D2", "D3", "D4",
    "E1",
    "F1",
    "H1",
    "I1", "I2"
  )
  setkey(LMED, start_isoyearweek, stop_isoyearweek, product_category)
  setkey(skeleton, id, isoyearweek)
  for(product in product_categories){
    skeleton[,(product) := FALSE]
  }
  for(product in product_categories){
    message(Sys.time()," ", product)
    LMED_product <- LMED[product_category == product]
    for(x_isoyearweek in sort(unique(skeleton$isoyearweek))){
      # identify all the women who received A1 in 2021-M01
      women_in_category_and_isoyearweek <- LMED_product[
        (start_isoyearweek <= x_isoyearweek & x_isoyearweek <= stop_isoyearweek)
      ]$P1193_LopNr_PersonNr %>% unique()

      if(length(women_in_category_and_isoyearweek)==0) next()
      # assign A1:=TRUE for all the women we found above, in 2021-M01
      skeleton[
        .(women_in_category_and_isoyearweek, x_isoyearweek),
        (product) := TRUE
      ]
    }
  }
  setorder(skeleton, id, isoyearweek)
}

x2023_mht_replace_false_runs <- function(x) {
  runs <- rle(x)
  runs$values[runs$values == FALSE & runs$lengths <= 4] <- TRUE
  inverse.rle(runs)
}

x2023_mht_cumulative_reset <- function(x) {
  grp_id <- rleid(!x)
  cumsum_reset <- ave(x, grp_id, FUN = cumsum)
  return(cumsum_reset)
}


x2023_mht_apply_lmed_approaches_to_skeleton <- function(skeleton){
  # approaches
  data_approach <- readxl::read_excel(
    system.file("2023-mht", "dataDictionary20230526.xlsx", package = "swereg"),
    sheet = "post_grouping"
  )
  setDT(data_approach)
  data_approach <- data_approach[!is.na(approach)]

  for(i in unique(data_approach$approach)){
    app <- data_approach[approach==i]
    for(j in unique(app$variable)) skeleton[, (j) := FALSE]

    for(j in 1:nrow(app)){
      x_approach <- app[j,]
      formula <- glue::glue("{x_approach$includes1}==T")
      if(!is.na(x_approach$includes2)) formula <- glue::glue("{formula} & {x_approach$includes2}==T")

      for(k in 1:30){
        dontinclude <- paste0("doesnotinclude",k)
        if(!is.na(x_approach[[dontinclude]])) formula <- glue::glue("{formula} & {x_approach[[dontinclude]]}==F")
      }
      formula <- glue::glue(
        'skeleton[{formula}, {x_approach$variable} := TRUE]'
      )
      eval(parse(text = formula))
    }

    # fill in the missing gaps (up to four weeks)
    for(j in unique(app$variable)){
      if(j=="local_or_none_mht") next()
      skeleton[, (j) := x2023_mht_replace_false_runs(get(j)), by=.(id)]
    }

    # how long they've been taking the drug for
    run_vars <- c()
    for(j in unique(app$variable)){
      if(j=="local_or_none_mht") next()
      var <- paste0("run_",j)
      run_vars <- c(run_vars, var)
      skeleton[, (var) := x2023_mht_cumulative_reset(get(j)), by=.(id)]
      skeleton[get(var)==0, (var) := 999999999]
    }

    skeleton[, row_min := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = run_vars]

    # combine them into the 'final' approach conclusion
    approach_name <- paste0("approach",i)
    skeleton[, (approach_name) := "local_or_none_mht"]
    for(j in unique(app$variable)){
      if(j=="local_or_none_mht") next()
      var <- paste0("run_",j)
      skeleton[get(var)==row_min & row_min != 999999999, (approach_name) := j]
    }

    skeleton[, row_min := NULL]
    for(j in run_vars) skeleton[, (j) := NULL]
    for(j in unique(app$variable)) skeleton[, (j) := NULL]
  }
}

#' @export
x2023_mht_add_lmed <- function(skeleton, lmed){
  message(Sys.time(), " LMED loading")
  message(Sys.time(), " LMED restricting")
  lmed <- lmed[P1193_LopNr_PersonNr %in% unique(skeleton$id)]
  message(Sys.time(), " LMED categorizing product names ")
  x2023_mht_lmed_categorize_product_names(lmed)

  # fixing IUDS
  lmed[product_category=="E1", fddd := 1680] # IUDs
  lmed[
    stringr::str_detect(produkt, 'Jaydess'),
    fddd := 1008
  ]

  message(Sys.time(), " LMED reducing size ")
  lmed <- lmed[!is.na(product_category)]
  lmed[, start_date := EDATUM]
  lmed[, stop_date := EDATUM + round(fddd)]
  message(Sys.time(), " LMED start/stop ")
  lmed[, start_isoyearweek := cstime::date_to_isoyearweek_c(start_date)]
  lmed[, stop_isoyearweek :=  cstime::date_to_isoyearweek_c(stop_date)]

  message(Sys.time(), " LMED apply categories to skeleton ")
  x2023_mht_apply_lmed_categories_to_skeleton(skeleton, lmed)
  message(Sys.time(), " LMED apply approaches ")
  x2023_mht_apply_lmed_approaches_to_skeleton(skeleton)
  message(Sys.time(), " LMED finished ")
  data.table::shouldPrint(skeleton)
}
