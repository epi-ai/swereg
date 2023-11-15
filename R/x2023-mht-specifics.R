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
        (start_isoyearweek <= x_isoyearweek & x_isoyearweek >= stop_isoyearweek)
      ]$P1193_LopNr_PersonNr %>% unique()

      # assign A1:=TRUE for all the women we found above, in 2021-M01
      skeleton[
        .(women_in_category_and_isoyearweek, x_isoyearweek),
        (product) := TRUE
      ]
    }
  }
  setorder(skeleton, id, isoyearweek)
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

    # combine them into the 'final' approach conclusion
    approach_name <- paste0("approach",i)
    skeleton[, (approach_name) := "none"]

    skeleton[, num_approaches := Reduce(`+`, .SD), .SDcol = unique(app$variable)]
    for(j in unique(app$variable)){
      skeleton[num_approaches==1 & get(j)==T, (approach_name) := j]
    }
    skeleton[num_approaches >= 2, (approach_name) := "multi"]

    for(j in unique(app$variable)) skeleton[, (j) := NULL]
  }
  skeleton[, num_approaches := NULL]
}

#' @export
x2023_mht_add_lmed <- function(skeleton, folder){
  message(Sys.time(), " LMED loading")
  LMED <- data.table::fread(
    fs::path(folder,"/sos/T_T_R_LMED__12831_2021.txt"),
    select = c(
      "P1193_LopNr_PersonNr",
      "EDATUM",
      "ATC",
      "produkt",
      "fddd"
    )
  )
  message(Sys.time(), " LMED restricting")
  LMED <- LMED[P1193_LopNr_PersonNr %in% unique(skeleton$id)]
  message(Sys.time(), " LMED categorizing product names ")
  x2023_mht_lmed_categorize_product_names(LMED)
  message(Sys.time(), " LMED reducing size ")
  LMED <- LMED[!is.na(product_category)]
  LMED[, start_date := EDATUM]
  LMED[, stop_date := EDATUM + round(fddd)]
  message(Sys.time(), " LMED start/stop ")
  LMED[, start_isoyearweek := cstime::date_to_isoyearweek_c(start_date)]
  LMED[, stop_isoyearweek :=  cstime::date_to_isoyearweek_c(stop_date)]

  message(Sys.time(), " LMED apply categories to skeleton ")
  x2023_mht_apply_lmed_categories_to_skeleton(skeleton, LMED)
  message(Sys.time(), " LMED apply approaches ")
  x2023_mht_apply_lmed_approaches_to_skeleton(skeleton)
  message(Sys.time(), " LMED finished ")
  data.table::shouldPrint(skeleton)
}
