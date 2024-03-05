
server <- function(input, output, session) {
  
  ## Help pages
  
  observeEvent(input$help_prefilter, {
    
    showModal(
      session = session,
      modalDialog(title = "Pre-filtering based on the hazard identification results", 
                  easyClose = TRUE,
                  size = "xl",
                  tagList(
                    tags$p("This step is to rank the prioritized microbial hazards identified via the Microbiological Hazards IDentification Tool (MiID DSS)."),
                    tags$a("MiID", href="https://foodmicrobiologywur.shinyapps.io/Microbial_hazards_ID/"),
                    tags$p("Refer to the instruction in the MiID tool to download the final results in a csv file, and upload it here."),
                    tags$p("If this step is NOT DONE, all 33 Microbiological Hazards are ranked.")
                  )
      )
    )
    
  })
  
  observeEvent(input$help_food_pars, {
    
    showModal(
      session = session,
      modalDialog(title = "Food parameters", 
                  easyClose = TRUE,
                  size = "xl",
                  tagList(
                    tags$p(paste("Select the food item of your choice from the drop down menu and specify the product characteristics if it does not 
                                 match with the default setting in this tool. More than 1 selection is possible.",
                                 "High in fat content, completely dry (water activity aw <0.5), or partially dry/ viscous products (0.5 – 0.9)."
                    )
                    ),
                    tags$p(paste(
                      "For wet products (aw ~0.99), no selection is needed. Select the pH category for the food item of your choice.",
                      "If there is NO selection in this step, a default prefilled value is used."
                      )
                    )
                  )
      )
    )
    
  })
  
  observeEvent(input$help_factory_pars, {
    
    showModal(
      session = session,
      modalDialog(title = "Factory parameters", 
                  easyClose = TRUE,
                  size = "xl",
                  tagList(
                    tags$p(
                      paste(
                        "Please select how the food item of your choice is being processed using the guide tables showing the processing conditions below.",
                        "If extra ingredients were added AFTER processing, please uncheck the box for Aseptic processing, and select the relevant category."
                      )
                    ),
                    tags$h4(
                      "Wet environment"
                    ),
                    tags$p(
                      paste("Indicates during food processing, the surrounding environment contains water, or water vapor or is humid. 
                            If this is not the case, please select dry environment")
                    )
                  ),
                  tags$h4(
                    "Human cross-contamination"
                  ),
                  tags$p(
                    "Refers to humans are involved in preparing and/or handling the food products"
                  ),
                  reactable(FACTORY_PARS_DESCRIPTION)
      )
    )
    
  })
  
  observeEvent(input$help_distribution_pars, {
    
    showModal(
      session = session,
      modalDialog(title = "Distribution parameters", 
                  easyClose = TRUE,
                  size = "xl",
                  tagList(
                    tags$p(
                      "Please select how the food item is being transported and/or stored."
                    ),
                    tags$h4(
                      "Refrigeration"
                    ),
                    tags$p(
                      "Refers to storage temperature at ~ 1- 4°C"
                    ),
                    tags$h4(
                      "Freezing"
                    ),
                    tags$p(
                      "Refers to storage temperature at 0°C or < 0°C"
                    )
                  )
      )
    )
    
  })
  
  observeEvent(input$plus_comments, {

    showModal(
      session = session,
      modalDialog(title = "Comments on process parameters",
                  easyClose = TRUE,
                  size = "l",
                  tagList(
                    tableOutput("plus_table")
                  )
      )
    )

  })
  
  output$plus_table <- renderTable({
    
    out <- FULL_DATA$`Step 2_PE` %>%
      select(Type, Genus, Species, `Observation high fat`, `Observation dry`)
    
    if (input$prefilter) {
      out <- left_join(prefilter_species(), out)
    }
    
    out %>% filter(!is.na(`Observation high fat`) | !is.na(`Observation dry`))
    
    
  })
  
  ## Prefilter based on excel
  
  excelFile <- reactive({
    input$excel_file
  })
  
  observeEvent(excelFile(), {  # Update the choices
    
    validate(need(excelFile(), message = ""))
    updateSelectInput(session = session,
                      inputId = "excel_sheet",
                      choices = excel_sheets(excelFile()$datapath) 
    )
    
  })
  
  excel_frame <- reactive({
    read_excel(excelFile()$datapath,
               sheet = input$excel_sheet
               )
  })
  
  prefilter_species <- reactive({
    validate(need(excelFile(), message = ""))
    
    excel_frame() %>%
      select(Genus, Species) %>%
      group_by(Genus, Species) %>%
      summarize()
  })
  
  output$prefilter_n <- renderTable({
    validate(need(prefilter_species(), message = ""))
    # browser()
    prefilter_species()
  })
  
  ## Reactive with the filters -------------------------------------------------

  d_step1 <- reactive({  # Filter of step 1

    validate(
      need(input$food_item, message = "")
    )
    
    categories <- FULL_DATA$table_properties %>%
      filter(Food_item == input$food_item)

    FULL_DATA$`Step 1b` %>%
      filter(Food_sub_category == categories$Food_sub_category[1]) %>%
      filter(Food_main_category == categories$Food_main_category[1])

  })

  d_step2 <- reactive({  # Filter of step 2

    validate(
      need(input$process_technique, message = "")
      )
    
    # browser()
    
    FULL_DATA$`Step 2_PE` %>% 
      filter(`Processing condition` == input$process_technique)

  })
  
  d_step3 <- reactive({  # Filter of step 3
    
    FULL_DATA$`Step 3_RP`
      
  })
  
  get_score4 <- function(food_pars, 
                         food_pH, 
                         temp,
                         need_growth,
                         can_grow_ph4p5, can_grow_ph_4p8, can_grow_ph_5to9, can_grow_phmorethan10,
                         can_grow_aw_0p99, can_grow_aw_0p5, can_grow_aw_0p5_0p85,
                         can_grow_rt, can_grow_refrig, can_grow_refrig_ta, can_grow_freeze) {
    
    ## Check for the pH
    
    value_pH <- switch(food_pH,
                       `pH < 4.5` = can_grow_ph4p5,
                       `4.5 < pH < 4.8` = can_grow_ph_4p8,
                       `pH > 10` = can_grow_phmorethan10,
                       Neutral = 1
    )
    
    ## Check for the aw
    
    if ("Dry product (aw < 0.5)" %in% food_pars) {
      
      value_aw <- can_grow_aw_0p5
      
    } else if ("Low aw (0.5 - 0.9)" %in% food_pars) {
      
      value_aw <- can_grow_aw_0p5_0p85
      
    } else {
      
      value_aw <- 1
      
    }
    
    ## Check for the temperature
    
    value_temp <- switch(temp,
                         `Room temperature` = can_grow_rt,
                         Refrigeration = can_grow_refrig,
                         Frozen = can_grow_freeze
    )
    
    ## Check for temperature abuse
    
    if (temp == "Refrigeration") {
      
      if (input$temp_abuse) {
        
        value_temp <- can_grow_refrig_ta
        
      }
      
    }
    
    ## Get the minimum score
    
    out <- pmin(value_pH,
                value_aw,
                value_temp)
    
    ## Update for the ones that do not need growth
    
    out[!need_growth]  <- 1
    
    ## Return
    
    out
    
    
  }
  
  d_step4 <- reactive({  # Filter of step 4
    
    food_pars <- input$dry_fat
    food_pH <- input$acidity
    temp <- input$stor_conditions
    
    FULL_DATA$`Step 4_PPC_2` %>%
      mutate(score4 = get_score4(
        food_pars, 
        food_pH, 
        temp,
        need_growth,
        can_grow_ph4p5, can_grow_ph4p8, can_grow_ph_5to9,can_grow_phmorethan10,
        can_grow_aw_0p99, can_grow_aw_0p5, can_grow_aw_0p5_0p85,
        can_grow_rt, can_grow_refrig, can_grow_refrig_ta, can_grow_freeze
      ))
    

    
  })
  
  d_step5 <- reactive({  # Filter of step 5
    
    FULL_DATA$`Step 5_MP`
    
  })
  
  d_step6 <- reactive({  # Filter of step 6
    
    validate(
      need(input$food_item, message = "")
    )
    
    categories <- FULL_DATA$table_properties %>%
      filter(Food_item == input$food_item)
    
    FULL_DATA$`Step 6_HFA` %>%
      filter(Food_sub_category == categories$Food_sub_category[1]) %>%
      filter(Food_main_category == categories$Food_main_category[1])
    
  })
  
  d_step7 <- reactive({
    
    validate(
      need(input$food_item, message = "")
    )
    
    #########
    # browser()
    #########
    # 
    # categories <- FULL_DATA$table_properties %>%
    #   filter(Food_item == input$food_item)
    # 
    # FULL_DATA$`Step 7_FC` %>%
    #   filter(`Food subcategory` == categories$Food_sub_category[1]) %>%
    #   filter(Food_main_category == categories$Food_main_category[1])
    
    FULL_DATA$`Step 7_FC` %>%
      filter(Food_item == input$food_item)
      # filter(`Food subcategory` == categories$Food_sub_category[1]) %>%
      # filter(Food_main_category == categories$Food_main_category[1])

  })
  
  d_step8 <- reactive({
    
    FULL_DATA$`Step 8_HS`
    
  })
  
  ## Updating the product factors when changing the item

  observeEvent(input$food_item, {
    
    # browser()

    this_d <- FULL_DATA$table_properties %>%
      filter(Food_item == input$food_item)

    this_d <- this_d[1,]  # In case more than one is selected

    selected <- c()

    # if (this_d$pH_category == "Neutral") {
    #   selected <- c(selected, "Neutral")
    # }

    if (this_d$Fat_category == "high fat") {
      selected <- c(selected, "High fat")
    }

    if (this_d$Aw_category == "Aw > 0.5 < 0.9") {
      selected <- c(selected, "Low aw (0.5 - 0.9)")
    }

    if (this_d$Aw_category %in% c("Aw 0.2-0.5", "Aw 0.2 - 0.5")) {
      selected <- c(selected, "Dry product (aw < 0.5)")
    }

    updatePrettyCheckboxGroup(inputId = "dry_fat",
                              selected = selected
                              )
    # print(selected)
    
    updatePrettyCheckboxGroup(inputId = "acidity", 
                              selected = this_d$pH_category
                              )
    
  })

  # Output of the filtes ------------------------------------------------------
  # 
  # output$filter_1 <- renderTable(
  #   d_step1()
  # )
  # 
  # output$filter_2 <- renderTable(
  #   d_step6()
  # )
  
  ## Calculation of the score --------------------------------------------------
  
  scores <- reactive({
    
    ############
    # browser()
    ###########
    
    ## Step 1
    
    validate(
      need(d_step1(), message = "")
    )
    
    out <- d_step1() %>%
      select(Type, Genus, Species, score1 = Count)
    
    ## Step 2
    
    validate(
      need(d_step2(), message = "")
    )
    
    #############
    # browser()
    #############
    
    product_pars <- input$dry_fat
    
    product_pars <- product_pars[product_pars != "Neutral"]  # Acid does not matter here
    
    if (length(product_pars) == 0) {
      
      out <- d_step2() %>%
        select(Type, Genus, Species, score2 = `Hazard inactivation`) %>%
        full_join(out, .)
      
    } else if (is.null(product_pars)) {
      
      out <- d_step2() %>%
        select(Type, Genus, Species, score2 = `Hazard inactivation`) %>%
        full_join(out, .)
      
    } else if (length(product_pars > 1)) {  # Both high fat and low aw
      
      out <- d_step2() %>%
        rowwise() %>%
        mutate(score2 = max(`Hazard inactivation_highfat`, `Hazard inactivation_dryfood`)) %>% 
        select(Type, Genus, Species, score2) %>%
        full_join(out, .)
      
    } else if(product_pars == "High fat") {
      
      out <- d_step2() %>%
        select(Type, Genus, Species, score2 = `Hazard inactivation_highfat`) %>%
        full_join(out, .)
      
    } else if(product_pars == "Low aw (0.5 - 0.9)") {
      
      out <- d_step2() %>%
        select(Type, Genus, Species, score2 = `Hazard inactivation_dryfood`) %>%
        full_join(out, .)
      
    } else if(product_pars == "Dry product (aw < 0.5)") {
      
      out <- d_step2() %>%
        select(Type, Genus, Species, score2 = `Hazard inactivation_dryfood`) %>%
        full_join(out, .)
    }
    
    ###########
    # browser()
    ##########
    
    ## Step 3
    
    validate(
      need(d_step3(), message = "")
    )
    
    if (input$aseptic) {  # No recontamination probability
      out <- d_step3() %>%
        mutate(score3 = NA) %>%
        select(Type, Genus, Species, score3) %>%
        full_join(out, .)
      
    } else {
      
      ####
      # browser()
      ###
      
      rec <- input$recontaminations
      
      if (length(rec) == 0) {  # No recontaminations
        
        out <- d_step3() %>%
          mutate(score3 = 0) %>%
          select(Type, Genus, Species, score3) %>%
          full_join(out, .)
        
      } else {
        
        out <- d_step3() %>%
          mutate(
            `RP_EF_wet` = ifelse("Wet environment" %in% rec, 
                                 `RP_EF_wet`,
                                 1),
            `RP_EF_dry` = ifelse("Dry environment" %in% rec, 
                                 `RP_EF_dry`,
                                 1),
            `RP_FH_adddryspieces` = ifelse("Addition of dry spices" %in% rec, 
                                           `RP_FH_adddryspieces`,
                                           1),
            `RP_FH_adddryvitamins` = ifelse("Addition of dry vitamins" %in% rec, 
                                            `RP_FH_adddryvitamins`,
                                            1),
            `RP_FH_otherdryingredients` = ifelse("Addition of other dry ingredients" %in% rec, 
                                                 `RP_FH_otherdryingredients`,
                                                 1),
            `RP_FH_human` = ifelse("Human cross-contamination" %in% rec, 
                                   `RP_FH_human`,
                                   1)
          ) %>%
          mutate(score3 = RP_EF_wet *RP_EF_dry * RP_FH_adddryspieces * RP_FH_adddryvitamins * RP_FH_otherdryingredients * RP_FH_human) %>%
          select(Type, Genus, Species, score3) %>%
          full_join(out, .)
        
      }
      
    }
    
    ## Step 4
    
    validate(
      need(d_step4(), message = "")
    )
    
    # browser()
    
    out <- d_step4() %>%
      select(Type, Genus, Species, score4) %>%
      full_join(out, .)
    
    ## Step 5
    
    ###########
    # browser()
    ##########
    
    validate(
      need(d_step5(), message = "")
    )
    
    cook <- input$cooking
    
    ###
    # browser()
    ####
    
    out <- d_step5() %>%
      mutate(
        score5 = ifelse(
          cook == "Ready to eat product",
          No_Further_cooking,
          ifelse(
            cook == "Cooking >70ºC in the whole product",
            `Further_cooking (< 70°C)`,
            `Further_cooking ( >70°C)`
          )
        )
      ) %>%
      select(Type, Genus, Species, score5) %>%
      full_join(out, .)
    
    ## Step 6
    
    validate(
      need(d_step6(), message = "")
    )
    
    out <- d_step6() %>%
      rowwise() %>%
      mutate(score6 = `Final prevalence_4th root`) %>%
      select(Type, Genus, Species, score6) %>%
      full_join(out, .)
    
    ## Step 7
    
    validate(
      need(d_step7(), message = "")
    )
    
    out <- out %>%
      mutate(score7 = d_step7()$`Average (P)`)
    
    ## Step 8
    
    validate(
      need(d_step8(), message = "")
    )
    
    out <- d_step8() %>%
      select(Type, Genus, Species, score8 = `Risk value (DALY/Case)`) %>%
      full_join(out, .)
    
    ## Apply the ugly filter
    
    if (input$ugly_filter) {
      out <- out %>%
        filter(score1 > 1)
    }
    
    out <- out %>% select(-score1)
    
    ## Ranking
    
    # browser()

    score_sum <- out %>%
      select(starts_with("score")) %>%
      rowSums(na.rm = TRUE)
    
    score_prod <- out %>%
      mutate(score2 = ifelse(is.na(score2), 0, score2),
             score3 = ifelse(is.na(score3), 0, score3)
      ) %>%
      mutate(score23 = score2 + score3) %>%  # These 2 must be added now
      select(-score2, -score3) %>%
      select(starts_with("score")) %>%
      pmap_dbl(., prod, na.rm = TRUE)
    
    out <- out %>%
      mutate(score_sum = score_sum,
             score_prod = score_prod)
    
    ## Include only those filtered
    
    if (input$prefilter) {
      out <- left_join(prefilter_species(), out)
    }
    
    ## Convert to the qualitative index
    
    #####
    # browser()
    ######
    
    # #- Score 1
    # 
    # out <- out %>% mutate(score_qual1 = score1)
    
    #- Score 2
    
    table <- CONVERSION_TABLE$`Step 2_PE` %>%
      mutate(low = ifelse(is.na(low), -Inf, low),
             high = ifelse(is.na(high), Inf, high))
    
    out <- out %>% mutate(score_qual2 = NA)
    
    for (i in 1:nrow(table)) {
      
      out <- out %>%
        mutate(score_qual2 = ifelse(between(score2, table[i,]$low, table[i,]$high), table[i,]$Score, score_qual2))
      
    }
    
    ###########
    # browser()
    ##########
    
    #- Score 3
    
    table <- CONVERSION_TABLE$`Step 3_RP` %>%
      mutate(low = ifelse(is.na(low), -Inf, low),
             high = ifelse(is.na(high), Inf, high))
    
    out <- out %>% mutate(score_qual3 = NA)
    
    for (i in 1:nrow(table)) {
      
      out <- out %>%
        mutate(score_qual3 = ifelse(between(score3, table[i,]$low, table[i,]$high), 
                                    table[i,]$Score, score_qual3))
      
    }
    
    ############
    # browser()
    #############
    
    #- Score 4
    
    table <- CONVERSION_TABLE$`Step 4_PPC` %>%
      mutate(low = ifelse(is.na(low), -Inf, low),
             high = ifelse(is.na(high), Inf, high))
    
    out <- out %>% mutate(score_qual4 = NA)
    
    for (i in 1:nrow(table)) {
      
      out <- out %>%
        mutate(score_qual4 = ifelse(between(score4, table[i,]$low, table[i,]$high), 
                                    table[i,]$Score, 
                                    score_qual4))
      
    }
    
    ############
    # browser()
    #############
    
    #- Score 5
    
    table <- CONVERSION_TABLE$`Step 5_MP` %>%
      mutate(low = ifelse(is.na(low), -Inf, low),
             high = ifelse(is.na(high), Inf, high))
    
    out <- out %>% mutate(score_qual5 = NA)
    
    for (i in 1:nrow(table)) {
      
      out <- out %>%
        mutate(score_qual5 = ifelse(between(score5, table[i,]$low, table[i,]$high), 
                                    table[i,]$Score, 
                                    score_qual5))
      
    }
    
    ############
    # browser()
    ############
    
    #- Score 6
    
    table <- CONVERSION_TABLE$`Step 6_HFA` %>%
      mutate(low = ifelse(is.na(low), -Inf, low),
             high = ifelse(is.na(high), Inf, high))
    
    out <- out %>% mutate(score_qual6 = NA)
    
    for (i in 1:nrow(table)) {
      
      out <- out %>%
        mutate(score_qual6 = ifelse(between(score6, table[i,]$low, table[i,]$high), 
                                    table[i,]$Score, 
                                    score_qual6
        )
        )
    }
    
    ############
    # browser()
    ############
    
    #- Score 7
    
    table <- CONVERSION_TABLE$`Step 7_FC` %>%
      mutate(low = ifelse(is.na(low), -Inf, low),
             high = ifelse(is.na(high), Inf, high))
    
    out <- out %>% mutate(score_qual7 = NA)
    
    for (i in 1:nrow(table)) {
      
      out <- out %>%
        mutate(score_qual7 = ifelse(between(score7, table[i,]$low, table[i,]$high), 
                                    table[i,]$Score, 
                                    score_qual7
        )
        )
    }
    
    ############
    # browser()
    ############
    
    #- Score 8
    
    table <- CONVERSION_TABLE$`Step 8_HS` %>%
      mutate(low = ifelse(is.na(low), -Inf, low),
             high = ifelse(is.na(high), Inf, high))
    
    out <- out %>% mutate(score_qual8 = NA)
    
    for (i in 1:nrow(table)) {
      
      out <- out %>%
        mutate(score_qual8 = ifelse(between(score8, table[i,]$low, table[i,]$high), 
                                    table[i,]$Score, 
                                    score_qual8
        )
        )
    }
    
    ############
    # browser()
    ############
    
    
    score_sum_qual <- out %>%
      ungroup() %>%
      select(starts_with("score_qual")) %>%
      rowSums(na.rm = TRUE)
    
    score_prod_qual <- out %>%
      ungroup() %>%
      mutate(score_qual2 = ifelse(is.na(score_qual2), 0, score_qual2),
             score_qual3 = ifelse(is.na(score_qual3), 0, score_qual3)
             ) %>%
      mutate(score_qual23 = score_qual2 + score_qual3) %>%  # These 2 must be added now
      select(-score_qual2, -score_qual3) %>%
      select(starts_with("score_qual")) %>%
      pmap_dbl(., prod, na.rm = TRUE)
    
    out <- out %>%
      ungroup() %>%
      mutate(score_qual_sum = score_sum_qual,
             score_qual_prod = score_prod_qual)
    
    ## Return ------------------------------------------------------------------
    
    out %>% ungroup()
    
  })
  
  ## Score table ---------------------------------------------------------------
  
  output$score_table_qual <- renderReactable({
    
    validate(
      need(scores(), message = "")
    )
    
    
    out <- scores() %>%
      select(Type, Genus, Species,
             `Semi-quantitative score (product)` = score_qual_prod,
             `Semi-quantitative score (sum)` = score_qual_sum,
             # `C1` = score_qual1,
             `C2` = score_qual2,
             `C3` = score_qual3,
             `C4` = score_qual4,
             `C5` = score_qual5,
             `C6` = score_qual6,
             `C7` = score_qual7,
             `C8` = score_qual8
             )
    
    # browser()
    
    ## Output
    
    reactable(out,
              searchable = TRUE,
              defaultPageSize = 10,
              defaultColDef = colDef(
                style = color_scales(out),
                format = colFormat(digits = 3)
              )
    )
    
  })
  
  output$score_table_quant <- renderReactable({
    
    validate(
      need(scores(), message = "")
    )

    out <- scores() %>%
      select(Type, Genus, Species,
             `Quantitative score (product)` = score_prod,
             `Quantitative score (sum)` = score_sum,
             # `C1` = score1,
             `C2` = score2,
             `C3` = score3,
             `C4` = score4,
             `C5` = score5,
             `C6` = score6,
             `C7` = score7,
             `C8` = score8
             ) 

    ## Output

   reactable(out,
             searchable = TRUE,
             defaultColDef = colDef(
               style = color_scales(out)
             )
             )
    
    
  })
  
  ## Plot of the scores --------------------------------------------------------
  
  output$plot_scores <- renderPlotly({
    
    validate(
      need(scores(), message = "")
    )
    
    p <- scores() %>%
      select(Genus, Species,
             matches(input$methods_used)
             # score_prod,
             # score_sum,
             # score_qual_prod,
             # score_qual_sum
             ) %>%
      unite(hazard, Genus, Species) %>%
      pivot_longer(-hazard, names_to = "method", values_to = "score") %>%
      group_by(method) %>%
      mutate(rank = rank(score)) %>%
      mutate(rank = max(rank) - rank) %>%
      ungroup() %>%
      group_by(hazard) %>%
      mutate(average = mean(rank, na.rm = TRUE)) %>%
      group_by(hazard) %>%
      mutate(mean_rank = mean(rank, na.rm = TRUE)) %>%
      arrange(desc(mean_rank)) %>%
      mutate(., hazard = factor(hazard, levels = unique(.$hazard))) %>%
      mutate(method = ifelse(method == "score_prod", "Semi-quantitative (product)", method),
             method = ifelse(method == "score_sum", "Semi-quantitative (sum)", method),
             method = ifelse(method == "score_qual_prod", "Qualitative (product)", method),
             method = ifelse(method == "score_qual_sum", "Qualitative (sum)", method)
             ) %>%
      ggplot() + 
      geom_point(aes(x = hazard, y = rank, colour = method)) +
      xlab("") +
      coord_flip()
    
    ggplotly(p)
    
  })
  
  ## Sensitivity analysis ------------------------------------------------------
  
  sensitivity_scores_qual <- reactive({
    
    validate(
      need(scores(), message = ""),
      # need(input$sensi_pars, message = "")
    )
    
    # browser()
    
    ## Get the scores and remove the result
    
    out <- scores() %>%
      select(Type, Genus, Species, 
             C2 = score_qual2,
             C3 = score_qual3,
             C4 = score_qual4,
             C5 = score_qual5,
             C6 = score_qual6,
             C7 = score_qual7,
             C8 = score_qual8
             )
    # select(-score_sum, -score_prod, -score_qual_sum, -score_qual_prod)
    
    ## Remove the ones we are not including
    
    if (!is.null(input$sensi_pars_qual)) {
      
      out <- out %>%
        select(-matches(input$sensi_pars_qual))
      
    }
    
    
    ## Calculate the scores and return
    
    # browser()
    
    score_sum <- out %>%
      ungroup() %>%
      select(starts_with("C")) %>%
      rowSums(na.rm = TRUE)

    C23 <- out %>% 
      ungroup() %>%
      select(matches("C2"), matches("C3")) %>%
      rowSums(na.rm = TRUE)
    
    C23 <- ifelse(C23 == 0, NA, C23)
    
    score_prod <- out %>%
      ungroup() %>%
      select(-matches("C2"), -matches("C3")) %>%
      mutate(C23 = C23) %>%  # These 2 must be added now
      select(starts_with("C")) %>%
      pmap_dbl(., prod, na.rm = TRUE)
    
    out <- out %>%
      ungroup() %>%
      mutate(score_sum = score_sum,
             score_prod = score_prod)
    
    out
    
    
  })
  
  sensitivity_scores_quant <- reactive({
    
    validate(
      need(scores(), message = ""),
      # need(input$sensi_pars, message = "")
    )
    
    # browser()
    
    ## Get the scores and remove the result
    
    out <- scores() %>%
      select(Type, Genus, Species,
             C2 = score2,
             C3 = score3,
             C4 = score4,
             C5 = score5,
             C6 = score6,
             C7 = score7,
             C8 = score8)
    
    ## Remove the ones we are not including
    
    if (!is.null(input$sensi_pars_quant)) {
      
      out <- out %>%
        select(-matches(input$sensi_pars_quant))
      
    }
    
    
    ## Calculate the scores and return
    
    score_sum <- out %>%
      ungroup() %>%
      select(starts_with("C")) %>%
      rowSums(na.rm = TRUE)
    
    C23 <- out %>% 
      ungroup() %>%
      select(matches("C2"), matches("C3")) %>%
      rowSums(na.rm = TRUE)
    
    C23 <- ifelse(C23 == 0, NA, C23)
    
    score_prod <- out %>%
      ungroup() %>%
      select(-matches("C2"), -matches("C3")) %>%
      mutate(C23 = C23) %>%  # These 2 must be added now
      select(starts_with("C")) %>%
      pmap_dbl(., prod, na.rm = TRUE)
    
    out <- out %>%
      ungroup() %>%
      mutate(score_sum = score_sum,
             score_prod = score_prod)
    
    out
    
    
  })
  
  ## Sensitivity table ---------------------------------------------------------
  
  output$sensitivity_table_qual <- renderReactable({
    
    # browser()
    
    validate(
      need(sensitivity_scores_qual(), message = "")
    )
    
    out <- sensitivity_scores_qual() %>%
      select(Type, Genus, Species,
             `Total score (product)` = score_prod,
             `Total score (sum)` = score_sum,
             everything())
    
    # browser()
    
    ## Output
    
    reactable(out,
              searchable = TRUE,
              defaultPageSize = 10,
              defaultColDef = colDef(
                style = color_scales(out),
                format = colFormat(digits = 3)
              )
    )
    
  })
  
  output$sensitivity_table_quant <- renderReactable({
    
    # browser()
    
    validate(
      need(sensitivity_scores_quant(), message = "")
    )
    
    out <- sensitivity_scores_quant() %>%
      select(Type, Genus, Species,
             `Total score (product)` = score_prod,
             `Total score (sum)` = score_sum,
             everything())
    
    # browser()
    
    ## Output
    
    reactable(out,
              searchable = TRUE,
              defaultPageSize = 10,
              defaultColDef = colDef(
                style = color_scales(out),
                format = colFormat(digits = 3)
              )
    )
    
  })
  
  ## Results download ----------------------------------------------------------
  
  output$down_qual_score <- downloadHandler(
    filename = function() {
      paste('qual_score-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      
      validate(
        need(scores(), message = "")
      )
      
      
      out <- scores() %>%
        select(Type, Genus, Species,
               `Semi-quantitative score (product)` = score_qual_prod,
               `Semi-quantitative score (sum)` = score_qual_sum,
               # `C1` = score_qual1,
               `C2` = score_qual2,
               `C3` = score_qual3,
               `C4` = score_qual4,
               `C5` = score_qual5,
               `C6` = score_qual6,
               `C7` = score_qual7,
               `C8` = score_qual8
        )
      
      ## Output
      
      write.csv(out, con)
    }
  )
  
  output$down_quant_score <- downloadHandler(
    filename = function() {
      paste('quant_score-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      
      
      validate(
        need(scores(), message = "")
      )
      
      out <- scores() %>%
        select(Type, Genus, Species,
               `Quantitative score (product)` = score_prod,
               `Quantitative score (sum)` = score_sum,
               # `C1` = score1,
               `C2` = score2,
               `C3` = score3,
               `C4` = score4,
               `C5` = score5,
               `C6` = score6,
               `C7` = score7,
               `C8` = score8
        ) 
      
      ## Output
      
      write.csv(out, con)
    }
  )
  
}
