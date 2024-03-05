
ui <- fullPage(
  
  ## Activate prompter
  use_prompt(),
  
  opts = list(
    # navigation = TRUE,
    # navigationPosition = "left",
    slidesNavigation = TRUE,
    sectionsColor = c(
      "#FE9199",# welcome
      "#FFFFFF", # prefilter
      "#FFFFFF", # criteria
      "#FFFFFF",# ranking
      "#FFFFFF", # sensitivity
      "white"# about
      
    )
  ),
  menu = c(Home = "welcome",
           `Pre-filter` = "prefilter_slide",
           Criteria = "criteria",
           # Filters = "filters",
           Ranking = "ranking",
           Sensitivity = "sensitivity",
           About = "about"
           ),

  fullSectionImage(  # Welcome ------------------------------------------------------
    menu = "welcome",
    center = TRUE,
    # img = "Homepage_new.png"
    img = "Homepage_new.png"
    
    # # fullContainer(
    #   fullRow(
    #     fullColumn(
    #       width = 12,
    #       tags$h1("Claire's tool")
    #     )
    #   ),
    #   fullRow(
    #     fullColumn("One pic"),
    #     fullColumn("Other pic")
    #   )
    # )
  ),
  
  fullSectionImage(  # Pre-filtering ------------------------------------------------
    menu = "prefilter_slide",
    img = "miid_page.png",
    fullContainer(
      fullRow(
        fullColumn(
          width = 12,
          h2(
            tagList(
              actionBttn("help_prefilter", 
                         label = NULL,
                         icon = icon("question"),
                         style = "gradient"
              ),
              "Pre-filter Hazards based on Hazard Identification MiID DSS Output")
            )
        )
      ),
      hr(),
      fullRow(
        fullColumn(
          prettyCheckbox(  # Step 5.5
            "prefilter",
            "Rank only identified hazards?",
            status = "danger",
            fill = TRUE
          ),
          conditionalPanel(
            condition = "input.prefilter",
            fileInput("excel_file", "Upload Excel output from MiID DSS"),
            selectInput("excel_sheet", "Hazard Identification Result Tab", choices = c())
          ),
          conditionalPanel(
            condition = "!input.prefilter",
            prettyCheckbox( 
              "ugly_filter",
              "Exclude none and low associated MHs",
              status = "danger",
              fill = TRUE
            )
          )
        ),
        fullColumn(
          tableOutput("prefilter_n")
        )
      )
    )
  ),

  fullSection(  # Criteria -----------------------------------------------------
    menu = "criteria",
    fullSlideImage(  # Food slide ---------------------------------------------------
      # center = TRUE,
      img = "criteria_food.png",
      fullContainer(
        # center = TRUE,
        fullRow(
          fullColumn(
            width = 12,
            h2(tagList(
              actionBttn("help_food_pars", 
                         label = NULL,
                         icon = icon("question"),
                         style = "gradient"
              ),
              "Food parameters")
              )
          )
        ),
        hr(),
        fullRow(
          fullColumn(
            width = 6,
            pickerInput(  # Step 1.1
              "food_item",
              selected = NULL,
              label = "Food item",
              choices = FOOD_CATEGORIES,
              options = list(
                `live-search` = TRUE,
                `actions-box` = TRUE
              )
            )
          ),
          fullColumn(
            width = 6,
            prettyCheckboxGroup(  # Step 2.2
              fill = TRUE,
              status = "danger",
              inputId = "dry_fat",
              label = "Product characteristic",
              choices = c("High fat", 
                          "Low aw (0.5 - 0.9)",
                          "Dry product (aw < 0.5)"
                          ),
            ),
            prettyRadioButtons(
              fill = TRUE,
              inputId = "acidity",
              "Acidity",
              choices = c("pH < 4.5",
                          "4.5 < pH < 4.8",
                          "pH > 10",
                          "Neutral"
                          )
            )
          ) %>%
            add_prompt(
              position = "top", 
              type = "info",
              message = "Although the app assigns default values for the product, you can edit them here",
              rounded = TRUE,
              animate = TRUE,
              bounce = TRUE,
              arrow = FALSE,
              shadow = TRUE
            )
        )
      )
    ),
    fullSlideImage(  # Factory slide ------------------------------------------------
      img = "criteria_factory.png",               
      fullContainer(
        fullRow(
          fullColumn(
            width = 12,
            h2(tagList(
              actionBttn("help_factory_pars", 
                         label = NULL,
                         icon = icon("question"),
                         style = "gradient"
              ),
              "Factory parameters")
              )
          )
        ),
        hr(),
        fullRow(
          fullColumn(
            width = 6,
            prettyRadioButtons(  # Step 2.1
              "process_technique",
              "Inactivation treatment",
              choices = c(
                "No inactivation",
                "Pasteurization",
                "Boiling",
                "Sterilization",
                "HPP < 400 Mpa_long",
                "HPP < 400 - 600 Mpa_long",
                "HPP > 600 Mpa_long",
                "HPP < 400 Mpa_short",
                "HPP < 400 - 600 Mpa_short",
                "HPP > 600 Mpa_short"
              ),
              status = "info",
              fill = TRUE
            )
          ),
          fullColumn(
            width = 6,
            prettyCheckbox("aseptic", "Aseptic processing",
                           status = "success", fill = TRUE,
                           value = TRUE),
            conditionalPanel(condition = '!input.aseptic',
                             prettyCheckboxGroup(
                               "recontaminations",
                               "Possible contamination sources",
                               choices = c(
                                 "Wet environmental contaminants",
                                 "Dry environmental contaminants",
                                 "Addition of dry herbs and spices",
                                 "Addition of dry vitamins",
                                 "Addition of other dry ingredients",
                                 "Human cross-contamination"
                               ),
                               status = "danger",
                               fill = TRUE
                             ),
                             actionBttn("plus_comments", 
                                        label = NULL,
                                        icon = icon("plus"),
                                        style = "gradient"
                                        )
                             )
          #   prettyCheckboxGroup(  # Step 3.3 - 3.4
          #     "process_pars",
          #     "Process parameters",
          #     choices = c(
          #       "Possible environmental contaminants",
          #       "Processing in open bag",
          #       "Addition of unprocessed ingredients"
          #     ),
          #     status = "danger",
          #     fill = TRUE
          #   )
          )
        )
      )
    ),
    fullSlideImage(  # Distribution slide -------------------------------------------
        img = "criteria_distribution.png",               
        fullContainer(
        fullRow(
          fullColumn(
            width = 12,
            h2(tagList(
              actionBttn("help_distribution_pars", 
                         label = NULL,
                         icon = icon("question"),
                         style = "gradient"
              ),
              "Distribution parameters")
              )
          )
        ),
        hr(),
        fullRow(
          fullColumn(
            width = 4,
            fullRow(
              prettyRadioButtons(  # Step 4.3
                "stor_conditions",
                "Conditions during storage/distribution/retailing",
                choices = c(
                  "Room temperature",
                  "Refrigeration (1-4°C)",
                  "Frozen (0°C)"
                ),
                status = "info",
                fill = TRUE
              )
            ),
            fullRow(
              prettyCheckbox(  # Step 4.4
                "temp_abuse",
                "Potential temperature abuse",
                status = "danger",
                fill = TRUE
              )
            )
          ),
          fullColumn(
            width = 4,
            prettyRadioButtons(  # Step 6.6
              "cooking",
              "Meal preparation at household",
              choices = c(
                "Ready to eat product",
                "Cooking >70ºC in the whole product",
                "Cooking <70ºC in some point"
              ),
              status = "info",
              fill = TRUE
            )
          )
        )
      )
    )
    # fullSlide(  # Other slide --------------------------------------------------
    #   fullContainer(
    #     fullRow(
    #       fullColumn(
    #         width = 12,
    #         h2("(4/4) Other parameters")
    #       )
    #     ),
    #     hr(),
    #     fullRow(
    #       fullColumn(
    #         prettyCheckbox(  # Step 5.5
    #           "prefilter",
    #           "Prefilter hazards",
    #           status = "danger",
    #           fill = TRUE
    #         ),
    #         conditionalPanel(
    #           condition = "input.prefilter",
    #           fileInput("excel_file", "Excel file"),
    #           selectInput("excel_sheet", "Sheet name", choices = c())
    #         )
    #       ),
    #       fullColumn(
    #         tableOutput("prefilter_n")
    #       )
    #     )
    #   )
    # )
  ),

  fullSection(  # Ranking ------------------------------------------------------
    menu = "ranking",
    fullSlideImage(
      img = "ranking_page.png",
      fullContainer(
        fullRow(
          fullColumn(
            width = 12,
            prettyCheckboxGroup(
              "methods_used",
              "Method",
              choices = c(
                `Semi-quantitative (product)` = "score_prod",
                `Semi-quantitative (sum)` = "score_sum",
                `Qualitative (product)` = "score_qual_prod",
                `Qualitative (sum)` = "score_qual_sum"
              ),
              selected = c("score_prod", "score_sum", "score_qual_prod", "score_qual_sum"),
              status = "success",
              fill = TRUE,
              inline = TRUE
            )
          )
        ),
        fullRow(
          fullColumn(
            width = 12,
            plotlyOutput("plot_scores")
          )
        )
      )
    ),
    fullSlideImage(
      img = "ranking_page.png",
      fullContainer(
        fullRow(
          fullColumn(
            width = 12,
            reactableOutput("score_table_qual")
          )
        ),
        fullRow(
          fullColumn(
            width = 12,
            align = "right",
            downloadBttn("down_qual_score", 
                       label = NULL,
                       icon = icon("download"),
                       style = "gradient",
                       size = "lg"
                       )
          )
        )
      )
    ),
    fullSlideImage(
      img = "ranking_page.png",
      fullContainer(
        fullRow(
          fullColumn(
            width = 12,
            reactableOutput("score_table_quant")
          )
        ),
        fullRow(
          fullColumn(
            width = 12,
            align = "right",
            downloadBttn("down_quant_score", 
                         label = NULL,
                         icon = icon("download"),
                         style = "gradient",
                         size = "lg"
            )
          )
        )
      )
    )

  ),
  
  fullSection(
    menu = "ranking",
    fullSlideImage(
      img = "ranking_page.png",
      fullContainer(
        fullRow(
          fullColumn(width = 12,
                     tags$h2("Sensitivity of the qualitative score")
                     )
        ),
        fullRow(
          fullColumn(width = 12,
                     prettyCheckboxGroup(
                       fill = TRUE,
                       status = "danger",
                       inputId = "sensi_pars_qual",
                       label = "Omit criteria",
                       choices = c(
                         # "Criteria 1: Hazard Food Pairing" = "score1",
                         "C2: Processing Survival" = "C2",
                         "C3: Recontamination" = "C3",
                         "C4: Post-processing Control" = "C4",
                         "C5: Meal Preparation" = "C5",
                         "C6: Hazard Prevalence" = "C6",
                         "C7: Food Consumption" = "C7",
                         "C8: Hazard Severity" = "C8"
                       ),
                       inline = TRUE
                     )
                     )
        ),
        fullRow(
          fullColumn(width = 12,
                     reactableOutput("sensitivity_table_qual")
                     )
        )
      )
    ),
    fullSlideImage(
      img = "ranking_page.png",
      fullContainer(
        fullRow(
          fullColumn(width = 12,
                     tags$h2("Sensitivity of the quantitative score")
          )
        ),
        fullRow(
          fullColumn(width = 12,
                     prettyCheckboxGroup(
                       fill = TRUE,
                       status = "danger",
                       inputId = "sensi_pars_quant",
                       label = "Omit criteria",
                       choices = c(
                         # "Criteria 1: Hazard Food Pairing" = "score1",
                         "C2: Processing Survival" = "C2",
                         "C3: Recontamination" = "C3",
                         "C4: Post-processing Control" = "C4",
                         "C5: Meal Preparation" = "C5",
                         "C6: Hazard Prevalence" = "C6",
                         "C7: Food Consumption" = "C7",
                         "C8: Hazard Severity" = "C8"
                       ),
                       inline = TRUE
                     )
          )
        ),
        fullRow(
          fullColumn(width = 12,
                     reactableOutput("sensitivity_table_quant")
          )
        )
      )
    )
  ),
  

  
  # fullSectionImage(  # Sensitivity ------------------------------------------------------
  #               menu = "sensitivity",
  #               img = "tilt_orange_down.svg",
                # fullContainer(
                #   fullRow(
                #     fullColumn(
                #       width = 3,
                #       prettyCheckboxGroup(
                #         fill = TRUE,
                #         status = "danger",
                #         inputId = "sensi_pars",
                #         label = "Omit criteria",
                #         choices = c(
                #           # "Criteria 1: Hazard Food Pairing" = "score1",
                #           "Criteria 2: Processing Survival" = "score2",
                #           "Criteria 3: Recontamination" = "score3",
                #           "Criteria 4: Post-processing Control" = "score4",
                #           "Criteria 5: Meal Preparation" = "score5",
                #           "Criteria 6: Hazard Prevalence" = "score6",
                #           "Criteria 7: Food Consumption" = "score7",
                #           "Criteria 8: Hazard Severity" = "score8"
                #           )
                #       )
                #     ),
                #     fullColumn(
                #       width = 9,
                #       reactableOutput("sensitivity_table")
                #     )
                #   )
                # )
  # ),
  
  fullSection(  # About --------------------------------------------------------
                menu = "about",
                fullContainer(
                  fullSlide(
                    fullRow(
                      fullColumn(
                        width = 12,
                        h2(
                          HTML("<strong>About Mira DSS</strong>"),
                          style = "font-size: 85px; color:dark blue; margin-bottom: 40px; font-family:'Fascinate',cursive"
                        ),
                        div(
                          style = "background-color: #f2f2f2; padding: 20px; border-radius: 10px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2);",
                          img(src = "Mira_vertical.png", height = "150px", width = "150px", style = "float: left; margin-right: 20px;font-family:'Fascinate',cursive"),
                          p(
                            "Welcome to Mira, the MIcrobiological hazards risk RAnking Decision Support System (DSS). ",
                            "Mira is an innovative web-based tool designed to rank the risks of microbiological hazards (MHs) ",
                            "associated with infant foods based on the expansion from the groundwork of our hazard identification DSS.",
                            br(), br(),
                            "We present here the comprehensive and systematic framework for the assessment and prioritization",
                            "of MH risks through the established nine risk ranking criteria as reported in the scientific article",
                            "doi: XXX ",
                            br(), br(),
                            
                            style = "font-size: 18px; line-height: 1.5;"
                          )
                        )
                      )
                    )
                  ),
                  
                  fullSlide(
                    fullRow(
                      fullColumn(
                        width = 12,
                        div(
                          img(src = "about.png", height = "100%", width = "100%"),
                          p(HTML("<strong> Perform the Hazard Identification using the Microbiological Hazards IDentification (MiID) DSS </strong>")),
                          p(a(tags$img(src = "favicon.ico", height = "35px", width = "35px"),
                              "Click to access MiID ",
                              href = "https://foodmicrobiologywur.shinyapps.io/Microbial_hazards_ID/"),
                          )
                        )
                      )
                    )
                    
                  ),
                  fullSlide(
                    fullRow(
                      fullColumn(
                        width = 12,
                        div(
                          img(src = "ranking_criteria_2.png", height = "100%", width = "100%"),
                          p(HTML("<strong> Risk Ranking Criteria used in the Mira-DSS </strong> </strong>")),
                          p(a(tags$img(src = "favicon.ico", height = "35px", width = "35px"),
                              "If no hazard identification was done, all 33 microbiological hazards are ranked ",
                              href = "https://foodmicrobiologywur.shinyapps.io/Microbial_hazards_ID/"),
                          )
                        )
                      )
                    )
                    
                  ),
                  
                  fullSlide(
                    fullRow(
                      fullColumn(
                        width = 12,
                        div(
                          img(src = "criteria_description.png", height = "100%", width = "100%"),
                          p(HTML("<strong> all criteria descriptions are described in details in the Mira User manual </strong>")),
                          p(
                            a(
                              tags$img(src = "Mira_vertical.png", height = "35px", width = "35px"),
                              "Acess Mira DSS user manual",
                              href = "https://docs.google.com/document/d/1aNt0sBqNxWZ4aD48LFxdj7WKJycU3R4-I8MCYx4LL0M/edit"
                            ),
                            style = "display: inline; margin-right: 20px;"  # Add inline CSS for spacing
                          ),
                          p(
                            a(
                              "Open Mira Workflow PDF",
                              href = "https://drive.google.com/file/d/1J-AoGRA3fzWscCPN7C8pg_-pIXj2xXe6/view?pli=1",
                              target = "_blank"
                            ),
                            style = "display: inline;"  # Add inline CSS for spacing
                          )
                        )
                      )
                    )
                  
                  ),

                  fullSlide(
                    fullRow(
                      fullColumn(
                        width = 6,
                        div(
                          img(src = "Mira_vertical.png", height = "50%", width = "50%", style = "display: block; margin: 0 auto;")
                        ),
                        p(HTML("<strong><span style='font-size: 24px;'> Disclaimer </span></strong>")),
                        p("This Microbial Hazard IRisk Ranking Tool (Mira-DSS) has been developed for educational purposes 
                          and is provided as a resource to assist users in understanding microbial hazards (MHs) in food products
                          for infants and young children up to the age of 3 through a systematic MHs analysis decision support system (DSS). 
                          The development of this tool was based on research data and established scientific procedures
                          which are reported in the accompanying peer review research paper submitted for publication."),
                        br(),
                        p("The Mira DSS tool is intended solely for educational purposes and should not be considered a 
                          substitute for professional advice or judgment. The creators, developers, and contributors of this tool 
                          make no representations or warranties, express or implied, regarding its accuracy, completeness, or suitability 
                          for any specific purpose. Users are solely responsible for the use of the Tool and the decisions they make based 
                          on the information it provides. The creators, developers, and contributors shall not be held responsible or 
                          liable for any direct, indirect, or consequential damages or consequences arising from the use of the Tool.
                          The creators, developers, and contributors reserve the right to modify, update, or discontinue the Tool at any
                          time without prior notice. By using this Tool, you acknowledge that it is for educational purposes and that you 
                          understand and accept the limitations and disclaimers outlined herein. If you do not agree with any part of this disclaimer, 
                          please refrain from using the Tool.")
                      ),
                      fullColumn(
                        width = 6,
                        p(HTML("<strong> Dr. Alberto Garre </strong>")),
                        p(HTML('<a href="mailto:alberto.garre@upct.es">Contact: alberto.garre@upct.es</a>')),
                        div(
                          img(src = "AG.jpg", height = "150px", width = "100px")
                        ),
                        br(), br(),
                        p(HTML("<strong> Dr. Kah Yen Claire Yeak </strong>")),
                        p(HTML('<a href="mailto:kahyen.yeak@wur.nl">Contact: kahyen.yeak@wur.nl</a>')),
                        div(
                          img(src = "CY.png", height = "150px", width = "100px")
                        ),
                        br(), br(),
                        p(HTML("<strong> Read Full Disclaimer Text </strong>")),
                        a(href = "https://docs.google.com/document/d/1Ekx9KzCLLNnBPYWq_aDeOY0HkABSZ3sXjxmB_mUS2f0/edit", 
                          target = "_blank", 
                          "View")
                      )
                    )
                  )
                )
  )
)