# Required packages - everything else uses package:: found in r/required_packages.R
library(shiny)
library(shinyjs)
library(dplyr)
library(googlesheets4)

#### Start UI ####
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Gulf of Maine Cod and Haddock Recreational Fisheries Decision Support Tool"),
  #### Regulation Selection ####
  tabsetPanel(
    tabPanel( "Regulation Selection",
              strong(div("Future Notes Here", style = "color:blue")), # Warning for users
              #Run Button
              actionButton("runmeplease", "Run Me"),

              fluidRow(
                column(6,
                       titlePanel("Cod"),
                       sliderInput(inputId = "CodFH_seas1", label ="For Hire Open Season 1",
                                   min = as.Date("2023-05-01","%Y-%m-%d"),
                                   max = as.Date("2024-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2023-09-01","%Y-%m-%d"),as.Date("2023-10-31","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "CodFH_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 1)),
                         column(5,
                                sliderInput(inputId = "CodFH_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 22, step = .5))),
                       sliderInput(inputId = "CodPR_seas1", label ="Private Open Season 1",
                                   min = as.Date("2023-05-01","%Y-%m-%d"),
                                   max = as.Date("2024-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2023-09-01","%Y-%m-%d"),as.Date("2023-10-31","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "CodPR_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 1)),
                         column(5,
                                sliderInput(inputId = "CodPR_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 22, step = .5))),

                       actionButton("CODaddSeason", "Add Season"),
                       shinyjs::hidden( div(ID = "CodSeason2",
                                            sliderInput(inputId = "CodFH_seas2", label ="For Hire Open Season 2",
                                                        min = as.Date("2023-05-01","%Y-%m-%d"),
                                                        max = as.Date("2024-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2024-04-30","%Y-%m-%d"),as.Date("2024-04-30","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "CodFH_2_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "CodFH_2_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 10, step = .5))),
                                            sliderInput(inputId = "CodPR_seas2", label ="Private Open Season 2",
                                                        min = as.Date("2023-05-01","%Y-%m-%d"),
                                                        max = as.Date("2024-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2024-04-30","%Y-%m-%d"),as.Date("2024-04-30","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "CodPR_2_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "CodPR_2_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 10, step = .5)))))),
                column(6,
                       titlePanel("Haddock"),
                       sliderInput(inputId = "HadFH_seas1", label ="For Hire Open Season 1",
                                   min = as.Date("2023-05-01","%Y-%m-%d"),
                                   max = as.Date("2024-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2023-05-01","%Y-%m-%d"),as.Date("2024-02-28","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadFH_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadFH_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 18, step = .5))),
                       sliderInput(inputId = "HadPR_seas1", label ="Private Open Season 1",
                                   min = as.Date("2023-05-01","%Y-%m-%d"),
                                   max = as.Date("2024-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2023-05-01","%Y-%m-%d"),as.Date("2024-02-28","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadPR_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 10)),
                         column(5,
                                sliderInput(inputId = "HadPR_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 17, step = .5))),

                       sliderInput(inputId = "HadFH_seas2", label ="For Hire Open Season 2",
                                   min = as.Date("2023-05-01","%Y-%m-%d"),
                                   max = as.Date("2024-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2024-04-01","%Y-%m-%d"),as.Date("2024-04-30","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadFH_2_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadFH_2_len", label = "Min Length",
                                            min = 15, max = 30, value = 18, step = .5))),
                       sliderInput(inputId = "HadPR_seas2", label ="Private Open Season 2",
                                   min = as.Date("2023-05-01","%Y-%m-%d"),
                                   max = as.Date("2024-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2024-04-01","%Y-%m-%d"),as.Date("2024-04-30","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadPR_2_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 10)),
                         column(5,
                                sliderInput(inputId = "HadPR_2_len", label = "Min Length",
                                            min = 15, max = 30, value = 17, step = .5))),

                       actionButton("HADaddSeason", "Add Season"),
                       shinyjs::hidden( div(ID = "HadSeason3",
                                            sliderInput(inputId = "HadFH_seas3", label ="For Hire Open Season 3",
                                                        min = as.Date("2023-05-01","%Y-%m-%d"),
                                                        max = as.Date("2024-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2024-04-30","%Y-%m-%d"),as.Date("2024-04-30","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "HadFH_3_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "HadFH_3_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 10, step = .5))),
                                            sliderInput(inputId = "HadPR_seas3", label ="Private Open Season 3",
                                                        min = as.Date("2023-05-01","%Y-%m-%d"),
                                                        max = as.Date("2024-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2024-04-30","%Y-%m-%d"),as.Date("2024-04-30","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "HadPR_3_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "HadPR_3_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 10, step = .5)))))))),


    #### Results ####
    tabPanel("Results - Aggregated",
             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                              tags$div("Calculating...This will take ~15-20 min per state selected.",id="loadmessage")), #Warning for users

             downloadButton(outputId = "downloadData", "Download"),
             # Add table outputs
             ## KB - Make tables DTs - should fix RMD documentation issue
             tableOutput(outputId = "regtableout"),
             tableOutput(outputId = "catch_tableout"),
             tableOutput(outputId = "welfare_tableout"),
             tableOutput(outputId = "keep_tableout"),
             plotOutput(outputId = "fig")),
     #### By Mode ####
     tabPanel("Results - By Mode",
              tableOutput(outputId = "regtableout"),
              tableOutput(outputId = "catchmode"),
              tableOutput(outputId = "welfaremode"),
              tableOutput(outputId = "keepmode")),

    #### Documentation ####
    tabPanel("Documentation",
             htmlOutput("documentation"))

  ))

####### Start Server ###################
server <- function(input, output, session){

  library(magrittr)


  #### Toggle extra seasons on UI ####
  # Allows for extra seasons to show and hide based on click
  shinyjs::onclick("CODaddSeason",
                   shinyjs::toggle(id = "CodSeason2", anim = TRUE))
  shinyjs::onclick("HADaddSeason",
                   shinyjs::toggle(id = "HadSeason3", anim = TRUE))
  shinyjs::onclick("displaymode",
                   shinyjs::toggle(id = "Predictions_mode", anim = TRUE))


  predictions <- eventReactive(input$runmeplease,{
    source(here::here(paste0("model_run.R")), local = TRUE)

    predictions_out <- read.csv(here::here("sq_predictions.csv")) %>%
      dplyr::mutate(option = c("SQ")) %>%
      dplyr::select(!X) %>%
      rbind(predictions_out10) %>%
      dplyr::mutate(Value = dplyr::case_when(number_weight == "Weight" ~ Value/2205, TRUE ~ Value))
    return(predictions_out)
  })

  #### Regulations ####
  regs_agg <- reactive({

    print("start regs")
    SQ_regulations <- read.csv(here::here("SQ_regulations.csv"))

    Regs<- data.frame(Opt = c("alt"),
                      Var = c("Cod1_FH_bag", "Cod1_FH_size", "Cod1_FH_Season",
                              "Cod1_PR_bag", "Cod1_PR_size", "Cod1_PR_Season",
                              "Had1_FH_bag", "Had1_FH_size", "Had1_FH_Season",
                              "Had1_PR_bag", "Had1_PR_size", "Had1_PR_Season",
                              "Had2_FH_bag", "Had2_FH_size", "Had2_FH_Season",
                              "Had2_PR_bag", "Had2_PR_size", "Had2_PR_Season",
                              "Cod2_FH_bag", "Cod2_FH_size", "Cod2_FH_Season",
                              "Cod2_PR_bag", "Cod2_PR_size", "Cod2_PR_Season",
                              "Had3_FH_bag", "Had3_FH_size", "Had3_FH_Season",
                              "Had3_PR_bag", "Had3_PR_size", "Had3_PR_Season"),
                      Val = c(input$CodFH_1_bag, input$CodFH_1_len, paste0(input$CodFH_seas1[1], " - ", input$CodFH_seas1[2]),
                              input$CodPR_1_bag, input$CodPR_1_len, paste0(input$CodPR_seas1[1], " - ", input$CodPR_seas1[2]),
                              input$HadFH_1_bag, input$HadFH_1_len, paste0(input$HadFH_seas1[1], " - ", input$HadFH_seas1[2]),
                              input$HadPR_1_bag, input$HadPR_1_len, paste0(input$HadPR_seas1[1], " - ", input$HadPR_seas1[2]),
                              input$HadFH_2_bag, input$HadFH_2_len, paste0(input$HadFH_seas2[1], " - ", input$HadFH_seas2[2]),
                              input$HadPR_2_bag, input$HadPR_2_len, paste0(input$HadPR_seas2[1], " - ", input$HadPR_seas2[2]),
                              input$CodFH_2_bag, input$CodFH_2_len, paste0(input$CodFH_seas2[1], " - ", input$CodFH_seas2[2]),
                              input$CodPR_2_bag, input$CodPR_2_len, paste0(input$CodPR_seas2[1], " - ", input$CodPR_seas2[2]),
                              input$HadFH_3_bag, input$HadFH_3_len, paste0(input$HadFH_seas3[1], " - ", input$HadFH_seas3[2]),
                              input$HadPR_3_bag, input$HadPR_3_len, paste0(input$HadPR_seas3[1], " - ", input$HadPR_seas3[2])))

    Regs<- Regs %>% rbind(SQ_regulations)


    Regs_out<- Regs %>%
      tidyr::separate(Var, into =c("Species", "mode", "Var"), sep = "_") %>%
      tidyr::pivot_wider(names_from = Var, values_from = Val) %>%
      dplyr::filter(!bag == 0) %>%
      dplyr::mutate(all_regs = paste0( bag, "_", size, "_", Season),
                    bag_size = paste0( bag, "_", size),
                    bag_season = paste0( bag,"_", Season),
                    size_season = paste0( size, "_", Season)) %>%
      dplyr::group_by(Species,Opt) %>%
      dplyr::distinct(all_regs, .keep_all = TRUE) %>%
      dplyr::mutate(mode = dplyr::case_when(length(Species) == 1 ~ "All", TRUE ~ mode)) %>%
      dplyr::select(!all_regs) %>%
      dplyr::mutate(Species = stringr::str_extract(Species, "[:alpha:]+"))

    return(Regs_out)
    })

  ##### Catch ###########
  catch_agg <- reactive({

    catch_agg<- #predictions() %>%
      predictions_out %>%
      dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                    number_weight == "Weight") %>%
      dplyr::group_by(option, Category, draw_out) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::mutate(under_acl = dplyr::case_when(Category == "cod" & Value <= 99 ~ 1, TRUE ~ 0),
                    under_acl = dplyr::case_when(Category == "had" & Value <= 500 ~ 1, TRUE ~ under_acl)) %>%
      dplyr::group_by(option, Category) %>%
      dplyr::summarise(under_acl = sum(under_acl),
                       Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = c(option), values_from = c(Value, under_acl)) %>%
      dplyr::select(Category, Value_SQ, Value_alt, under_acl_alt) %>%
      dplyr::rename(Species = Category, `SQ Catch Total Mortality (mt)` = Value_SQ,
                    `Alternative Total Mortality (mt)` = Value_alt, `Atlernative % Under ACL` = under_acl_alt)

    return(catch_agg)
  })

  catch_by_mode <- reactive({

    catch_by_mode<- #predictions() %>%
      predictions_out %>%
      dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                    number_weight == "Weight") %>%
      dplyr::group_by(option, Category, draw_out, mode) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::mutate(under_acl = dplyr::case_when(Category == "cod" & Value <= 99 ~ 1, TRUE ~ 0),
                    under_acl = dplyr::case_when(Category == "had" & Value <= 500 ~ 1, TRUE ~ under_acl)) %>%
      dplyr::group_by(option, Category, mode) %>%
      dplyr::summarise(under_acl = sum(under_acl),
                       Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = c(option), values_from = c(Value, under_acl)) %>%
      dplyr::select(Category, Value_SQ, Value_alt, under_acl_alt, mode) %>%
      dplyr::rename(Species = Category, `SQ Catch Total Mortality (mt)` = Value_SQ,
                    `Alternative Total Mortality (mt)` = Value_alt, `Atlernative % Under ACL` = under_acl_alt)

    return(catch_by_mode)
  })

  #### keep release discards ####
  keep_agg <- reactive({

    keep_agg<- predictions() %>%
      #predictions_out %>%
      dplyr::filter(catch_disposition %in% c("keep", "release", "Discmortality")) %>%
      dplyr::group_by(option, Category, catch_disposition, number_weight, draw_out) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::group_by(option, Category, catch_disposition, number_weight) %>%
      dplyr::summarise(Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = c(option, number_weight), values_from = Value) %>%
      dplyr::mutate(perc_diff_num = (alt_Number-SQ_Number)/SQ_Number,
                    perc_diff_wt = (alt_Weight-SQ_Weight)/SQ_Weight) %>%
      dplyr::select(!c(SQ_Number, SQ_Weight)) %>%
      dplyr::select(Category, catch_disposition, alt_Number, perc_diff_num, alt_Weight, perc_diff_wt) %>%
      dplyr::rename(Species = Category, Variable = catch_disposition,
                    `Total fish (N)` = alt_Number, `Percent difference in number of fish` = perc_diff_num,
                    `Total Weight (mt)` = alt_Weight, `Percent difference in weight of fish` = perc_diff_wt)

    return(keep_agg)

    })


  keep_by_mode <- reactive({
    keep_by_mode<- predictions() %>%
      #predictions_out %>%
      dplyr::filter(catch_disposition %in% c("keep", "release", "Discmortality")) %>%
      dplyr::group_by(option, Category, catch_disposition, number_weight, draw_out, mode) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::group_by(option, Category, catch_disposition, number_weight, mode) %>%
      dplyr::summarise(Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = c(option, number_weight), values_from = Value) %>%
      dplyr::mutate(perc_diff_num = (alt_Number-SQ_Number)/SQ_Number,
                    perc_diff_wt = (alt_Weight-SQ_Weight)/SQ_Weight) %>%
      dplyr::select(!c(SQ_Number, SQ_Weight)) %>%
      dplyr::select(Category, catch_disposition, mode, alt_Number, perc_diff_num, alt_Weight, perc_diff_wt) %>%
      dplyr::rename(Species = Category, Variable = catch_disposition,
                    `Total fish (N)` = alt_Number, `Percent difference in number of fish` = perc_diff_num,
                    `Total Weight (mt)` = alt_Weight, `Percent difference in weight of fish` = perc_diff_wt)
    return(keep_by_mode)
  })
#####################

  ##### Ntrips & welfare #######
  welfare_agg <- reactive({

    welfare_agg<- predictions() %>%
      #predictions_out %>%
      dplyr::filter(Category %in% c("CV", "ntrips")) %>%
      dplyr::group_by(option, Category, draw_out) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::group_by(option, Category) %>%
      dplyr::summarise(Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = option, values_from = Value) %>%
      dplyr::mutate(perc_diff = (alt-SQ)/SQ)%>%
      dplyr::select(!SQ) %>%
      dplyr::mutate(Category = dplyr::recode(Category, CV = "Angler Satisfaction ($)",
                                             ntrips = "Angler Trips (N)")) %>%
      dplyr::rename(`Percent difference in median values` = perc_diff)

    return(welfare_agg)

  })


  welfare_by_mode <- reactive({
    welfare_by_mode<- predictions() %>%
      #predictions_out %>%
      dplyr::filter(Category %in% c("CV", "ntrips")) %>%
      dplyr::group_by(option, Category, draw_out, mode) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::group_by(option, Category, mode) %>%
      dplyr::summarise(Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = option, values_from = Value) %>%
      dplyr::mutate(perc_diff = (alt-SQ)/SQ)%>%
      dplyr::select(!SQ) %>%
      dplyr::mutate(Category = dplyr::recode(Category, CV = "Angler Satisfaction ($)",
                                             ntrips = "Angler Trips (N)")) %>%
      dplyr::rename(`Percent difference in median values` = perc_diff)
    return(welfare_by_mode)
  })

  ###Output Tables
  output$regtableout <- renderTable({
    regs_agg()
  })

  output$catchtableout <- renderTable({
    catch_agg()
  })

  output$keep_tableout<- renderTable({
    keep_agg()
  })

  output$welfare_tableout<- renderTable({
    welfare_agg()
  })

  ### Tables for by mode tab
  output$regmode <- renderTable({
    regs_by_mode()
  })

  output$catchmode <- renderTable({
    catch_by_mode()
  })

  output$keepmode<- renderTable({
    keep_by_mode()
  })

  output$welfaremode<- renderTable({
    welfare_by_mode()
  })

  observeEvent(input$runmeplease, {
    dat<- predictions()
    readr::write_csv(dat, file = here::here(paste0("output/output_", format(Sys.time(), "%Y%m%d_%H%M%S_"),  ".csv")))
    })

  output$downloadData <- downloadHandler(
    filename = function(){"RecDSToutput.xlsx"},
    content = function(filename) {
      df_list <- list(Regulations=regulations(), Catch_Mortality_aggregated = catch_agg(), Catch_Mortality_by_mode = catch_by_mode(),
                      Keep_Release_aggregated = keep_agg(), Keep_Release_by_mode = keep_by_mode(),
                      Satisfaction_trips_aggregated = welfare_agg(), Satisfaction_trips_by_mode = welfare_mode())
      openxlsx::write.xlsx(x = df_list , file = filename, row.names = FALSE)
    })

}
shiny::shinyApp(ui = ui, server = server)
