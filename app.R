

library(shiny)
library(shinyjs)

#### Start UI ####
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Western Gulf of Maine Cod and Haddock Recreational Fisheries Decision Support Tool"),
  #### Regulation Selection ####
  tabsetPanel(
    tabPanel("Cod and Haddock Model Summary",
             p("This page summarizes models results for sets of policies that have been run to date. These are
               intended as a jumping off point for your own model runs."),
             p("Hover over each point to view the detailed statistics for each model run. The first section contains
               a table of recreational management measures. The second section contains graphs of mortality. The third
               section has graphs of other performance measures, including relative change in Angler Satisfaction, Trips, and Discards."),

             shinyjs::useShinyjs(),
             shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
             actionButton("updatedat", "Update"),

             plotly::plotlyOutput(outputId = "totCatch"),

             DT::DTOutput(outputId = "DTout"),



             shinyWidgets::awesomeCheckboxGroup(
               inputId = "fig",
               label = "Supplemental Figures",
               choices = c( "Angler Satisfaction","Discards", "Trips"),
               inline = TRUE,
               status = "danger"),
             uiOutput("summary_regs_table"),
             uiOutput("addCVCod"),
             uiOutput("addCVHad"),
             uiOutput("addReleaseCod"),
             uiOutput("addReleaseHad"),
             uiOutput("addTripsCod"),
             uiOutput("addTripsHad")),





    tabPanel( "Regulation Selection",
              strong(div("Use this page to set up the regulations that you would like to simulate.  Only click the button ONCE before clicking over to Results tab. We have pre-loaded the status quo regulations.", style = "color:blue")), # Warning for users
              #Run Button
              actionButton("runmeplease", "Run Me"),
              textInput("Run_Name", "Please name this using your initials and the number of the run (ex. AB1)."),

              fluidRow(
                column(6,
                       titlePanel("Cod"),
                       sliderInput(inputId = "CodFH_seas1", label ="For Hire Open Season 1",
                                   min = as.Date("2025-05-01","%Y-%m-%d"),
                                   max = as.Date("2026-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2025-09-01","%Y-%m-%d"),as.Date("2025-10-31","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "CodFH_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 1)),
                         column(5,
                                sliderInput(inputId = "CodFH_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 23, step = 1))),
                       sliderInput(inputId = "CodPR_seas1", label ="Private Open Season 1",
                                   min = as.Date("2025-05-01","%Y-%m-%d"),
                                   max = as.Date("2026-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2025-09-01","%Y-%m-%d"),as.Date("2025-10-31","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "CodPR_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 1)),
                         column(5,
                                sliderInput(inputId = "CodPR_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 23, step = 1))),

                       actionButton("CODaddSeason", "Add Season"),
                       shinyjs::hidden( div(ID = "CodSeason2",
                                            sliderInput(inputId = "CodFH_seas2", label ="For Hire Open Season 2",
                                                        min = as.Date("2025-05-01","%Y-%m-%d"),
                                                        max = as.Date("2026-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2026-01-01","%Y-%m-%d"),as.Date("2026-01-01","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "CodFH_2_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "CodFH_2_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 23, step = 1))),
                                            sliderInput(inputId = "CodPR_seas2", label ="Private Open Season 2",
                                                        min = as.Date("2025-05-01","%Y-%m-%d"),
                                                        max = as.Date("2026-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2026-01-01","%Y-%m-%d"),as.Date("2026-01-01","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "CodPR_2_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "CodPR_2_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 23, step = 1)))))),
                column(6,
                       titlePanel("Haddock"),
                       sliderInput(inputId = "HadFH_seas1", label ="For Hire Open Season 1",
                                   min = as.Date("2025-05-01","%Y-%m-%d"),
                                   max = as.Date("2026-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2025-05-01","%Y-%m-%d"),as.Date("2026-02-28","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadFH_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadFH_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 18, step = 1))),
                       sliderInput(inputId = "HadPR_seas1", label ="Private Open Season 1",
                                   min = as.Date("2025-05-01","%Y-%m-%d"),
                                   max = as.Date("2026-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2025-05-01","%Y-%m-%d"),as.Date("2026-02-28","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadPR_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadPR_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 18, step = 1))),

                       sliderInput(inputId = "HadFH_seas2", label ="For Hire Open Season 2",
                                   min = as.Date("2025-05-01","%Y-%m-%d"),
                                   max = as.Date("2026-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2026-04-01","%Y-%m-%d"),as.Date("2026-04-30","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadFH_2_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadFH_2_len", label = "Min Length",
                                            min = 15, max = 30, value = 18, step = 1))),
                       sliderInput(inputId = "HadPR_seas2", label ="Private Open Season 2",
                                   min = as.Date("2025-05-01","%Y-%m-%d"),
                                   max = as.Date("2026-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2026-04-01","%Y-%m-%d"),as.Date("2026-04-30","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadPR_2_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadPR_2_len", label = "Min Length",
                                            min = 15, max = 30, value = 18, step = 1))),

                       actionButton("HADaddSeason", "Add Season"),
                       shinyjs::hidden( div(ID = "HadSeason3",
                                            sliderInput(inputId = "HadFH_seas3", label ="For Hire Open Season 3",
                                                        min = as.Date("2025-05-01","%Y-%m-%d"),
                                                        max = as.Date("2026-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2026-01-01","%Y-%m-%d"),as.Date("2026-01-01","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "HadFH_3_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "HadFH_3_len", label ="Min Length",
                                                                 min = 15, max = 30, value = 18, step = 1))),
                                            sliderInput(inputId = "HadPR_seas3", label ="Private Open Season 3",
                                                        min = as.Date("2025-05-01","%Y-%m-%d"),
                                                        max = as.Date("2026-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2026-01-01","%Y-%m-%d"),as.Date("2026-01-01","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "HadPR_3_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "HadPR_3_len", label ="Min Length",
                                                                 min = 15, max = 30, value = 18, step = 1))))))))#,

  ))

####### Start Server ###################
server <- function(input, output, session){

  library(magrittr)
  library(ggplot2)
  #library(webshot)

  observeEvent(input$updatedat,{
    print("updating")
    shinyjs::js$refresh_page();
  })

  df2 <- function(){
    fnames <- list.files(path=here::here("output/"),pattern = "*.csv",full.names = T)

    fnames2<- as.data.frame(fnames) %>%
      tidyr::separate(fnames, into = c("a", "b"), sep = "_") %>%
      dplyr::mutate(b = ifelse(stringr::str_detect(b, "202501"),  "NA", b),
                    c=c(1:nrow(.)),
                    run_name = dplyr::case_when(b != "NA" ~ b, TRUE ~ as.character(c))) %>%
      dplyr::select(run_name)

    # fnames2<- as.data.frame(fnames) %>%
    #   tidyr::separate(fnames, into = c("a", "b", "c"), sep = "_") %>%
    #   dplyr::mutate(c = ifelse(stringr::str_detect(c, "202501"),  "NA", c),
    #                 d=c(1:nrow(.)),
    #                 run_name = dplyr::case_when(c != "NA" ~ c, TRUE ~ as.character(c))) %>%
    #   dplyr::select(run_name)

    df <- fnames %>%
      purrr::map_df(~data.table::fread(.,stringsAsFactors=F,check.names=T,strip.white=T))


    df2<- df %>% dplyr::mutate(run_number = as.character(rep(fnames2$run_name, each = 6030)))
    return(df2)
  }

  cod_acl <- function(){
    cod_acl = 118
    return(cod_acl)
  }

  had_acl <- function(){
    had_acl = 1146
    return(had_acl)
  }

  lb_to_mt <- function(){
    lb_to_mt = 0.000454
    return(lb_to_mt)
  }

  Run_Name <- function(){
    if(stringr::str_detect(input$Run_Name, "_")){
      Run_Name <-  gsub("_", "-", input$Run_Name)
    }else {
      Run_Name <- input$Run_Name
    }
    print(Run_Name)
    return(Run_Name)
  }

  regs<- function(){
    flist <- list.files(path = here::here("saved_regs/"), pattern = "\\.csv$", full.names = TRUE)

    regs_data <- flist %>%
      purrr::map_dfr(readr::read_csv)
  }

  output$DTout <- DT::renderDT({
    catch_agg<- df2() %>%
      #dat %>%
      dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                    number_weight == "Weight") %>%
      dplyr::group_by(run_number, Category,draw_out) %>%
      dplyr::summarise(Value = sum(as.numeric(Value))) %>%
      dplyr::mutate(Value = Value * lb_to_mt()) %>%
      dplyr::mutate(under_acl = dplyr::case_when(Category == "cod" & Value <= cod_acl() ~ 1, TRUE ~ 0),
                    under_acl = dplyr::case_when(Category == "had" & Value <= had_acl() ~ 1, TRUE ~ under_acl)) %>%
      dplyr::group_by(run_number, Category) %>%
      dplyr::summarise(under_acl = sum(under_acl),
                       Value = round(median(Value),0)) %>%
      tidyr::pivot_wider(names_from = Category, values_from = c(Value, under_acl))

    SQ_regulations <- read.csv(here::here("data-raw/SQ_regulations.csv")) %>%
      dplyr::rename(Category = Var,
                    SQ = Val)

    df3<- df2() %>% dplyr::filter(!Category %in% c("CV", "ntrips", "nchoiceoccasions","cod" , "had")) %>%
      dplyr::select(Category, Value, run_number) %>%
      dplyr::left_join(SQ_regulations, by = c("Category"))


    Regs_out <- df3 %>%
      dplyr::left_join(catch_agg, by = c("run_number")) %>%
      dplyr::select(!SQ) %>%
      dplyr::select(!Opt) %>%
      tidyr::separate(Category, into =c("Species", "mode", "Var"), sep = "_") %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = Var, values_from = c(Value)) %>%
      dplyr::mutate(Season = dplyr::case_when(bag == 0 ~"NA", TRUE ~ Season),
                    size = dplyr::case_when(bag == 0 ~"NA", TRUE ~ size),
                    # Diff_from_SQ_bag = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Diff_from_SQ_bag),
                    # Diff_from_SQ_size = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Diff_from_SQ_size),
                    # Diff_from_SQ_Season = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Diff_from_SQ_Season),
                    bag = dplyr::case_when(bag == 0 ~"NA", TRUE ~ bag)) %>%
      dplyr::ungroup() %>%
      #dplyr::mutate(Diff_from_SQ = paste0(Diff_from_SQ_bag,Diff_from_SQ_size,Diff_from_SQ_Season)) %>%
      #dplyr::select(!c(Diff_from_SQ_bag,Diff_from_SQ_size,Diff_from_SQ_Season)) %>%
      tidyr::pivot_wider(names_from = Species, values_from = c(bag, size, Season)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cod_bag = paste0(bag_Cod1, " , ", bag_Cod2),
                    cod_size = paste0(size_Cod1, " , ", size_Cod2),
                    cod_season = paste0(Season_Cod1, " , ", Season_Cod2),
                    had_bag = paste0(bag_Had1, " , ", bag_Had2, " , ", bag_Had3),
                    had_size = paste0(size_Had1, " , ", size_Had2, " , ", size_Had3),
                    had_season = paste0(Season_Had1, " , ", Season_Had2, " , ", Season_Had3),
                    cod_bag = stringr::str_remove(cod_bag, " , NA"),
                    cod_size = stringr::str_remove(cod_size, " , NA"),
                    cod_season = stringr::str_remove(cod_season, " , NA"),
                    had_bag = stringr::str_remove(had_bag, " , NA"),
                    had_size = stringr::str_remove(had_size, " , NA"),
                    had_season = stringr::str_remove(had_season, " , NA"),
                    cod_bag = stringr::str_remove(cod_bag, "NA ,"),
                    cod_size = stringr::str_remove(cod_size, "NA ,"),
                    cod_season = stringr::str_remove(cod_season, "NA ,"),
                    had_bag = stringr::str_remove(had_bag, "NA ,"),
                    had_size = stringr::str_remove(had_size, "NA ,"),
                    had_season = stringr::str_remove(had_season, "NA ,")) %>%
                    # Diff_from_SQ_cod = paste0(Diff_from_SQ_Cod1, " , ", Diff_from_SQ_Cod2),
                    # Diff_from_SQ_had = paste0(Diff_from_SQ_Had1, " , ", Diff_from_SQ_Had2, " , ", Diff_from_SQ_Had3),
                    # Diff_from_SQ_cod = stringr::str_remove(Diff_from_SQ_cod, " , NA"),
                    # Diff_from_SQ_cod = stringr::str_remove(Diff_from_SQ_cod, "NANA"),
                    # Diff_from_SQ_cod = stringr::str_remove(Diff_from_SQ_cod, "NA ,"),
                    # Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, "NA ,"),
                    # Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, " , NA"),
                    # Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, " , NANA"),
                    # Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, "NANA")) %>%
      dplyr::select(mode, run_number, cod_bag, cod_size, cod_season, had_bag, had_size, had_season, under_acl_cod, under_acl_had) %>%
      dplyr::mutate(cod_season = stringr::str_remove_all(cod_season, "202.-"),
                    had_season = stringr::str_remove_all(had_season, "202.-")) %>%
      dplyr::mutate(mode = dplyr::recode(mode, "FH" = "For Hire",
                    "PR" = "Private")) %>%
      dplyr::select(run_number, mode, cod_bag, cod_size, cod_season,
                    had_bag, had_size, had_season, under_acl_cod, under_acl_had) %>%
      dplyr::rename(Mode = mode,
                    `Run Identifier` = run_number,
                    `Cod Bag Limit` = cod_bag,
                    `Cod Minimum Size (in)` = cod_size,
                    `Cod Season(s)` = cod_season,
                    `Haddock Bag Limit` = had_bag,
                    `Haddock Minimum Size (in)` = had_size,
                    `Haddock Season(s)` = had_season,
                    `% under Cod ACL` = under_acl_cod,
                    `% under Haddock ACL` = under_acl_had)


    DT::datatable(Regs_out)
  })

  output$summary_regs_table <- DT::renderDT({
    Regs_out <-regs() %>%
      tidyr::separate(input, into = c("species", "season", "measure"), sep = "_") %>%
      dplyr::mutate(season = stringr::str_remove(season, "^seas")) %>%
      tidyr::extract(species, into = c("species", "state2", "mode"), regex =  "([^a-z]+)([a-z]+)(.*)") %>%
      dplyr::select(-state2) %>%
      dplyr::group_by(run_name, state, species, mode, season) %>%
      tidyr::pivot_wider(names_from = measure, values_from = value) %>%
      dplyr::filter(!bag == 0) %>%
      dplyr::mutate(season2 = paste0(op, " - ", cl)) %>%
      dplyr::group_by(run_name, state, species, mode) %>%
      dplyr::summarise(
        bag = paste(bag, collapse = ","),
        len = paste(len, collapse = ","),
        season = paste(season2, collapse = ","),
        .groups = "drop" ) %>%
      dplyr::mutate(mode = if_else(mode == "", "All modes", mode)) %>%
      dplyr::mutate(season = gsub("2025-", "", season))

  })

  output$totCatch <- plotly::renderPlotly({

    # sq<- read.csv("predictions_sq_no_august.csv") %>%
    #   dplyr::mutate(run_number = "SQ")
    #
    # aug<- read.csv("predictions_with_open_august.csv") %>%
    #   dplyr::mutate(run_number = "aug")
    #
    # all<-read.csv("predictions_open_all.csv") %>%
    #   dplyr::mutate(run_number = "all")
    #
    # dat<- sq %>% rbind(aug, all)

    catch_agg<- df2() %>%
      #dat %>%
      dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                    number_weight == "Weight") %>%
      dplyr::group_by(run_number, Category,draw_out) %>%
      dplyr::summarise(Value = sum(as.numeric(Value))) %>%
      dplyr::mutate(Value = Value * lb_to_mt()) %>%
      dplyr::mutate(under_acl = dplyr::case_when(Category == "cod" & Value <= cod_acl() ~ 1, TRUE ~ 0),
                    under_acl = dplyr::case_when(Category == "had" & Value <= had_acl() ~ 1, TRUE ~ under_acl)) %>%
      dplyr::group_by(run_number, Category) %>%
      dplyr::summarise(under_acl = sum(under_acl),
                       Value = round(median(Value),0)) %>%
      tidyr::pivot_wider(names_from = Category, values_from = c(Value, under_acl))

    catch_agg2<- catch_agg %>%
      dplyr::mutate(under_acl_cod2 = dplyr::case_when(under_acl_cod < 50 ~ "Less than 50%", TRUE ~ ""),
                    under_acl_cod2 = dplyr::case_when(under_acl_cod >= 50 & under_acl_cod < 60 ~ "50-59%", TRUE ~ under_acl_cod2),
                    under_acl_cod2 = dplyr::case_when(under_acl_cod >= 60 & under_acl_cod < 70~ "60-69%", TRUE ~ under_acl_cod2),
                    under_acl_cod2 = dplyr::case_when(under_acl_cod >= 70 & under_acl_cod < 80 ~ "70-79%", TRUE ~ under_acl_cod2),
                    under_acl_cod2 = dplyr::case_when(under_acl_cod >= 80 & under_acl_cod < 90 ~ "80-89%", TRUE ~ under_acl_cod2),
                    under_acl_cod2 = dplyr::case_when(under_acl_cod >= 90 & under_acl_cod <=100 ~ "90-100%", TRUE ~ under_acl_cod2)) %>%
      dplyr::mutate(under_acl_had2 = dplyr::case_when(under_acl_had < 50 ~ "Less than 50%", TRUE ~ ""),
                    under_acl_had2 = dplyr::case_when(under_acl_had >= 50 & under_acl_had < 60 ~ "50-59%", TRUE ~ under_acl_had2),
                    under_acl_had2 = dplyr::case_when(under_acl_had >= 60 & under_acl_had < 70~ "60-69%", TRUE ~ under_acl_had2),
                    under_acl_had2 = dplyr::case_when(under_acl_had >= 70 & under_acl_had < 80 ~ "70-79%", TRUE ~ under_acl_had2),
                    under_acl_had2 = dplyr::case_when(under_acl_had >= 80 & under_acl_had < 90 ~ "80-89%", TRUE ~ under_acl_had2),
                    under_acl_had2 = dplyr::case_when(under_acl_had >= 90 & under_acl_had <=100 ~ "90-100%", TRUE ~ under_acl_had2)) %>%
      dplyr::rename(`Cod Mortality`=Value_cod) %>%
      dplyr::rename(`Haddock Mortality`=Value_had) %>%
      dplyr::ungroup()

    # catch_agg <- data.frame(run_number = c("SQ","what", "test"),
    #                         Cod_Mortality = c(43, 60, 130),
    #                         Haddock_Mortality = c(810, 955, 1099),
    #                         under_acl_cod2 = c("90-100%","60-69%", "Less than 50%"),
    #                         under_acl_had2 = c("90-100%","60-69%", "Less than 50%")) %>%
    #   dplyr::rename(`Cod Mortality`=Cod_Mortality, `Haddock Mortality`=Haddock_Mortality)

    # my_palette <- c("red3","red3","red3","red3","red3","#C5E8B7",
    #                           "#ABE098", "#83D475","green4","darkgreen")

    p<- catch_agg2 %>%
      #dplyr::mutate(under_acl_cod = as.numeric(under_acl_cod)) %>%
      ggplot2::ggplot(ggplot2::aes(x = `Cod Mortality`, y = `Haddock Mortality`))+
      ggplot2::geom_point(ggplot2::aes(colour = under_acl_cod2, size = under_acl_had2)) +
      ggplot2::scale_color_manual(values = c("50-59%" = "#A9DFBF", "60-69%" = "#7DCEA0",
                                             "70-79%" = "#52BE80","80-89%" = "#27AE60",
                                             "90-100%" = "#1B5E20", "Less than 50%" = "red3"))+
      ggplot2::scale_size_manual(values = c("50-59%" = 1, "60-69%" = 1,
                                             "70-79%" = 1,"80-89%" = 1,
                                             "90-100%" = 1, "Less than 50%" = 1))+
      ggplot2::labs(colour="% of simulations under cod ACL",
                    size="% of simulations under haddock ACL")+
      # ggplot2::scale_colour_stepsn(limits = c(0,100), n.breaks = 10,
      #                             colors =  c("red3","red3","red3","red3","red3","#C5E8B7",
      #                                         "#ABE098", "#83D475","green4","darkgreen"),
      #                             name = "% Under Cod ACL")+
      ggplot2::geom_text(ggplot2::aes(label = run_number), check_overlap = TRUE)+
      ggplot2::geom_vline( xintercept =cod_acl(), linetype="dashed")+
      ggplot2::geom_hline( yintercept =had_acl(), color="grey45")+
      ggplot2::annotate(geom="text", x=cod_acl(), label="Cod ACL", y=1200) +
      ggplot2::annotate(geom="text", y=had_acl(), label="Had ACL", x=80) +
      ggplot2::guides(size = "none")+
      ggplot2::ggtitle("Cod and Haddock Mortality")+
      ggplot2::ylab("Median Recreational Haddock Mortality (mt)")+
      ggplot2::xlab("Median Recreational Cod Mortality (mt)")

    fig<- plotly::ggplotly(p) %>% #,
                           #tooltip = c("x", "y", "colour")) %>%
      plotly::style(textposition = "top center")
    fig
  })

  output$addCVCod <- renderUI({

    if(any("Angler Satisfaction" == input$fig)){

      plotly::renderPlotly({

        SQ<-read.csv(here::here("data-raw/sq_predictions_cm.csv")) %>%
          dplyr::filter(Category == "CV") %>%
          dplyr::group_by(draw_out, Category) %>%
          dplyr::summarise(Value_SQ = sum(Value))

        welfare <-  df2() %>%
          dplyr::filter(Category == c("CV")) %>%
          dplyr::group_by(run_number,  draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::left_join(SQ) %>%
          dplyr::mutate(Value_diff = Value_SQ-Value) %>%
          dplyr::summarise(median_cv = median(Value_diff, na.rm = TRUE))

        catch<- df2() %>%
          dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(run_number, option, Category) %>%
          dplyr::summarise(Value =round(median(Value),0)) %>%
          tidyr::pivot_wider(names_from = Category, values_from = Value) %>%
          dplyr::left_join(welfare) %>%
          dplyr::rename(`Angler Satisfaction($)` = median_cv)

        p1<- catch %>% ggplot2::ggplot(ggplot2::aes(x = `Angler Satisfaction($)`, y = cod))+
          ggplot2::geom_point() +
          ggplot2::geom_hline( yintercept =cod_acl())+
          ggplot2::geom_text(ggplot2::aes(label=run_number), check_overlap = TRUE)+
          ggplot2::geom_text(ggplot2::aes(y=cod_acl(), label="Cod ACL", x=1000000)) +
          ggplot2::xlab("Relative Change in Angler Satisfaction ($)")+
          ggplot2::ylab("Total Recreational Cod Mortality (mt)")+
          ggplot2::labs(title = "Cod Mortality (mt) compared to Angler Satisfaction (Compared to status-quo regulations, how much better- or worse-off are anglers, in dollars?)",
                        subtitle = "testing")+
          ggplot2::theme(legend.position = "none")

        fig1<- plotly::ggplotly(p1) %>%
          # graphics::layout(title = list(text = paste0('Cod Mortality (mt) compared to Angler Satisfaction',
          #                                   '<br>',
          #                                   '<sup>',
          #                                   'More descirptuon of CV','</sup>'))) %>%
          plotly::style(textposition = "top center")

        fig1
      })

    }
  })


  output$addCVHad <- renderUI({

    if(any("Angler Satisfaction" == input$fig)){

      plotly::renderPlotly({
        SQ<-read.csv(here::here("data-raw/sq_predictions_cm.csv")) %>%
          dplyr::filter(Category == "CV") %>%
          dplyr::group_by(draw_out, Category) %>%
          dplyr::summarise(Value_SQ = sum(Value))

        welfare <-  df2() %>%
          dplyr::filter(Category == c("CV")) %>%
          dplyr::group_by(run_number,  draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::left_join(SQ) %>%
          dplyr::mutate(Value_diff = Value_SQ-Value) %>%
          dplyr::summarise(median_cv = median(Value_diff, na.rm = TRUE))

        catch<- df2() %>%
          dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(run_number, option, Category) %>%
          dplyr::summarise(Value =round(median(Value),0)) %>%
          tidyr::pivot_wider(names_from = Category, values_from = Value) %>%
          dplyr::left_join(welfare) %>%
          dplyr::rename(`Angler Satisfaction($)` = median_cv)

        p2<- catch %>% ggplot2::ggplot(ggplot2::aes(x = `Angler Satisfaction($)`, y = had))+
          ggplot2::geom_point() +
          ggplot2::geom_hline( yintercept =had_acl())+
          ggplot2::geom_text(ggplot2::aes(label=run_number), check_overlap = TRUE)+
          ggplot2::xlab("Relative Change in Angler Satisfaction ($)")+
          ggplot2::ylab("Total Recreational Haddock Mortality (mt)")+
          ggplot2::geom_text(ggplot2::aes(x=had_acl(), label="Had ACL", y=1075)) +
          ggplot2::labs(title = "Haddock Mortality (mt) compared to Angler Satisfaction (Compared to status-quo regulations, how much better- or worse-off are anglers, in dollars?)",
                        subtitle = "testing")+
          ggplot2::theme(legend.position = "none")

        fig2<- plotly::ggplotly(p2) %>%
          # graphics::layout(title = list(text = paste0('Haddock Mortality (mt) compared to Angler Satisfaction',
          #                                   '<br>',
          #                                   '<sup>',
          #                                   'More descirptuon of CV','</sup>'))) %>%
          plotly::style(textposition = "top center")
        fig2
      })

    }
  })

  output$addReleaseCod <- renderUI({

    if(any("Discards" == input$fig)){

      plotly::renderPlotly({
        release <-  df2() %>%
          dplyr::filter(catch_disposition %in% c("Discmortality"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(run_number,option, Category) %>%
          dplyr::summarise(Discmortality = round(median(Value),0))


        catch<- df2() %>%
          dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(run_number, option, Category) %>%
          dplyr::summarise(Value =round(median(Value),0)) %>%
          dplyr::left_join(release) %>%
          tidyr::pivot_wider(names_from = Category, values_from = c(Value, Discmortality))%>%
          dplyr::rename(`Cod Mortality`=Value_cod) %>%
          dplyr::rename(`Haddock Mortality`=Value_had)%>%
          dplyr::rename(`Cod Discard Mortality`=Discmortality_cod) %>%
          dplyr::rename(`Haddock Discard Mortality`=Discmortality_had)

        p3<- catch %>% ggplot2::ggplot(ggplot2::aes(x = `Cod Discard Mortality`, y = `Cod Mortality`))+
          ggplot2::geom_point() +
          ggplot2::geom_hline( yintercept =cod_acl())+
          ggplot2::geom_text(ggplot2::aes(label=run_number), check_overlap = TRUE)+
          ggplot2::geom_text(ggplot2::aes(y=cod_acl(), label="Cod ACL", x=240)) +
          ggplot2::xlab("Cod Discard Mortality (mt)")+
          ggplot2::ylab("Total Recreational Cod Mortality (mt)")+
          ggplot2::labs(title = "Total Cod Mortality (mt) compared to Discard Mortality(mt)",
                        subtitle = "testing")+
          ggplot2::theme(legend.position = "none")

        fig3<- plotly::ggplotly(p3)%>%
          # graphics::layout(title = list(text = paste0('Cod Mortality (mt) compared to Cod Releases (mt)'))) %>%
          plotly::style(textposition = "top center")
        fig3
      })
    }
  })

      output$addReleaseHad <- renderUI({
        if(any("Discards" == input$fig)){


          plotly::renderPlotly({
            release <-  df2() %>%
              dplyr::filter(catch_disposition %in% c("Discmortality"),
                            number_weight == "Weight") %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::mutate(Value = Value * lb_to_mt()) %>%
              dplyr::group_by(run_number,option, Category) %>%
              dplyr::summarise(Discmortality = round(median(Value),0))


            catch<- df2() %>%
              dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                            number_weight == "Weight") %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::mutate(Value = Value * lb_to_mt()) %>%
              dplyr::group_by(run_number, option, Category) %>%
              dplyr::summarise(Value = round(median(Value),0)) %>%
              dplyr::left_join(release) %>%
              tidyr::pivot_wider(names_from = Category, values_from = c(Value, Discmortality))%>%
              dplyr::rename(`Cod Mortality`=Value_cod) %>%
              dplyr::rename(`Haddock Mortality`=Value_had)%>%
              dplyr::rename(`Cod Discard Mortality`=Discmortality_cod) %>%
              dplyr::rename(`Haddock Discard Mortality`=Discmortality_had)


            p4<- catch %>% ggplot2::ggplot(ggplot2::aes(x = `Haddock Discard Mortality` , y = `Haddock Mortality`))+
              ggplot2::geom_point() +
              ggplot2::geom_hline( yintercept = had_acl())+
              ggplot2::geom_text(ggplot2::aes(label=run_number), check_overlap = TRUE)+
              ggplot2::geom_text(ggplot2::aes(y=had_acl(), label="Had ACL", x=600)) +
              ggplot2::xlab("Haddock Discard Mortality (mt)")+
              ggplot2::ylab("Total Recreational Haddock Mortality (mt)")+
              ggplot2::labs(title = "Total Haddock Mortality (mt) compared to Discard Mortality (mt)",
                            subtitle = "testing")+
              ggplot2::theme(legend.position = "none")

            fig4<- plotly::ggplotly(p4)%>%
              # graphics::layout(title = list(text = paste0('Haddock Mortality (mt) compared to Haddock Releases (mt)'))) %>%
              plotly::style(textposition = "top center")
            fig4

    })
    }
  })


      output$addTripsCod <- renderUI({
        if(any("Trips" == input$fig)){

          plotly::renderPlotly({
            trips <-  df2() %>%
              dplyr::filter(Category %in% c("ntrips")) %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::group_by(run_number,option, Category) %>%
              dplyr::summarise(Trips = round(median(Value),0))


            catch<- df2() %>%
              dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                            number_weight == "Weight") %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::mutate(Value = Value * lb_to_mt()) %>%
              dplyr::group_by(run_number, option, Category) %>%
              dplyr::summarise(Value = round(median(Value),0)) %>%
              tidyr::pivot_wider(names_from = Category, values_from = Value) %>%
              dplyr::left_join(trips) %>%
              dplyr::select(!Category)%>%
              dplyr::rename(`Cod Mortality`=cod) %>%
              dplyr::rename(`Haddock Mortality`=had)


            p5<- catch %>% ggplot2::ggplot(ggplot2::aes(x = Trips, y = `Cod Mortality`))+
              ggplot2::geom_point() +
              ggplot2::geom_hline( yintercept = cod_acl())+
              ggplot2::geom_text(ggplot2::aes(label=run_number), check_overlap = TRUE)+
              ggplot2::geom_text(ggplot2::aes(y=cod_acl(), label="Cod ACL", x=167000), angle=90) +
              ggplot2::xlab("Number of Trips")+
              ggplot2::ylab("Total Recreational Cod Mortality (mt)")+
              ggplot2::labs(title = "Cod Mortality (mt) compared to Total Number of Trips",
                            subtitle = "testing")+
              ggplot2::theme(legend.position = "none")

            fig5<- plotly::ggplotly(p5)%>%
              #graphics::layout(title = list(text = paste0('Cod Mortality (mt) compared to Total Number of Trips'))) %>%
              plotly::style(textposition = "top center")
            fig5

          })
        }
      })


      output$addTripsHad <- renderUI({
        if(any("Trips" == input$fig)){

          plotly::renderPlotly({
            trips <-  df2() %>%
              dplyr::filter(Category %in% c("ntrips")) %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::group_by(run_number,option, Category) %>%
              dplyr::summarise(Trips = round(median(Value),0))


            catch<- df2() %>%
              dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                            number_weight == "Weight") %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::mutate(Value = Value * lb_to_mt()) %>%
              dplyr::group_by(run_number, option, Category) %>%
              dplyr::summarise(Value = round(median(Value),0)) %>%
              tidyr::pivot_wider(names_from = Category, values_from = Value) %>%
              dplyr::left_join(trips) %>%
              dplyr::select(!Category)%>%
              dplyr::rename(`Cod Mortality`=cod) %>%
              dplyr::rename(`Haddock Mortality`=had)


            p6<- catch %>% ggplot2::ggplot(ggplot2::aes(x =Trips , y = `Haddock Mortality`))+
              ggplot2::geom_point() +
              ggplot2::geom_hline( yintercept = had_acl())+
              ggplot2::geom_text(ggplot2::aes(label=run_number), check_overlap = TRUE)+
              ggplot2::geom_text(ggplot2::aes(y=had_acl(), label="Had ACL", x=167000)) +
              ggplot2::xlab("Number of Trips")+
              ggplot2::ylab("Total Recreational Haddock Mortality (mt)")+
              ggplot2::labs(title = "Haddock Mortality (mt) compared to Total Number of Trips",
                            subtitle = "testing")+
              ggplot2::theme(legend.position = "none")

            fig6<- plotly::ggplotly(p6)%>%
              #layout(title = list(text = paste0('Haddock Mortality (mt) compared to Total Number of Trips'))) %>%
              plotly::style(textposition = "top center")
            fig6
          })
        }
      })

  #### Toggle extra seasons on UI ####
  # Allows for extra seasons to show and hide based on click
  shinyjs::onclick("CODaddSeason",
                   shinyjs::toggle(id = "CodSeason2", anim = TRUE))
  shinyjs::onclick("HADaddSeason",
                   shinyjs::toggle(id = "HadSeason3", anim = TRUE))


  pred <- eventReactive(input$runmeplease,{
    print("STarting this process")
    source(here::here(paste0("model_run.R")), local = TRUE)
    return(predictions_out10)
    print("predicitions out")
  })

  predictions <- reactive({

    #test<- read.csv(here::here("output/output_help_20250106_133859.csv"))
    predictions_out <- read.csv(here::here("data-raw/SQ_predictions_cm.csv")) %>%
      #dplyr::mutate(option = c("SQ")) %>%
      #dplyr::select(!X) %>%
      #rbind(predictions_out10) %>%
      rbind(pred()) %>%
      dplyr::mutate(Value = dplyr::case_when(number_weight == "Weight" ~ as.numeric(Value)/2205, TRUE ~ as.numeric(Value)))
    return(predictions_out)
  })

  #### Regulations ####
  regulations <- observeEvent(input$runmeplease,{
    library(httr)
    library(jsonlite)
    library(openssl)
    library(uuid)

    enqueue_simple_sas <- function(run_name, queue_url_sas = Sys.getenv("AZURE_STORAGE_QUEUE_URL")) {
      stopifnot(nzchar(run_name), nzchar(queue_url_sas))
      payload <- list(
        runName = run_name,
        submissionId = UUIDgenerate(),
        submittedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )
      msg_b64 <- base64_encode(charToRaw(toJSON(payload, auto_unbox = TRUE)))
      xml_body <- sprintf("<QueueMessage><MessageText>%s</MessageText></QueueMessage>", msg_b64)

      res <- POST(
        url = queue_url_sas,
        body = xml_body,
        content_type_xml(),
        add_headers(`x-ms-version` = "2020-10-02")
      )
      stop_for_status(res)
      invisible(TRUE)
    }


    regulations <- NULL
    print("where am i")
    #if(any( )) will run all selected check boxes on UI-regulations selection tab
    if(any("MA" == input$state)){
      print("start MA")

      if(input$BSB_MA_input_type == "All Modes Combined"){
        bsbMAregs <- data.frame(run_name = c(Run_Name()),
                                state = c("MA"),
                                input =  c("BSBma_seas1_op", "BSBma_seas1_cl", "BSBma_1_bag", "BSBma_1_len",
                                           "BSBmaFH_seas2_op", "BSBmaFH_seas2_cl", "BSBmaFH_2_bag", "BSBmaFH_2_len",
                                           "BSBmaPR_seas2_op", "BSBmaPR_seas2_cl", "BSBmaPR_2_bag", "BSBmaPR_2_len",
                                           "BSBmaSH_seas2_op", "BSBmaSH_seas2_cl", "BSBmaSH_2_bag", "BSBmaSH_2_len"),
                                value =  c(as.character(input$BSBma_seas1[1]), as.character(input$BSBma_seas1[2]), as.character(input$BSBma_1_bag), as.character(input$BSBma_1_len),
                                           as.character(input$BSBmaFH_seas2[1]), as.character(input$BSBmaFH_seas2[2]), as.character(input$BSBmaFH_2_bag), as.character(input$BSBmaFH_2_len),
                                           as.character(input$BSBmaPR_seas2[1]), as.character(input$BSBmaPR_seas2[2]), as.character(input$BSBmaPR_2_bag), as.character(input$BSBmaPR_2_len),
                                           as.character(input$BSBmaSH_seas2[1]), as.character(input$BSBmaSH_seas2[2]), as.character(input$BSBmaSH_2_bag), as.character(input$BSBmaSH_2_len)))
      }else{
        bsbMAregs <- data.frame(run_name = c(Run_Name()),
                                state = c("MA"),
                                input =  c("BSBmaFH_seas1_op", "BSBmaFH_seas1_cl", "BSBmaFH_1_bag", "BSBmaFH_1_len",
                                           "BSBmaPR_seas1_op", "BSBmaPR_seas1_cl", "BSBmaPR_1_bag", "BSBmaPR_1_len",
                                           "BSBmaSH_seas1_op", "BSBmaSH_seas1_cl", "BSBmaSH_1_bag", "BSBmaSH_1_len",
                                           "BSBmaFH_seas2_op", "BSBmaFH_seas2_cl", "BSBmaFH_2_bag", "BSBmaFH_2_len",
                                           "BSBmaPR_seas2_op", "BSBmaPR_seas2_cl", "BSBmaPR_2_bag", "BSBmaPR_2_len",
                                           "BSBmaSH_seas2_op", "BSBmaSH_seas2_cl", "BSBmaSH_2_bag", "BSBmaSH_2_len"),
                                value =  c(as.character(input$BSBmaFH_seas1[1]), as.character(input$BSBmaFH_seas1[2]), as.character(input$BSBmaFH_1_bag), as.character(input$BSBmaFH_1_len),
                                           as.character(input$BSBmaPR_seas1[1]), as.character(input$BSBmaPR_seas1[2]), as.character(input$BSBmaPR_1_bag), as.character(input$BSBmaPR_1_len),
                                           as.character(input$BSBmaSH_seas1[1]), as.character(input$BSBmaSH_seas1[2]), as.character(input$BSBmaSH_1_bag), as.character(input$BSBmaSH_1_len),
                                           as.character(input$BSBmaFH_seas2[1]), as.character(input$BSBmaFH_seas2[2]), as.character(input$BSBmaFH_2_bag), as.character(input$BSBmaFH_2_len),
                                           as.character(input$BSBmaPR_seas2[1]), as.character(input$BSBmaPR_seas2[2]), as.character(input$BSBmaPR_2_bag), as.character(input$BSBmaPR_2_len),
                                           as.character(input$BSBmaSH_seas2[1]), as.character(input$BSBmaSH_seas2[2]), as.character(input$BSBmaSH_2_bag), as.character(input$BSBmaSH_2_len)))
      }

      MA_regs <- data.frame(run_name = c(Run_Name()),
                            state = c("MA"),
                            input =  c("SFmaFH_seas1_op", "SFmaFH_seas1_cl", "SFmaFH_1_bag", "SFmaFH_1_len",
                                       "SFmaPR_seas1_op", "SFmaPR_seas1_cl", "SFmaPR_1_bag", "SFmaPR_1_len",
                                       "SFmaSH_seas1_op", "SFmaSH_seas1_cl", "SFmaSH_1_bag", "SFmaSH_1_len",
                                       "SFmaFH_seas2_op", "SFmaFH_seas2_cl", "SFmaFH_2_bag", "SFmaFH_2_len",
                                       "SFmaPR_seas2_op", "SFmaPR_seas2_cl", "SFmaPR_2_bag", "SFmaPR_2_len",
                                       "SFmaSH_seas2_op", "SFmaSH_seas2_cl", "SFmaSH_2_bag", "SFmaSH_2_len",

                                       "SCUPmaFH_seas1_op", "SCUPmaFH_seas1_cl", "SCUPmaFH_1_bag", "SCUPmaFH_1_len",
                                       "SCUPmaPR_seas1_op", "SCUPmaPR_seas1_cl", "SCUPmaPR_1_bag", "SCUPmaPR_1_len",
                                       "SCUPmaSH_seas1_op", "SCUPmaSH_seas1_cl", "SCUPmaSH_1_bag", "SCUPmaSH_1_len",
                                       "SCUPmaFH_seas2_op", "SCUPmaFH_seas2_cl", "SCUPmaFH_2_bag", "SCUPmaFH_2_len",
                                       "SCUPmaPR_seas2_op", "SCUPmaPR_seas2_cl", "SCUPmaPR_2_bag", "SCUPmaPR_2_len",
                                       "SCUPmaSH_seas2_op", "SCUPmaSH_seas2_cl", "SCUPmaSH_2_bag", "SCUPmaSH_2_len",
                                       "SCUPmaFH_seas3_op", "SCUPmaFH_seas3_cl", "SCUPmaFH_3_bag", "SCUPmaFH_3_len"),
                            value =  c(as.character(input$SFmaFH_seas1[1]), as.character(input$SFmaFH_seas1[2]), as.character(input$SFmaFH_1_bag), as.character(input$SFmaFH_1_len),
                                       as.character(input$SFmaPR_seas1[1]), as.character(input$SFmaPR_seas1[2]), as.character(input$SFmaPR_1_bag), as.character(input$SFmaPR_1_len),
                                       as.character(input$SFmaSH_seas1[1]), as.character(input$SFmaSH_seas1[2]), as.character(input$SFmaSH_1_bag), as.character(input$SFmaSH_1_len),
                                       as.character(input$SFmaFH_seas2[1]), as.character(input$SFmaFH_seas2[2]), as.character(input$SFmaFH_2_bag), as.character(input$SFmaFH_2_len),
                                       as.character(input$SFmaPR_seas2[1]), as.character(input$SFmaPR_seas2[2]), as.character(input$SFmaPR_2_bag), as.character(input$SFmaPR_2_len),
                                       as.character(input$SFmaSH_seas2[1]), as.character(input$SFmaSH_seas2[2]), as.character(input$SFmaSH_2_bag), as.character(input$SFmaSH_2_len),

                                       as.character(input$SCUPmaFH_seas1[1]), as.character(input$SCUPmaFH_seas1[2]), as.character(input$SCUPmaFH_1_bag), as.character(input$SCUPmaFH_1_len),
                                       as.character(input$SCUPmaPR_seas1[1]), as.character(input$SCUPmaPR_seas1[2]), as.character(input$SCUPmaPR_1_bag), as.character(input$SCUPmaPR_1_len),
                                       as.character(input$SCUPmaSH_seas1[1]), as.character(input$SCUPmaSH_seas1[2]), as.character(input$SCUPmaSH_1_bag), as.character(input$SCUPmaSH_1_len),
                                       as.character(input$SCUPmaFH_seas2[1]), as.character(input$SCUPmaFH_seas2[2]), as.character(input$SCUPmaFH_2_bag), as.character(input$SCUPmaFH_2_len),
                                       as.character(input$SCUPmaPR_seas2[1]), as.character(input$SCUPmaPR_seas2[2]), as.character(input$SCUPmaPR_2_bag), as.character(input$SCUPmaPR_2_len),
                                       as.character(input$SCUPmaSH_seas2[1]), as.character(input$SCUPmaSH_seas2[2]), as.character(input$SCUPmaSH_2_bag), as.character(input$SCUPmaSH_2_len),
                                       as.character(input$SCUPmaFH_seas3[1]), as.character(input$SCUPmaFH_seas3[2]), as.character(input$SCUPmaFH_3_bag), as.character(input$SCUPmaFH_3_len)))

      print("out SF and Scup")


      regulations <- regulations %>% rbind(MA_regs, bsbMAregs)
      print("made regulations MA")
    }

    if(any("RI" == input$state)){
      if(input$SF_RI_input_type == "All Modes Combined"){
        sfRIregs <- data.frame(run_name = c(Run_Name()),
                               state = c("RI"),
                               input =  c("SFri_seas1_op", "SFri_seas1_cl", "SFri_1_bag", "SFri_1_len",
                                          "SFriFH_seas2_op", "SFriFH_seas2_cl", "SFriFH_2_bag", "SFriFH_2_len",
                                          "SFriPR_seas2_op", "SFriPR_seas2_cl", "SFriPR_2_bag", "SFriPR_2_len",
                                          "SFriSH_seas2_op", "SFriSH_seas2_cl", "SFriSH_2_bag", "SFriSH_2_len"),
                               value =  c(as.character(input$SFri_seas1[1]), as.character(input$SFri_seas1[2]), as.character(input$SFri_1_bag), as.character(input$SFri_1_len),
                                          as.character(input$SFriFH_seas2[1]), as.character(input$SFriFH_seas2[2]), as.character(input$SFriFH_2_bag), as.character(input$SFriFH_2_len),
                                          as.character(input$SFriPR_seas2[1]), as.character(input$SFriPR_seas2[2]), as.character(input$SFriPR_2_bag), as.character(input$SFriPR_2_len),
                                          as.character(input$SFriSH_seas2[1]), as.character(input$SFriSH_seas2[2]), as.character(input$SFriSH_2_bag), as.character(input$SFriSH_2_len)))
      }else{
        sfRIregs <-  data.frame(run_name = c(Run_Name()),
                                state = c("RI"),
                                input =  c("SFriFH_seas1_op", "SFriFH_seas1_cl", "SFriFH_1_bag", "SFriFH_1_len",
                                           "SFriPR_seas1_op", "SFriPR_seas1_cl", "SFriPR_1_bag", "SFriPR_1_len",
                                           "SFriSH_seas1_op", "SFriSH_seas1_cl", "SFriSH_1_bag", "SFriSH_1_len",
                                           "SFriFH_seas2_op", "SFriFH_seas2_cl", "SFriFH_2_bag", "SFriFH_2_len",
                                           "SFriPR_seas2_op", "SFriPR_seas2_cl", "SFriPR_2_bag", "SFriPR_2_len",
                                           "SFriSH_seas2_op", "SFriSH_seas2_cl", "SFriSH_2_bag", "SFriSH_2_len"),
                                value = c( as.character(input$SFriFH_seas1[1]), as.character(input$SFriFH_seas1[2]), as.character(input$SFriFH_1_bag), as.character(input$SFriFH_1_len),
                                           as.character(input$SFriPR_seas1[1]), as.character(input$SFriPR_seas1[2]), as.character(input$SFriPR_1_bag), as.character(input$SFriPR_1_len),
                                           as.character(input$SFriSH_seas1[1]), as.character(input$SFriSH_seas1[2]), as.character(input$SFriSH_1_bag), as.character(input$SFriSH_1_len),
                                           as.character(input$SFriFH_seas2[1]), as.character(input$SFriFH_seas2[2]), as.character(input$SFriFH_2_bag), as.character(input$SFriFH_2_len),
                                           as.character(input$SFriPR_seas2[1]), as.character(input$SFriPR_seas2[2]), as.character(input$SFriPR_2_bag), as.character(input$SFriPR_2_len),
                                           as.character(input$SFriSH_seas2[1]), as.character(input$SFriSH_seas2[2]), as.character(input$SFriSH_2_bag), as.character(input$SFriSH_2_len)))
      }


      RI_regs <- data.frame(run_name = c(Run_Name()),
                            state = c("RI"),
                            input =  c( "BSBriFH_seas1_op", "BSBriFH_seas1_cl", "BSBriFH_1_bag", "BSBriFH_1_len",
                                        "BSBriPR_seas1_op", "BSBriPR_seas1_cl", "BSBriPR_1_bag", "BSBriPR_1_len",
                                        "BSBriSH_seas1_op", "BSBriSH_seas1_cl", "BSBriSH_1_bag", "BSBriSH_1_len",
                                        "BSBriFH_seas2_op", "BSBriFH_seas2_cl", "BSBriFH_2_bag", "BSBriFH_2_len",
                                        "BSBriPR_seas2_op", "BSBriPR_seas2_cl", "BSBriPR_2_bag", "BSBriPR_2_len",
                                        "BSBriSH_seas2_op", "BSBriSH_seas2_cl", "BSBriSH_2_bag", "BSBriSH_2_len",
                                        "BSBriFH_seas3_op", "BSBriFH_seas3_cl", "BSBriFH_3_bag", "BSBriFH_3_len",
                                        "BSBriPR_seas3_op", "BSBriPR_seas3_cl", "BSBriPR_3_bag", "BSBriPR_3_len",
                                        "BSBriSH_seas3_op", "BSBriSH_seas3_cl", "BSBriSH_3_bag", "BSBriSH_3_len",

                                        "SCUPriFH_seas1_op", "SCUPriFH_seas1_cl", "SCUPriFH_1_bag", "SCUPriFH_1_len",
                                        "SCUPriPR_seas1_op", "SCUPriPR_seas1_cl", "SCUPriPR_1_bag", "SCUPriPR_1_len",
                                        "SCUPriSH_seas1_op", "SCUPriSH_seas1_cl", "SCUPriSH_1_bag", "SCUPriSH_1_len",
                                        "SCUPriFH_seas2_op", "SCUPriFH_seas2_cl", "SCUPriFH_2_bag", "SCUPriFH_2_len",
                                        "SCUPriPR_seas2_op", "SCUPriPR_seas2_cl", "SCUPriPR_2_bag", "SCUPriPR_2_len",
                                        "SCUPriSH_seas2_op", "SCUPriSH_seas2_cl", "SCUPriSH_2_bag", "SCUPriSH_2_len",
                                        "SCUPriFH_seas3_op", "SCUPriFH_seas3_cl", "SCUPriFH_3_bag", "SCUPriFH_3_len",
                                        "SCUPriFH_seas4_op", "SCUPriFH_seas4_cl", "SCUPriFH_4_bag", "SCUPriFH_4_len"),
                            value =  c(as.character(input$BSBriFH_seas1[1]), as.character(input$BSBriFH_seas1[2]), as.character(input$BSBriFH_1_bag), as.character(input$BSBriFH_1_len),
                                       as.character(input$BSBriPR_seas1[1]), as.character(input$BSBriPR_seas1[2]), as.character(input$BSBriPR_1_bag), as.character(input$BSBriPR_1_len),
                                       as.character(input$BSBriSH_seas1[1]), as.character(input$BSBriSH_seas1[2]), as.character(input$BSBriSH_1_bag), as.character(input$BSBriSH_1_len),
                                       as.character(input$BSBriFH_seas2[1]), as.character(input$BSBriFH_seas2[2]), as.character(input$BSBriFH_2_bag), as.character(input$BSBriFH_2_len),
                                       as.character(input$BSBriPR_seas2[1]), as.character(input$BSBriPR_seas2[2]), as.character(input$BSBriPR_2_bag), as.character(input$BSBriPR_2_len),
                                       as.character(input$BSBriSH_seas2[1]), as.character(input$BSBriSH_seas2[2]), as.character(input$BSBriSH_2_bag), as.character(input$BSBriSH_2_len),
                                       as.character(input$BSBriFH_seas3[1]), as.character(input$BSBriFH_seas3[2]), as.character(input$BSBriFH_3_bag), as.character(input$BSBriFH_3_len),
                                       as.character(input$BSBriPR_seas3[1]), as.character(input$BSBriPR_seas3[2]), as.character(input$BSBriPR_3_bag), as.character(input$BSBriPR_3_len),
                                       as.character(input$BSBriSH_seas3[1]), as.character(input$BSBriSH_seas3[2]), as.character(input$BSBriSH_3_bag), as.character(input$BSBriSH_3_len),

                                       as.character(input$SCUPriFH_seas1[1]), as.character(input$SCUPriFH_seas1[2]), as.character(input$SCUPriFH_1_bag), as.character(input$SCUPriFH_1_len),
                                       as.character(input$SCUPriPR_seas1[1]), as.character(input$SCUPriPR_seas1[2]), as.character(input$SCUPriPR_1_bag), as.character(input$SCUPriPR_1_len),
                                       as.character(input$SCUPriSH_seas1[1]), as.character(input$SCUPriSH_seas1[2]), as.character(input$SCUPriSH_1_bag), as.character(input$SCUPriSH_1_len),
                                       as.character(input$SCUPriFH_seas2[1]), as.character(input$SCUPriFH_seas2[2]), as.character(input$SCUPriFH_2_bag), as.character(input$SCUPriFH_2_len),
                                       as.character(input$SCUPriPR_seas2[1]), as.character(input$SCUPriPR_seas2[2]), as.character(input$SCUPriPR_2_bag), as.character(input$SCUPriPR_2_len),
                                       as.character(input$SCUPriSH_seas2[1]), as.character(input$SCUPriSH_seas2[2]), as.character(input$SCUPriSH_2_bag), as.character(input$SCUPriSH_2_len),
                                       as.character(input$SCUPriFH_seas3[1]), as.character(input$SCUPriFH_seas3[2]), as.character(input$SCUPriFH_3_bag), as.character(input$SCUPriFH_3_len),
                                       as.character(input$SCUPriFH_seas4[1]), as.character(input$SCUPriFH_seas4[2]), as.character(input$SCUPriFH_4_bag), as.character(input$SCUPriFH_4_len)))
      regulations <- regulations %>% rbind(RI_regs, sfRIregs)
    }

    if(any("CT" == input$state)){
      if(input$SF_CT_input_type == "All Modes Combined"){
        sfCTregs <- data.frame(run_name = c(Run_Name()),
                               state = c("CT"),
                               input =  c("SFct_seas1_op", "SFct_seas1_cl", "SFct_1_bag", "SFct_1_len",
                                          "SFct_seas2_op", "SFct_seas2_cl", "SFct_2_bag", "SFct_2_len",
                                          "SFctFH_seas3_op", "SFctFH_seas3_cl", "SFctFH_3_bag", "SFctFH_3_len",
                                          "SFctPR_seas3_op", "SFctPR_seas3_cl", "SFctPR_3_bag", "SFctPR_3_len",
                                          "SFctSH_seas3_op", "SFctSH_seas3_cl", "SFctSH_3_bag", "SFctSH_3_len"),
                               value =  c(as.character(input$SFct_seas1[1]), as.character(input$SFct_seas1[2]), as.character(input$SFct_1_bag), as.character(input$SFct_1_len),
                                          as.character(input$SFct_seas2[1]), as.character(input$SFct_seas2[2]), as.character(input$SFct_2_bag), as.character(input$SFct_2_len),
                                          as.character(input$SFctFH_seas3[1]), as.character(input$SFctFH_seas3[2]), as.character(input$SFctFH_3_bag), as.character(input$SFctFH_3_len),
                                          as.character(input$SFctPR_seas3[1]), as.character(input$SFctPR_seas3[2]), as.character(input$SFctPR_3_bag), as.character(input$SFctPR_3_len),
                                          as.character(input$SFctSH_seas3[1]), as.character(input$SFctSH_seas3[2]), as.character(input$SFctSH_3_bag), as.character(input$SFctSH_3_len)))
      }else{
        sfCTregs <-  data.frame(run_name = c(Run_Name()),
                                state = c("CT"),
                                input =  c("SFctFH_seas1_op", "SFctFH_seas1_cl", "SFctFH_1_bag", "SFctFH_1_len",
                                           "SFctPR_seas1_op", "SFctPR_seas1_cl", "SFctPR_1_bag", "SFctPR_1_len",
                                           "SFctSH_seas1_op", "SFctSH_seas1_cl", "SFctSH_1_bag", "SFctSH_1_len",
                                           "SFctFH_seas2_op", "SFctFH_seas2_cl", "SFctFH_2_bag", "SFctFH_2_len",
                                           "SFctPR_seas2_op", "SFctPR_seas2_cl", "SFctPR_2_bag", "SFctPR_2_len",
                                           "SFctSH_seas2_op", "SFctSH_seas2_cl", "SFctSH_2_bag", "SFctSH_2_len",
                                           "SFctFH_seas3_op", "SFctFH_seas3_cl", "SFctFH_3_bag", "SFctFH_3_len",
                                           "SFctPR_seas3_op", "SFctPR_seas3_cl", "SFctPR_3_bag", "SFctPR_3_len",
                                           "SFctSH_seas3_op", "SFctSH_seas3_cl", "SFctSH_3_bag", "SFctSH_3_len"),
                                value = c( as.character(input$SFctFH_seas1[1]), as.character(input$SFctFH_seas1[2]), as.character(input$SFctFH_1_bag), as.character(input$SFctFH_1_len),
                                           as.character(input$SFctPR_seas1[1]), as.character(input$SFctPR_seas1[2]), as.character(input$SFctPR_1_bag), as.character(input$SFctPR_1_len),
                                           as.character(input$SFctSH_seas1[1]), as.character(input$SFctSH_seas1[2]), as.character(input$SFctSH_1_bag), as.character(input$SFctSH_1_len),
                                           as.character(input$SFctFH_seas2[1]), as.character(input$SFctFH_seas2[2]), as.character(input$SFctFH_2_bag), as.character(input$SFctFH_2_len),
                                           as.character(input$SFctPR_seas2[1]), as.character(input$SFctPR_seas2[2]), as.character(input$SFctPR_2_bag), as.character(input$SFctPR_2_len),
                                           as.character(input$SFctSH_seas2[1]), as.character(input$SFctSH_seas2[2]), as.character(input$SFctSH_2_bag), as.character(input$SFctSH_2_len),
                                           as.character(input$SFctFH_seas3[1]), as.character(input$SFctFH_seas3[2]), as.character(input$SFctFH_3_bag), as.character(input$SFctFH_3_len),
                                           as.character(input$SFctPR_seas3[1]), as.character(input$SFctPR_seas3[2]), as.character(input$SFctPR_3_bag), as.character(input$SFctPR_3_len),
                                           as.character(input$SFctSH_seas3[1]), as.character(input$SFctSH_seas3[2]), as.character(input$SFctSH_3_bag), as.character(input$SFctSH_3_len)))
      }


      CT_regs <- data.frame(run_name = c(Run_Name()),
                            state = c("CT"),
                            input =  c( "BSBctFH_seas1_op", "BSBctFH_seas1_cl", "BSBctFH_1_bag", "BSBctFH_1_len",
                                        "BSBctPR_seas1_op", "BSBctPR_seas1_cl", "BSBctPR_1_bag", "BSBctPR_1_len",
                                        "BSBctSH_seas1_op", "BSBctSH_seas1_cl", "BSBctSH_1_bag", "BSBctSH_1_len",
                                        "BSBctFH_seas2_op", "BSBctFH_seas2_cl", "BSBctFH_2_bag", "BSBctFH_2_len",
                                        "BSBctPR_seas2_op", "BSBctPR_seas2_cl", "BSBctPR_2_bag", "BSBctPR_2_len",
                                        "BSBctSH_seas2_op", "BSBctSH_seas2_cl", "BSBctSH_2_bag", "BSBctSH_2_len",
                                        "BSBctFH_seas3_op", "BSBctFH_seas3_cl", "BSBctFH_3_bag", "BSBctFH_3_len",
                                        "BSBctPR_seas3_op", "BSBctPR_seas3_cl", "BSBctPR_3_bag", "BSBctPR_3_len",
                                        "BSBctSH_seas3_op", "BSBctSH_seas3_cl", "BSBctSH_3_bag", "BSBctSH_3_len",

                                        "SCUPctFH_seas1_op", "SCUPctFH_seas1_cl", "SCUPctFH_1_bag", "SCUPctFH_1_len",
                                        "SCUPctPR_seas1_op", "SCUPctPR_seas1_cl", "SCUPctPR_1_bag", "SCUPctPR_1_len",
                                        "SCUPctSH_seas1_op", "SCUPctSH_seas1_cl", "SCUPctSH_1_bag", "SCUPctSH_1_len",
                                        "SCUPctFH_seas2_op", "SCUPctFH_seas2_cl", "SCUPctFH_2_bag", "SCUPctFH_2_len",
                                        "SCUPctPR_seas2_op", "SCUPctPR_seas2_cl", "SCUPctPR_2_bag", "SCUPctPR_2_len",
                                        "SCUPctSH_seas2_op", "SCUPctSH_seas2_cl", "SCUPctSH_2_bag", "SCUPctSH_2_len",
                                        "SCUPctFH_seas3_op", "SCUPctFH_seas3_cl", "SCUPctFH_3_bag", "SCUPctFH_3_len",
                                        "SCUPctFH_seas4_op", "SCUPctFH_seas4_cl", "SCUPctFH_4_bag", "SCUPctFH_4_len"),
                            value =  c(as.character(input$BSBctFH_seas1[1]), as.character(input$BSBctFH_seas1[2]), as.character(input$BSBctFH_1_bag), as.character(input$BSBctFH_1_len),
                                       as.character(input$BSBctPR_seas1[1]), as.character(input$BSBctPR_seas1[2]), as.character(input$BSBctPR_1_bag), as.character(input$BSBctPR_1_len),
                                       as.character(input$BSBctSH_seas1[1]), as.character(input$BSBctSH_seas1[2]), as.character(input$BSBctSH_1_bag), as.character(input$BSBctSH_1_len),
                                       as.character(input$BSBctFH_seas2[1]), as.character(input$BSBctFH_seas2[2]), as.character(input$BSBctFH_2_bag), as.character(input$BSBctFH_2_len),
                                       as.character(input$BSBctPR_seas2[1]), as.character(input$BSBctPR_seas2[2]), as.character(input$BSBctPR_2_bag), as.character(input$BSBctPR_2_len),
                                       as.character(input$BSBctSH_seas2[1]), as.character(input$BSBctSH_seas2[2]), as.character(input$BSBctSH_2_bag), as.character(input$BSBctSH_2_len),
                                       as.character(input$BSBctFH_seas3[1]), as.character(input$BSBctFH_seas3[2]), as.character(input$BSBctFH_3_bag), as.character(input$BSBctFH_3_len),
                                       as.character(input$BSBctPR_seas3[1]), as.character(input$BSBctPR_seas3[2]), as.character(input$BSBctPR_3_bag), as.character(input$BSBctPR_3_len),
                                       as.character(input$BSBctSH_seas3[1]), as.character(input$BSBctSH_seas3[2]), as.character(input$BSBctSH_3_bag), as.character(input$BSBctSH_3_len),

                                       as.character(input$SCUPctFH_seas1[1]), as.character(input$SCUPctFH_seas1[2]), as.character(input$SCUPctFH_1_bag), as.character(input$SCUPctFH_1_len),
                                       as.character(input$SCUPctPR_seas1[1]), as.character(input$SCUPctPR_seas1[2]), as.character(input$SCUPctPR_1_bag), as.character(input$SCUPctPR_1_len),
                                       as.character(input$SCUPctSH_seas1[1]), as.character(input$SCUPctSH_seas1[2]), as.character(input$SCUPctSH_1_bag), as.character(input$SCUPctSH_1_len),
                                       as.character(input$SCUPctFH_seas2[1]), as.character(input$SCUPctFH_seas2[2]), as.character(input$SCUPctFH_2_bag), as.character(input$SCUPctFH_2_len),
                                       as.character(input$SCUPctPR_seas2[1]), as.character(input$SCUPctPR_seas2[2]), as.character(input$SCUPctPR_2_bag), as.character(input$SCUPctPR_2_len),
                                       as.character(input$SCUPctSH_seas2[1]), as.character(input$SCUPctSH_seas2[2]), as.character(input$SCUPctSH_2_bag), as.character(input$SCUPctSH_2_len),
                                       as.character(input$SCUPctFH_seas3[1]), as.character(input$SCUPctFH_seas3[2]), as.character(input$SCUPctFH_3_bag), as.character(input$SCUPctFH_3_len),
                                       as.character(input$SCUPctFH_seas4[1]), as.character(input$SCUPctFH_seas4[2]), as.character(input$SCUPctFH_4_bag), as.character(input$SCUPctFH_4_len)))
      regulations <- regulations %>% rbind(CT_regs, sfCTregs)

    }

    if(any("NY" == input$state)){
      if(input$SF_NY_input_type == "All Modes Combined"){
        sfNYregs <- data.frame(run_name = c(Run_Name()),
                               state = c("NY"),
                               input =  c("SFny_seas1_op", "SFny_seas1_cl", "SFny_1_bag", "SFny_1_len",
                                          "SFny_seas2_op", "SFny_seas2_cl", "SFny_2_bag", "SFny_2_len",
                                          "SFnyFH_seas3_op", "SFnyFH_seas3_cl", "SFnyFH_3_bag", "SFnyFH_3_len",
                                          "SFnyPR_seas3_op", "SFnyPR_seas3_cl", "SFnyPR_3_bag", "SFnyPR_3_len",
                                          "SFnySH_seas3_op", "SFnySH_seas3_cl", "SFnySH_3_bag", "SFnySH_3_len"),
                               value =  c(as.character(input$SFny_seas1[1]), as.character(input$SFny_seas1[2]), as.character(input$SFny_1_bag), as.character(input$SFny_1_len),
                                          as.character(input$SFny_seas2[1]), as.character(input$SFny_seas2[2]), as.character(input$SFny_2_bag), as.character(input$SFny_2_len),
                                          as.character(input$SFnyFH_seas3[1]), as.character(input$SFnyFH_seas3[2]), as.character(input$SFnyFH_3_bag), as.character(input$SFnyFH_3_len),
                                          as.character(input$SFnyPR_seas3[1]), as.character(input$SFnyPR_seas3[2]), as.character(input$SFnyPR_3_bag), as.character(input$SFnyPR_3_len),
                                          as.character(input$SFnySH_seas3[1]), as.character(input$SFnySH_seas3[2]), as.character(input$SFnySH_3_bag), as.character(input$SFnySH_3_len)))
      }else{
        sfNYregs <-  data.frame(run_name = c(Run_Name()),
                                state = c("NY"),
                                input =  c("SFnyFH_seas1_op", "SFnyFH_seas1_cl", "SFnyFH_1_bag", "SFnyFH_1_len",
                                           "SFnyPR_seas1_op", "SFnyPR_seas1_cl", "SFnyPR_1_bag", "SFnyPR_1_len",
                                           "SFnySH_seas1_op", "SFnySH_seas1_cl", "SFnySH_1_bag", "SFnySH_1_len",
                                           "SFnyFH_seas2_op", "SFnyFH_seas2_cl", "SFnyFH_2_bag", "SFnyFH_2_len",
                                           "SFnyPR_seas2_op", "SFnyPR_seas2_cl", "SFnyPR_2_bag", "SFnyPR_2_len",
                                           "SFnySH_seas2_op", "SFnySH_seas2_cl", "SFnySH_2_bag", "SFnySH_2_len",
                                           "SFnyFH_seas3_op", "SFnyFH_seas3_cl", "SFnyFH_3_bag", "SFnyFH_3_len",
                                           "SFnyPR_seas3_op", "SFnyPR_seas3_cl", "SFnyPR_3_bag", "SFnyPR_3_len",
                                           "SFnySH_seas3_op", "SFnySH_seas3_cl", "SFnySH_3_bag", "SFnySH_3_len"),
                                value = c( as.character(input$SFnyFH_seas1[1]), as.character(input$SFnyFH_seas1[2]), as.character(input$SFnyFH_1_bag), as.character(input$SFnyFH_1_len),
                                           as.character(input$SFnyPR_seas1[1]), as.character(input$SFnyPR_seas1[2]), as.character(input$SFnyPR_1_bag), as.character(input$SFnyPR_1_len),
                                           as.character(input$SFnySH_seas1[1]), as.character(input$SFnySH_seas1[2]), as.character(input$SFnySH_1_bag), as.character(input$SFnySH_1_len),
                                           as.character(input$SFnyFH_seas2[1]), as.character(input$SFnyFH_seas2[2]), as.character(input$SFnyFH_2_bag), as.character(input$SFnyFH_2_len),
                                           as.character(input$SFnyPR_seas2[1]), as.character(input$SFnyPR_seas2[2]), as.character(input$SFnyPR_2_bag), as.character(input$SFnyPR_2_len),
                                           as.character(input$SFnySH_seas2[1]), as.character(input$SFnySH_seas2[2]), as.character(input$SFnySH_2_bag), as.character(input$SFnySH_2_len),
                                           as.character(input$SFnyFH_seas3[1]), as.character(input$SFnyFH_seas3[2]), as.character(input$SFnyFH_3_bag), as.character(input$SFnyFH_3_len),
                                           as.character(input$SFnyPR_seas3[1]), as.character(input$SFnyPR_seas3[2]), as.character(input$SFnyPR_3_bag), as.character(input$SFnyPR_3_len),
                                           as.character(input$SFnySH_seas3[1]), as.character(input$SFnySH_seas3[2]), as.character(input$SFnySH_3_bag), as.character(input$SFnySH_3_len)))
      }


      if(input$BSB_NY_input_type == "All Modes Combined"){
        bsbNYregs <- data.frame(run_name = c(Run_Name()),
                                state = c("NY"),
                                input =  c("BSBny_seas1_op", "BSBny_seas1_cl", "BSBny_1_bag", "BSBny_1_len",
                                           "BSBny_seas2_op", "BSBny_seas2_cl", "BSBny_2_bag", "BSBny_2_len",
                                           "BSBnyFH_seas3_op", "BSBnyFH_seas3_cl", "BSBnyFH_3_bag", "BSBnyFH_3_len",
                                           "BSBnyPR_seas3_op", "BSBnyPR_seas3_cl", "BSBnyPR_3_bag", "BSBnyPR_3_len",
                                           "BSBnySH_seas3_op", "BSBnySH_seas3_cl", "BSBnySH_3_bag", "BSBnySH_3_len"),
                                value =  c(as.character(input$BSBny_seas1[1]), as.character(input$BSBny_seas1[2]), as.character(input$BSBny_1_bag), as.character(input$BSBny_1_len),
                                           as.character(input$BSBny_seas2[1]), as.character(input$BSBny_seas2[2]), as.character(input$BSBny_2_bag), as.character(input$BSBny_2_len),
                                           as.character(input$BSBnyFH_seas3[1]), as.character(input$BSBnyFH_seas3[2]), as.character(input$BSBnyFH_3_bag), as.character(input$BSBnyFH_3_len),
                                           as.character(input$BSBnyPR_seas3[1]), as.character(input$BSBnyPR_seas3[2]), as.character(input$BSBnyPR_3_bag), as.character(input$BSBnyPR_3_len),
                                           as.character(input$BSBnySH_seas3[1]), as.character(input$BSBnySH_seas3[2]), as.character(input$BSBnySH_3_bag), as.character(input$BSBnySH_3_len)))
      }else{
        bsbNYregs <-  data.frame(run_name = c(Run_Name()),
                                 state = c("NY"),
                                 input =  c( "BSBnyFH_seas1_op", "BSBnyFH_seas1_cl", "BSBnyFH_1_bag", "BSBnyFH_1_len",
                                             "BSBnyPR_seas1_op", "BSBnyPR_seas1_cl", "BSBnyPR_1_bag", "BSBnyPR_1_len",
                                             "BSBnySH_seas1_op", "BSBnySH_seas1_cl", "BSBnySH_1_bag", "BSBnySH_1_len",
                                             "BSBnyFH_seas2_op", "BSBnyFH_seas2_cl", "BSBnyFH_2_bag", "BSBnyFH_2_len",
                                             "BSBnyPR_seas2_op", "BSBnyPR_seas2_cl", "BSBnyPR_2_bag", "BSBnyPR_2_len",
                                             "BSBnySH_seas2_op", "BSBnySH_seas2_cl", "BSBnySH_2_bag", "BSBnySH_2_len",
                                             "BSBnyFH_seas3_op", "BSBnyFH_seas3_cl", "BSBnyFH_3_bag", "BSBnyFH_3_len",
                                             "BSBnyPR_seas3_op", "BSBnyPR_seas3_cl", "BSBnyPR_3_bag", "BSBnyPR_3_len",
                                             "BSBnySH_seas3_op", "BSBnySH_seas3_cl", "BSBnySH_3_bag", "BSBnySH_3_len"),
                                 value = c( as.character(input$BSBnyFH_seas1[1]), as.character(input$BSBnyFH_seas1[2]), as.character(input$BSBnyFH_1_bag), as.character(input$BSBnyFH_1_len),
                                            as.character(input$BSBnyPR_seas1[1]), as.character(input$BSBnyPR_seas1[2]), as.character(input$BSBnyPR_1_bag), as.character(input$BSBnyPR_1_len),
                                            as.character(input$BSBnySH_seas1[1]), as.character(input$BSBnySH_seas1[2]), as.character(input$BSBnySH_1_bag), as.character(input$BSBnySH_1_len),
                                            as.character(input$BSBnyFH_seas2[1]), as.character(input$BSBnyFH_seas2[2]), as.character(input$BSBnyFH_2_bag), as.character(input$BSBnyFH_2_len),
                                            as.character(input$BSBnyPR_seas2[1]), as.character(input$BSBnyPR_seas2[2]), as.character(input$BSBnyPR_2_bag), as.character(input$BSBnyPR_2_len),
                                            as.character(input$BSBnySH_seas2[1]), as.character(input$BSBnySH_seas2[2]), as.character(input$BSBnySH_2_bag), as.character(input$BSBnySH_2_len),
                                            as.character(input$BSBnyFH_seas3[1]), as.character(input$BSBnyFH_seas3[2]), as.character(input$BSBnyFH_3_bag), as.character(input$BSBnyFH_3_len),
                                            as.character(input$BSBnyPR_seas3[1]), as.character(input$BSBnyPR_seas3[2]), as.character(input$BSBnyPR_3_bag), as.character(input$BSBnyPR_3_len),
                                            as.character(input$BSBnySH_seas3[1]), as.character(input$BSBnySH_seas3[2]), as.character(input$BSBnySH_3_bag), as.character(input$BSBnySH_3_len)))
      }



      NY_regs <- data.frame(run_name = c(Run_Name()),
                            state = c("NY"),
                            input =  c( "SCUPnyFH_seas1_op", "SCUPnyFH_seas1_cl", "SCUPnyFH_1_bag", "SCUPnyFH_1_len",
                                        "SCUPnyPR_seas1_op", "SCUPnyPR_seas1_cl", "SCUPnyPR_1_bag", "SCUPnyPR_1_len",
                                        "SCUPnySH_seas1_op", "SCUPnySH_seas1_cl", "SCUPnySH_1_bag", "SCUPnySH_1_len",
                                        "SCUPnyFH_seas2_op", "SCUPnyFH_seas2_cl", "SCUPnyFH_2_bag", "SCUPnyFH_2_len",
                                        "SCUPnyPR_seas2_op", "SCUPnyPR_seas2_cl", "SCUPnyPR_2_bag", "SCUPnyPR_2_len",
                                        "SCUPnySH_seas2_op", "SCUPnySH_seas2_cl", "SCUPnySH_2_bag", "SCUPnySH_2_len",
                                        "SCUPnyFH_seas3_op", "SCUPnyFH_seas3_cl", "SCUPnyFH_3_bag", "SCUPnyFH_3_len",
                                        "SCUPnyFH_seas4_op", "SCUPnyFH_seas4_cl", "SCUPnyFH_4_bag", "SCUPnyFH_4_len"),
                            value =  c(as.character(input$SCUPnyFH_seas1[1]), as.character(input$SCUPnyFH_seas1[2]), as.character(input$SCUPnyFH_1_bag), as.character(input$SCUPnyFH_1_len),
                                       as.character(input$SCUPnyPR_seas1[1]), as.character(input$SCUPnyPR_seas1[2]), as.character(input$SCUPnyPR_1_bag), as.character(input$SCUPnyPR_1_len),
                                       as.character(input$SCUPnySH_seas1[1]), as.character(input$SCUPnySH_seas1[2]), as.character(input$SCUPnySH_1_bag), as.character(input$SCUPnySH_1_len),
                                       as.character(input$SCUPnyFH_seas2[1]), as.character(input$SCUPnyFH_seas2[2]), as.character(input$SCUPnyFH_2_bag), as.character(input$SCUPnyFH_2_len),
                                       as.character(input$SCUPnyPR_seas2[1]), as.character(input$SCUPnyPR_seas2[2]), as.character(input$SCUPnyPR_2_bag), as.character(input$SCUPnyPR_2_len),
                                       as.character(input$SCUPnySH_seas2[1]), as.character(input$SCUPnySH_seas2[2]), as.character(input$SCUPnySH_2_bag), as.character(input$SCUPnySH_2_len),
                                       as.character(input$SCUPnyFH_seas3[1]), as.character(input$SCUPnyFH_seas3[2]), as.character(input$SCUPnyFH_3_bag), as.character(input$SCUPnyFH_3_len),
                                       as.character(input$SCUPnyFH_seas4[1]), as.character(input$SCUPnyFH_seas4[2]), as.character(input$SCUPnyFH_4_bag), as.character(input$SCUPnyFH_4_len)))
      regulations <- regulations %>% rbind(NY_regs, sfNYregs, bsbNYregs)

    }

    if(any("NJ" == input$state)){
      if(input$SF_NJ_input_type == "All Modes Combined"){
        sfNJregs <- data.frame(run_name = c(Run_Name()),
                               state = c("NJ"),
                               input =  c("SFnj_seas1_op", "SFnj_seas1_cl", "SFnj_1_bag", "SFnj_1_len",
                                          "SFnjFH_seas2_op", "SFnjFH_seas2_cl", "SFnjFH_2_bag", "SFnjFH_2_len",
                                          "SFnjPR_seas2_op", "SFnjPR_seas2_cl", "SFnjPR_2_bag", "SFnjPR_2_len",
                                          "SFnjSH_seas2_op", "SFnjSH_seas2_cl", "SFnjSH_2_bag", "SFnjSH_2_len"),
                               value =  c(as.character(input$SFnj_seas1[1]), as.character(input$SFnj_seas1[2]), as.character(input$SFnj_1_bag), as.character(input$SFnj_1_len),
                                          as.character(input$SFnjFH_seas2[1]), as.character(input$SFnjFH_seas2[2]), as.character(input$SFnjFH_2_bag), as.character(input$SFnjFH_2_len),
                                          as.character(input$SFnjPR_seas2[1]), as.character(input$SFnjPR_seas2[2]), as.character(input$SFnjPR_2_bag), as.character(input$SFnjPR_2_len),
                                          as.character(input$SFnjSH_seas2[1]), as.character(input$SFnjSH_seas2[2]), as.character(input$SFnjSH_2_bag), as.character(input$SFnjSH_2_len)))
      }else{
        sfNJregs <-  data.frame(run_name = c(Run_Name()),
                                state = c("NJ"),
                                input =  c("SFnjFH_seas1_op", "SFnjFH_seas1_cl", "SFnjFH_1_bag", "SFnjFH_1_len",
                                           "SFnjPR_seas1_op", "SFnjPR_seas1_cl", "SFnjPR_1_bag", "SFnjPR_1_len",
                                           "SFnjSH_seas1_op", "SFnjSH_seas1_cl", "SFnjSH_1_bag", "SFnjSH_1_len",
                                           "SFnjFH_seas2_op", "SFnjFH_seas2_cl", "SFnjFH_2_bag", "SFnjFH_2_len",
                                           "SFnjPR_seas2_op", "SFnjPR_seas2_cl", "SFnjPR_2_bag", "SFnjPR_2_len",
                                           "SFnjSH_seas2_op", "SFnjSH_seas2_cl", "SFnjSH_2_bag", "SFnjSH_2_len"),
                                value = c( as.character(input$SFnjFH_seas1[1]), as.character(input$SFnjFH_seas1[2]), as.character(input$SFnjFH_1_bag), as.character(input$SFnjFH_1_len),
                                           as.character(input$SFnjPR_seas1[1]), as.character(input$SFnjPR_seas1[2]), as.character(input$SFnjPR_1_bag), as.character(input$SFnjPR_1_len),
                                           as.character(input$SFnjSH_seas1[1]), as.character(input$SFnjSH_seas1[2]), as.character(input$SFnjSH_1_bag), as.character(input$SFnjSH_1_len),
                                           as.character(input$SFnjFH_seas2[1]), as.character(input$SFnjFH_seas2[2]), as.character(input$SFnjFH_2_bag), as.character(input$SFnjFH_2_len),
                                           as.character(input$SFnjPR_seas2[1]), as.character(input$SFnjPR_seas2[2]), as.character(input$SFnjPR_2_bag), as.character(input$SFnjPR_2_len),
                                           as.character(input$SFnjSH_seas2[1]), as.character(input$SFnjSH_seas2[2]), as.character(input$SFnjSH_2_bag), as.character(input$SFnjSH_2_len)))
      }


      if(input$BSB_NJ_input_type == "All Modes Combined"){
        bsbNJregs <- data.frame(run_name = c(Run_Name()),
                                state = c("NJ"),
                                input =  c("BSBnj_seas1_op", "BSBnj_seas1_cl", "BSBnj_1_bag", "BSBnj_1_len",
                                           "BSBnj_seas2_op", "BSBnj_seas2_cl", "BSBnj_2_bag", "BSBnj_2_len",
                                           "BSBnj_seas3_op", "BSBnj_seas3_cl", "BSBnj_3_bag", "BSBnj_3_len",
                                           "BSBnj_seas4_op", "BSBnj_seas4_cl", "BSBnj_4_bag", "BSBnj_4_len",
                                           "BSBnjFH_seas5_op", "BSBnjFH_seas5_cl", "BSBnjFH_5_bag", "BSBnjFH_5_len",
                                           "BSBnjPR_seas5_op", "BSBnjPR_seas5_cl", "BSBnjPR_5_bag", "BSBnjPR_5_len",
                                           "BSBnjSH_seas5_op", "BSBnjSH_seas5_cl", "BSBnjSH_5_bag", "BSBnjSH_5_len"),
                                value =  c(as.character(input$BSBnj_seas1[1]), as.character(input$BSBnj_seas1[2]), as.character(input$BSBnj_1_bag), as.character(input$BSBnj_1_len),
                                           as.character(input$BSBnj_seas2[1]), as.character(input$BSBnj_seas2[2]), as.character(input$BSBnj_2_bag), as.character(input$BSBnj_2_len),
                                           as.character(input$BSBnj_seas3[1]), as.character(input$BSBnj_seas3[2]), as.character(input$BSBnj_3_bag), as.character(input$BSBnj_3_len),
                                           as.character(input$BSBnj_seas4[1]), as.character(input$BSBnj_seas4[2]), as.character(input$BSBnj_4_bag), as.character(input$BSBnj_4_len),
                                           as.character(input$BSBnjFH_seas5[1]), as.character(input$BSBnjFH_seas5[2]), as.character(input$BSBnjFH_5_bag), as.character(input$BSBnjFH_5_len),
                                           as.character(input$BSBnjPR_seas5[1]), as.character(input$BSBnjPR_seas5[2]), as.character(input$BSBnjPR_5_bag), as.character(input$BSBnjPR_5_len),
                                           as.character(input$BSBnjSH_seas5[1]), as.character(input$BSBnjSH_seas5[2]), as.character(input$BSBnjSH_5_bag), as.character(input$BSBnjSH_5_len)))
      }else{
        bsbNJregs <-  data.frame(run_name = c(Run_Name()),
                                 state = c("NJ"),
                                 input =  c( "BSBnjFH_seas1_op", "BSBnjFH_seas1_cl", "BSBnjFH_1_bag", "BSBnjFH_1_len",
                                             "BSBnjPR_seas1_op", "BSBnjPR_seas1_cl", "BSBnjPR_1_bag", "BSBnjPR_1_len",
                                             "BSBnjSH_seas1_op", "BSBnjSH_seas1_cl", "BSBnjSH_1_bag", "BSBnjSH_1_len",
                                             "BSBnjFH_seas2_op", "BSBnjFH_seas2_cl", "BSBnjFH_2_bag", "BSBnjFH_2_len",
                                             "BSBnjPR_seas2_op", "BSBnjPR_seas2_cl", "BSBnjPR_2_bag", "BSBnjPR_2_len",
                                             "BSBnjSH_seas2_op", "BSBnjSH_seas2_cl", "BSBnjSH_2_bag", "BSBnjSH_2_len",
                                             "BSBnjFH_seas3_op", "BSBnjFH_seas3_cl", "BSBnjFH_3_bag", "BSBnjFH_3_len",
                                             "BSBnjPR_seas3_op", "BSBnjPR_seas3_cl", "BSBnjPR_3_bag", "BSBnjPR_3_len",
                                             "BSBnjSH_seas3_op", "BSBnjSH_seas3_cl", "BSBnjSH_3_bag", "BSBnjSH_3_len",
                                             "BSBnjFH_seas4_op", "BSBnjFH_seas4_cl", "BSBnjFH_4_bag", "BSBnjFH_4_len",
                                             "BSBnjPR_seas4_op", "BSBnjPR_seas4_cl", "BSBnjPR_4_bag", "BSBnjPR_4_len",
                                             "BSBnjSH_seas4_op", "BSBnjSH_seas4_cl", "BSBnjSH_4_bag", "BSBnjSH_4_len",
                                             "BSBnjFH_seas5_op", "BSBnjFH_seas5_cl", "BSBnjFH_5_bag", "BSBnjFH_5_len",
                                             "BSBnjPR_seas5_op", "BSBnjPR_seas5_cl", "BSBnjPR_5_bag", "BSBnjPR_5_len",
                                             "BSBnjSH_seas5_op", "BSBnjSH_seas5_cl", "BSBnjSH_5_bag", "BSBnjSH_5_len"),
                                 value = c( as.character(input$BSBnjFH_seas1[1]), as.character(input$BSBnjFH_seas1[2]), as.character(input$BSBnjFH_1_bag), as.character(input$BSBnjFH_1_len),
                                            as.character(input$BSBnjPR_seas1[1]), as.character(input$BSBnjPR_seas1[2]), as.character(input$BSBnjPR_1_bag), as.character(input$BSBnjPR_1_len),
                                            as.character(input$BSBnjSH_seas1[1]), as.character(input$BSBnjSH_seas1[2]), as.character(input$BSBnjSH_1_bag), as.character(input$BSBnjSH_1_len),
                                            as.character(input$BSBnjFH_seas2[1]), as.character(input$BSBnjFH_seas2[2]), as.character(input$BSBnjFH_2_bag), as.character(input$BSBnjFH_2_len),
                                            as.character(input$BSBnjPR_seas2[1]), as.character(input$BSBnjPR_seas2[2]), as.character(input$BSBnjPR_2_bag), as.character(input$BSBnjPR_2_len),
                                            as.character(input$BSBnjSH_seas2[1]), as.character(input$BSBnjSH_seas2[2]), as.character(input$BSBnjSH_2_bag), as.character(input$BSBnjSH_2_len),
                                            as.character(input$BSBnjFH_seas3[1]), as.character(input$BSBnjFH_seas3[2]), as.character(input$BSBnjFH_3_bag), as.character(input$BSBnjFH_3_len),
                                            as.character(input$BSBnjPR_seas3[1]), as.character(input$BSBnjPR_seas3[2]), as.character(input$BSBnjPR_3_bag), as.character(input$BSBnjPR_3_len),
                                            as.character(input$BSBnjSH_seas3[1]), as.character(input$BSBnjSH_seas3[2]), as.character(input$BSBnjSH_3_bag), as.character(input$BSBnjSH_3_len),
                                            as.character(input$BSBnjFH_seas4[1]), as.character(input$BSBnjFH_seas4[2]), as.character(input$BSBnjFH_4_bag), as.character(input$BSBnjFH_4_len),
                                            as.character(input$BSBnjPR_seas4[1]), as.character(input$BSBnjPR_seas4[2]), as.character(input$BSBnjPR_4_bag), as.character(input$BSBnjPR_4_len),
                                            as.character(input$BSBnjSH_seas4[1]), as.character(input$BSBnjSH_seas4[2]), as.character(input$BSBnjSH_4_bag), as.character(input$BSBnjSH_4_len),
                                            as.character(input$BSBnjFH_seas5[1]), as.character(input$BSBnjFH_seas5[2]), as.character(input$BSBnjFH_5_bag), as.character(input$BSBnjFH_5_len),
                                            as.character(input$BSBnjPR_seas5[1]), as.character(input$BSBnjPR_seas5[2]), as.character(input$BSBnjPR_5_bag), as.character(input$BSBnjPR_5_len),
                                            as.character(input$BSBnjSH_seas5[1]), as.character(input$BSBnjSH_seas5[2]), as.character(input$BSBnjSH_5_bag), as.character(input$BSBnjSH_5_len)))
      }


      if(input$SCUP_NJ_input_type == "All Modes Combined"){
        scupNJregs <- data.frame(run_name = c(Run_Name()),
                                 state = c("NJ"),
                                 input =  c("SCUPnj_seas1_op", "SCUPnj_seas1_cl", "SCUPnj_1_bag", "SCUPnj_1_len",
                                            "SCUPnj_seas2_op", "SCUPnj_seas2_cl", "SCUPnj_2_bag", "SCUPnj_2_len",
                                            "SCUPnjFH_seas3_op", "SCUPnjFH_seas3_cl", "SCUPnjFH_3_bag", "SCUPnjFH_3_len",
                                            "SCUPnjPR_seas3_op", "SCUPnjPR_seas3_cl", "SCUPnjPR_3_bag", "SCUPnjPR_3_len",
                                            "SCUPnjSH_seas3_op", "SCUPnjSH_seas3_cl", "SCUPnjSH_3_bag", "SCUPnjSH_3_len"),
                                 value =  c(as.character(input$SCUPnj_seas1[1]), as.character(input$SCUPnj_seas1[2]), as.character(input$SCUPnj_1_bag), as.character(input$SCUPnj_1_len),
                                            as.character(input$SCUPnj_seas2[1]), as.character(input$SCUPnj_seas2[2]), as.character(input$SCUPnj_2_bag), as.character(input$SCUPnj_2_len),
                                            as.character(input$SCUPnjFH_seas3[1]), as.character(input$SCUPnjFH_seas3[2]), as.character(input$SCUPnjFH_3_bag), as.character(input$SCUPnjFH_3_len),
                                            as.character(input$SCUPnjPR_seas3[1]), as.character(input$SCUPnjPR_seas3[2]), as.character(input$SCUPnjPR_3_bag), as.character(input$SCUPnjPR_3_len),
                                            as.character(input$SCUPnjSH_seas3[1]), as.character(input$SCUPnjSH_seas3[2]), as.character(input$SCUPnjSH_3_bag), as.character(input$SCUPnjSH_3_len)))
      }else{
        scupNJregs <-  data.frame(run_name = c(Run_Name()),
                                  state = c("NJ"),
                                  input =  c( "SCUPnjFH_seas1_op", "SCUPnjFH_seas1_cl", "SCUPnjFH_1_bag", "SCUPnjFH_1_len",
                                              "SCUPnjPR_seas1_op", "SCUPnjPR_seas1_cl", "SCUPnjPR_1_bag", "SCUPnjPR_1_len",
                                              "SCUPnjSH_seas1_op", "SCUPnjSH_seas1_cl", "SCUPnjSH_1_bag", "SCUPnjSH_1_len",
                                              "SCUPnjFH_seas2_op", "SCUPnjFH_seas2_cl", "SCUPnjFH_2_bag", "SCUPnjFH_2_len",
                                              "SCUPnjPR_seas2_op", "SCUPnjPR_seas2_cl", "SCUPnjPR_2_bag", "SCUPnjPR_2_len",
                                              "SCUPnjSH_seas2_op", "SCUPnjSH_seas2_cl", "SCUPnjSH_2_bag", "SCUPnjSH_2_len",
                                              "SCUPnjFH_seas3_op", "SCUPnjFH_seas3_cl", "SCUPnjFH_3_bag", "SCUPnjFH_3_len",
                                              "SCUPnjPR_seas3_op", "SCUPnjPR_seas3_cl", "SCUPnjPR_3_bag", "SCUPnjPR_3_len",
                                              "SCUPnjSH_seas3_op", "SCUPnjSH_seas3_cl", "SCUPnjSH_3_bag", "SCUPnjSH_3_len"),
                                  value =  c(as.character(input$SCUPnjFH_seas1[1]), as.character(input$SCUPnjFH_seas1[2]), as.character(input$SCUPnjFH_1_bag), as.character(input$SCUPnjFH_1_len),
                                             as.character(input$SCUPnjPR_seas1[1]), as.character(input$SCUPnjPR_seas1[2]), as.character(input$SCUPnjPR_1_bag), as.character(input$SCUPnjPR_1_len),
                                             as.character(input$SCUPnjSH_seas1[1]), as.character(input$SCUPnjSH_seas1[2]), as.character(input$SCUPnjSH_1_bag), as.character(input$SCUPnjSH_1_len),
                                             as.character(input$SCUPnjFH_seas2[1]), as.character(input$SCUPnjFH_seas2[2]), as.character(input$SCUPnjFH_2_bag), as.character(input$SCUPnjFH_2_len),
                                             as.character(input$SCUPnjPR_seas2[1]), as.character(input$SCUPnjPR_seas2[2]), as.character(input$SCUPnjPR_2_bag), as.character(input$SCUPnjPR_2_len),
                                             as.character(input$SCUPnjSH_seas2[1]), as.character(input$SCUPnjSH_seas2[2]), as.character(input$SCUPnjSH_2_bag), as.character(input$SCUPnjSH_2_len),
                                             as.character(input$SCUPnjFH_seas3[1]), as.character(input$SCUPnjFH_seas3[2]), as.character(input$SCUPnjFH_3_bag), as.character(input$SCUPnjFH_3_len),
                                             as.character(input$SCUPnjPR_seas3[1]), as.character(input$SCUPnjPR_seas3[2]), as.character(input$SCUPnjPR_3_bag), as.character(input$SCUPnjPR_3_len),
                                             as.character(input$SCUPnjSH_seas3[1]), as.character(input$SCUPnjSH_seas3[2]), as.character(input$SCUPnjSH_3_bag), as.character(input$SCUPnjSH_3_len)))
      }
      regulations <- regulations %>% rbind(sfNJregs, bsbNJregs, scupNJregs)

    }

    if(any("DE" == input$state)){
      if(input$SF_DE_input_type == "All Modes Combined"){
        sfDEregs <- data.frame(run_name = c(Run_Name()),
                               state = c("DE"),
                               input =  c("SFde_seas1_op", "SFde_seas1_cl", "SFde_1_bag", "SFde_1_len",
                                          "SFde_seas2_op", "SFde_seas2_cl", "SFde_2_bag", "SFde_2_len",
                                          "SFdeFH_seas3_op", "SFdeFH_seas3_cl", "SFdeFH_3_bag", "SFdeFH_3_len",
                                          "SFdePR_seas3_op", "SFdePR_seas3_cl", "SFdePR_3_bag", "SFdePR_3_len",
                                          "SFdeSH_seas3_op", "SFdeSH_seas3_cl", "SFdeSH_3_bag", "SFdeSH_3_len"),
                               value =  c(as.character(input$SFde_seas1[1]), as.character(input$SFde_seas1[2]), as.character(input$SFde_1_bag), as.character(input$SFde_1_len),
                                          as.character(input$SFde_seas2[1]), as.character(input$SFde_seas2[2]), as.character(input$SFde_2_bag), as.character(input$SFde_2_len),
                                          as.character(input$SFdeFH_seas3[1]), as.character(input$SFdeFH_seas3[2]), as.character(input$SFdeFH_3_bag), as.character(input$SFdeFH_3_len),
                                          as.character(input$SFdePR_seas3[1]), as.character(input$SFdePR_seas3[2]), as.character(input$SFdePR_3_bag), as.character(input$SFdePR_3_len),
                                          as.character(input$SFdeSH_seas3[1]), as.character(input$SFdeSH_seas3[2]), as.character(input$SFdeSH_3_bag), as.character(input$SFdeSH_3_len)))
      }else{
        sfDEregs <-  data.frame(run_name = c(Run_Name()),
                                state = c("DE"),
                                input =  c("SFdeFH_seas1_op", "SFdeFH_seas1_cl", "SFdeFH_1_bag", "SFdeFH_1_len",
                                           "SFdePR_seas1_op", "SFdePR_seas1_cl", "SFdePR_1_bag", "SFdePR_1_len",
                                           "SFdeSH_seas1_op", "SFdeSH_seas1_cl", "SFdeSH_1_bag", "SFdeSH_1_len",
                                           "SFdeFH_seas2_op", "SFdeFH_seas2_cl", "SFdeFH_2_bag", "SFdeFH_2_len",
                                           "SFdePR_seas2_op", "SFdePR_seas2_cl", "SFdePR_2_bag", "SFdePR_2_len",
                                           "SFdeSH_seas2_op", "SFdeSH_seas2_cl", "SFdeSH_2_bag", "SFdeSH_2_len",
                                           "SFdeFH_seas3_op", "SFdeFH_seas3_cl", "SFdeFH_3_bag", "SFdeFH_3_len",
                                           "SFdePR_seas3_op", "SFdePR_seas3_cl", "SFdePR_3_bag", "SFdePR_3_len",
                                           "SFdeSH_seas3_op", "SFdeSH_seas3_cl", "SFdeSH_3_bag", "SFdeSH_3_len"),
                                value = c( as.character(input$SFdeFH_seas1[1]), as.character(input$SFdeFH_seas1[2]), as.character(input$SFdeFH_1_bag), as.character(input$SFdeFH_1_len),
                                           as.character(input$SFdePR_seas1[1]), as.character(input$SFdePR_seas1[2]), as.character(input$SFdePR_1_bag), as.character(input$SFdePR_1_len),
                                           as.character(input$SFdeSH_seas1[1]), as.character(input$SFdeSH_seas1[2]), as.character(input$SFdeSH_1_bag), as.character(input$SFdeSH_1_len),
                                           as.character(input$SFdeFH_seas2[1]), as.character(input$SFdeFH_seas2[2]), as.character(input$SFdeFH_2_bag), as.character(input$SFdeFH_2_len),
                                           as.character(input$SFdePR_seas2[1]), as.character(input$SFdePR_seas2[2]), as.character(input$SFdePR_2_bag), as.character(input$SFdePR_2_len),
                                           as.character(input$SFdeSH_seas2[1]), as.character(input$SFdeSH_seas2[2]), as.character(input$SFdeSH_2_bag), as.character(input$SFdeSH_2_len),
                                           as.character(input$SFdeFH_seas3[1]), as.character(input$SFdeFH_seas3[2]), as.character(input$SFdeFH_3_bag), as.character(input$SFdeFH_3_len),
                                           as.character(input$SFdePR_seas3[1]), as.character(input$SFdePR_seas3[2]), as.character(input$SFdePR_3_bag), as.character(input$SFdePR_3_len),
                                           as.character(input$SFdeSH_seas3[1]), as.character(input$SFdeSH_seas3[2]), as.character(input$SFdeSH_3_bag), as.character(input$SFdeSH_3_len)))
      }


      if(input$BSB_DE_input_type == "All Modes Combined"){
        bsbDEregs <- data.frame(run_name = c(Run_Name()),
                                state = c("DE"),
                                input =  c("BSBde_seas1_op", "BSBde_seas1_cl", "BSBde_1_bag", "BSBde_1_len",
                                           "BSBde_seas2_op", "BSBde_seas2_cl", "BSBde_2_bag", "BSBde_2_len",
                                           "BSBdeFH_seas3_op", "BSBdeFH_seas3_cl", "BSBdeFH_3_bag", "BSBdeFH_3_len",
                                           "BSBdePR_seas3_op", "BSBdePR_seas3_cl", "BSBdePR_3_bag", "BSBdePR_3_len",
                                           "BSBdeSH_seas3_op", "BSBdeSH_seas3_cl", "BSBdeSH_3_bag", "BSBdeSH_3_len"),
                                value =  c(as.character(input$BSBde_seas1[1]), as.character(input$BSBde_seas1[2]), as.character(input$BSBde_1_bag), as.character(input$BSBde_1_len),
                                           as.character(input$BSBde_seas2[1]), as.character(input$BSBde_seas2[2]), as.character(input$BSBde_2_bag), as.character(input$BSBde_2_len),
                                           as.character(input$BSBdeFH_seas3[1]), as.character(input$BSBdeFH_seas3[2]), as.character(input$BSBdeFH_3_bag), as.character(input$BSBdeFH_3_len),
                                           as.character(input$BSBdePR_seas3[1]), as.character(input$BSBdePR_seas3[2]), as.character(input$BSBdePR_3_bag), as.character(input$BSBdePR_3_len),
                                           as.character(input$BSBdeSH_seas3[1]), as.character(input$BSBdeSH_seas3[2]), as.character(input$BSBdeSH_3_bag), as.character(input$BSBdeSH_3_len)))
      }else{
        bsbDEregs <-  data.frame(run_name = c(Run_Name()),
                                 state = c("DE"),
                                 input =  c( "BSBdeFH_seas1_op", "BSBdeFH_seas1_cl", "BSBdeFH_1_bag", "BSBdeFH_1_len",
                                             "BSBdePR_seas1_op", "BSBdePR_seas1_cl", "BSBdePR_1_bag", "BSBdePR_1_len",
                                             "BSBdeSH_seas1_op", "BSBdeSH_seas1_cl", "BSBdeSH_1_bag", "BSBdeSH_1_len",
                                             "BSBdeFH_seas2_op", "BSBdeFH_seas2_cl", "BSBdeFH_2_bag", "BSBdeFH_2_len",
                                             "BSBdePR_seas2_op", "BSBdePR_seas2_cl", "BSBdePR_2_bag", "BSBdePR_2_len",
                                             "BSBdeSH_seas2_op", "BSBdeSH_seas2_cl", "BSBdeSH_2_bag", "BSBdeSH_2_len",
                                             "BSBdeFH_seas3_op", "BSBdeFH_seas3_cl", "BSBdeFH_3_bag", "BSBdeFH_3_len",
                                             "BSBdePR_seas3_op", "BSBdePR_seas3_cl", "BSBdePR_3_bag", "BSBdePR_3_len",
                                             "BSBdeSH_seas3_op", "BSBdeSH_seas3_cl", "BSBdeSH_3_bag", "BSBdeSH_3_len"),
                                 value = c( as.character(input$BSBdeFH_seas1[1]), as.character(input$BSBdeFH_seas1[2]), as.character(input$BSBdeFH_1_bag), as.character(input$BSBdeFH_1_len),
                                            as.character(input$BSBdePR_seas1[1]), as.character(input$BSBdePR_seas1[2]), as.character(input$BSBdePR_1_bag), as.character(input$BSBdePR_1_len),
                                            as.character(input$BSBdeSH_seas1[1]), as.character(input$BSBdeSH_seas1[2]), as.character(input$BSBdeSH_1_bag), as.character(input$BSBdeSH_1_len),
                                            as.character(input$BSBdeFH_seas2[1]), as.character(input$BSBdeFH_seas2[2]), as.character(input$BSBdeFH_2_bag), as.character(input$BSBdeFH_2_len),
                                            as.character(input$BSBdePR_seas2[1]), as.character(input$BSBdePR_seas2[2]), as.character(input$BSBdePR_2_bag), as.character(input$BSBdePR_2_len),
                                            as.character(input$BSBdeSH_seas2[1]), as.character(input$BSBdeSH_seas2[2]), as.character(input$BSBdeSH_2_bag), as.character(input$BSBdeSH_2_len),
                                            as.character(input$BSBdeFH_seas3[1]), as.character(input$BSBdeFH_seas3[2]), as.character(input$BSBdeFH_3_bag), as.character(input$BSBdeFH_3_len),
                                            as.character(input$BSBdePR_seas3[1]), as.character(input$BSBdePR_seas3[2]), as.character(input$BSBdePR_3_bag), as.character(input$BSBdePR_3_len),
                                            as.character(input$BSBdeSH_seas3[1]), as.character(input$BSBdeSH_seas3[2]), as.character(input$BSBdeSH_3_bag), as.character(input$BSBdeSH_3_len)))
      }


      if(input$SCUP_DE_input_type == "All Modes Combined"){
        scupDEregs <- data.frame(run_name = c(Run_Name()),
                                 state = c("DE"),
                                 input =  c("SCUPde_seas1_op", "SCUPde_seas1_cl", "SCUPde_1_bag", "SCUPde_1_len",
                                            "SCUPdeFH_seas2_op", "SCUPdeFH_seas2_cl", "SCUPdeFH_2_bag", "SCUPdeFH_2_len",
                                            "SCUPdePR_seas2_op", "SCUPdePR_seas2_cl", "SCUPdePR_2_bag", "SCUPdePR_2_len",
                                            "SCUPdeSH_seas2_op", "SCUPdeSH_seas2_cl", "SCUPdeSH_2_bag", "SCUPdeSH_2_len"),
                                 value =  c(as.character(input$SCUPde_seas1[1]), as.character(input$SCUPde_seas1[2]), as.character(input$SCUPde_1_bag), as.character(input$SCUPde_1_len),
                                            as.character(input$SCUPdeFH_seas2[1]), as.character(input$SCUPdeFH_seas2[2]), as.character(input$SCUPdeFH_2_bag), as.character(input$SCUPdeFH_2_len),
                                            as.character(input$SCUPdePR_seas2[1]), as.character(input$SCUPdePR_seas2[2]), as.character(input$SCUPdePR_2_bag), as.character(input$SCUPdePR_2_len),
                                            as.character(input$SCUPdeSH_seas2[1]), as.character(input$SCUPdeSH_seas2[2]), as.character(input$SCUPdeSH_2_bag), as.character(input$SCUPdeSH_2_len)))
      }else{
        scupDEregs <-  data.frame(run_name = c(Run_Name()),
                                  state = c("DE"),
                                  input =  c( "SCUPdeFH_seas1_op", "SCUPdeFH_seas1_cl", "SCUPdeFH_1_bag", "SCUPdeFH_1_len",
                                              "SCUPdePR_seas1_op", "SCUPdePR_seas1_cl", "SCUPdePR_1_bag", "SCUPdePR_1_len",
                                              "SCUPdeSH_seas1_op", "SCUPdeSH_seas1_cl", "SCUPdeSH_1_bag", "SCUPdeSH_1_len",
                                              "SCUPdeFH_seas2_op", "SCUPdeFH_seas2_cl", "SCUPdeFH_2_bag", "SCUPdeFH_2_len",
                                              "SCUPdePR_seas2_op", "SCUPdePR_seas2_cl", "SCUPdePR_2_bag", "SCUPdePR_2_len",
                                              "SCUPdeSH_seas2_op", "SCUPdeSH_seas2_cl", "SCUPdeSH_2_bag", "SCUPdeSH_2_len"),
                                  value =  c(as.character(input$SCUPdeFH_seas1[1]), as.character(input$SCUPdeFH_seas1[2]), as.character(input$SCUPdeFH_1_bag), as.character(input$SCUPdeFH_1_len),
                                             as.character(input$SCUPdePR_seas1[1]), as.character(input$SCUPdePR_seas1[2]), as.character(input$SCUPdePR_1_bag), as.character(input$SCUPdePR_1_len),
                                             as.character(input$SCUPdeSH_seas1[1]), as.character(input$SCUPdeSH_seas1[2]), as.character(input$SCUPdeSH_1_bag), as.character(input$SCUPdeSH_1_len),
                                             as.character(input$SCUPdeFH_seas2[1]), as.character(input$SCUPdeFH_seas2[2]), as.character(input$SCUPdeFH_2_bag), as.character(input$SCUPdeFH_2_len),
                                             as.character(input$SCUPdePR_seas2[1]), as.character(input$SCUPdePR_seas2[2]), as.character(input$SCUPdePR_2_bag), as.character(input$SCUPdePR_2_len),
                                             as.character(input$SCUPdeSH_seas2[1]), as.character(input$SCUPdeSH_seas2[2]), as.character(input$SCUPdeSH_2_bag), as.character(input$SCUPdeSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfDEregs, bsbDEregs, scupDEregs)

    }

    if(any("MD" == input$state)){
      if(input$SF_MD_input_type == "All Modes Combined"){
        sfMDregs <- data.frame(run_name = c(Run_Name()),
                               state = c("MD"),
                               input =  c("SFmd_seas1_op", "SFmd_seas1_cl", "SFmd_1_bag", "SFmd_1_len",
                                          "SFmd_seas2_op", "SFmd_seas2_cl", "SFmd_2_bag", "SFmd_2_len",
                                          "SFmdFH_seas3_op", "SFmdFH_seas3_cl", "SFmdFH_3_bag", "SFmdFH_3_len",
                                          "SFmdPR_seas3_op", "SFmdPR_seas3_cl", "SFmdPR_3_bag", "SFmdPR_3_len",
                                          "SFmdSH_seas3_op", "SFmdSH_seas3_cl", "SFmdSH_3_bag", "SFmdSH_3_len"),
                               value =  c(as.character(input$SFmd_seas1[1]), as.character(input$SFmd_seas1[2]), as.character(input$SFmd_1_bag), as.character(input$SFmd_1_len),
                                          as.character(input$SFmd_seas2[1]), as.character(input$SFmd_seas2[2]), as.character(input$SFmd_2_bag), as.character(input$SFmd_2_len),
                                          as.character(input$SFmdFH_seas3[1]), as.character(input$SFmdFH_seas3[2]), as.character(input$SFmdFH_3_bag), as.character(input$SFmdFH_3_len),
                                          as.character(input$SFmdPR_seas3[1]), as.character(input$SFmdPR_seas3[2]), as.character(input$SFmdPR_3_bag), as.character(input$SFmdPR_3_len),
                                          as.character(input$SFmdSH_seas3[1]), as.character(input$SFmdSH_seas3[2]), as.character(input$SFmdSH_3_bag), as.character(input$SFmdSH_3_len)))
      }else{
        sfMDregs <-  data.frame(run_name = c(Run_Name()),
                                state = c("MD"),
                                input =  c("SFmdFH_seas1_op", "SFmdFH_seas1_cl", "SFmdFH_1_bag", "SFmdFH_1_len",
                                           "SFmdPR_seas1_op", "SFmdPR_seas1_cl", "SFmdPR_1_bag", "SFmdPR_1_len",
                                           "SFmdSH_seas1_op", "SFmdSH_seas1_cl", "SFmdSH_1_bag", "SFmdSH_1_len",
                                           "SFmdFH_seas2_op", "SFmdFH_seas2_cl", "SFmdFH_2_bag", "SFmdFH_2_len",
                                           "SFmdPR_seas2_op", "SFmdPR_seas2_cl", "SFmdPR_2_bag", "SFmdPR_2_len",
                                           "SFmdSH_seas2_op", "SFmdSH_seas2_cl", "SFmdSH_2_bag", "SFmdSH_2_len",
                                           "SFmdFH_seas3_op", "SFmdFH_seas3_cl", "SFmdFH_3_bag", "SFmdFH_3_len",
                                           "SFmdPR_seas3_op", "SFmdPR_seas3_cl", "SFmdPR_3_bag", "SFmdPR_3_len",
                                           "SFmdSH_seas3_op", "SFmdSH_seas3_cl", "SFmdSH_3_bag", "SFmdSH_3_len"),
                                value = c( as.character(input$SFmdFH_seas1[1]), as.character(input$SFmdFH_seas1[2]), as.character(input$SFmdFH_1_bag), as.character(input$SFmdFH_1_len),
                                           as.character(input$SFmdPR_seas1[1]), as.character(input$SFmdPR_seas1[2]), as.character(input$SFmdPR_1_bag), as.character(input$SFmdPR_1_len),
                                           as.character(input$SFmdSH_seas1[1]), as.character(input$SFmdSH_seas1[2]), as.character(input$SFmdSH_1_bag), as.character(input$SFmdSH_1_len),
                                           as.character(input$SFmdFH_seas2[1]), as.character(input$SFmdFH_seas2[2]), as.character(input$SFmdFH_2_bag), as.character(input$SFmdFH_2_len),
                                           as.character(input$SFmdPR_seas2[1]), as.character(input$SFmdPR_seas2[2]), as.character(input$SFmdPR_2_bag), as.character(input$SFmdPR_2_len),
                                           as.character(input$SFmdSH_seas2[1]), as.character(input$SFmdSH_seas2[2]), as.character(input$SFmdSH_2_bag), as.character(input$SFmdSH_2_len),
                                           as.character(input$SFmdFH_seas3[1]), as.character(input$SFmdFH_seas3[2]), as.character(input$SFmdFH_3_bag), as.character(input$SFmdFH_3_len),
                                           as.character(input$SFmdPR_seas3[1]), as.character(input$SFmdPR_seas3[2]), as.character(input$SFmdPR_3_bag), as.character(input$SFmdPR_3_len),
                                           as.character(input$SFmdSH_seas3[1]), as.character(input$SFmdSH_seas3[2]), as.character(input$SFmdSH_3_bag), as.character(input$SFmdSH_3_len)))
      }


      if(input$BSB_MD_input_type == "All Modes Combined"){
        bsbMDregs <- data.frame(run_name = c(Run_Name()),
                                state = c("MD"),
                                input =  c("BSBmd_seas1_op", "BSBmd_seas1_cl", "BSBmd_1_bag", "BSBmd_1_len",
                                           "BSBmd_seas2_op", "BSBmd_seas2_cl", "BSBmd_2_bag", "BSBmd_2_len",
                                           "BSBmdFH_seas3_op", "BSBmdFH_seas3_cl", "BSBmdFH_3_bag", "BSBmdFH_3_len",
                                           "BSBmdPR_seas3_op", "BSBmdPR_seas3_cl", "BSBmdPR_3_bag", "BSBmdPR_3_len",
                                           "BSBmdSH_seas3_op", "BSBmdSH_seas3_cl", "BSBmdSH_3_bag", "BSBmdSH_3_len"),
                                value =  c(as.character(input$BSBmd_seas1[1]), as.character(input$BSBmd_seas1[2]), as.character(input$BSBmd_1_bag), as.character(input$BSBmd_1_len),
                                           as.character(input$BSBmd_seas2[1]), as.character(input$BSBmd_seas2[2]), as.character(input$BSBmd_2_bag), as.character(input$BSBmd_2_len),
                                           as.character(input$BSBmdFH_seas3[1]), as.character(input$BSBmdFH_seas3[2]), as.character(input$BSBmdFH_3_bag), as.character(input$BSBmdFH_3_len),
                                           as.character(input$BSBmdPR_seas3[1]), as.character(input$BSBmdPR_seas3[2]), as.character(input$BSBmdPR_3_bag), as.character(input$BSBmdPR_3_len),
                                           as.character(input$BSBmdSH_seas3[1]), as.character(input$BSBmdSH_seas3[2]), as.character(input$BSBmdSH_3_bag), as.character(input$BSBmdSH_3_len)))
      }else{
        bsbMDregs <-  data.frame(run_name = c(Run_Name()),
                                 state = c("MD"),
                                 input =  c( "BSBmdFH_seas1_op", "BSBmdFH_seas1_cl", "BSBmdFH_1_bag", "BSBmdFH_1_len",
                                             "BSBmdPR_seas1_op", "BSBmdPR_seas1_cl", "BSBmdPR_1_bag", "BSBmdPR_1_len",
                                             "BSBmdSH_seas1_op", "BSBmdSH_seas1_cl", "BSBmdSH_1_bag", "BSBmdSH_1_len",
                                             "BSBmdFH_seas2_op", "BSBmdFH_seas2_cl", "BSBmdFH_2_bag", "BSBmdFH_2_len",
                                             "BSBmdPR_seas2_op", "BSBmdPR_seas2_cl", "BSBmdPR_2_bag", "BSBmdPR_2_len",
                                             "BSBmdSH_seas2_op", "BSBmdSH_seas2_cl", "BSBmdSH_2_bag", "BSBmdSH_2_len",
                                             "BSBmdFH_seas3_op", "BSBmdFH_seas3_cl", "BSBmdFH_3_bag", "BSBmdFH_3_len",
                                             "BSBmdPR_seas3_op", "BSBmdPR_seas3_cl", "BSBmdPR_3_bag", "BSBmdPR_3_len",
                                             "BSBmdSH_seas3_op", "BSBmdSH_seas3_cl", "BSBmdSH_3_bag", "BSBmdSH_3_len"),
                                 value = c( as.character(input$BSBmdFH_seas1[1]), as.character(input$BSBmdFH_seas1[2]), as.character(input$BSBmdFH_1_bag), as.character(input$BSBmdFH_1_len),
                                            as.character(input$BSBmdPR_seas1[1]), as.character(input$BSBmdPR_seas1[2]), as.character(input$BSBmdPR_1_bag), as.character(input$BSBmdPR_1_len),
                                            as.character(input$BSBmdSH_seas1[1]), as.character(input$BSBmdSH_seas1[2]), as.character(input$BSBmdSH_1_bag), as.character(input$BSBmdSH_1_len),
                                            as.character(input$BSBmdFH_seas2[1]), as.character(input$BSBmdFH_seas2[2]), as.character(input$BSBmdFH_2_bag), as.character(input$BSBmdFH_2_len),
                                            as.character(input$BSBmdPR_seas2[1]), as.character(input$BSBmdPR_seas2[2]), as.character(input$BSBmdPR_2_bag), as.character(input$BSBmdPR_2_len),
                                            as.character(input$BSBmdSH_seas2[1]), as.character(input$BSBmdSH_seas2[2]), as.character(input$BSBmdSH_2_bag), as.character(input$BSBmdSH_2_len),
                                            as.character(input$BSBmdFH_seas3[1]), as.character(input$BSBmdFH_seas3[2]), as.character(input$BSBmdFH_3_bag), as.character(input$BSBmdFH_3_len),
                                            as.character(input$BSBmdPR_seas3[1]), as.character(input$BSBmdPR_seas3[2]), as.character(input$BSBmdPR_3_bag), as.character(input$BSBmdPR_3_len),
                                            as.character(input$BSBmdSH_seas3[1]), as.character(input$BSBmdSH_seas3[2]), as.character(input$BSBmdSH_3_bag), as.character(input$BSBmdSH_3_len)))
      }


      if(input$SCUP_MD_input_type == "All Modes Combined"){
        scupMDregs <- data.frame(run_name = c(Run_Name()),
                                 state = c("MD"),
                                 input =  c("SCUPmd_seas1_op", "SCUPmd_seas1_cl", "SCUPmd_1_bag", "SCUPmd_1_len",
                                            "SCUPmdFH_seas2_op", "SCUPmdFH_seas2_cl", "SCUPmdFH_2_bag", "SCUPmdFH_2_len",
                                            "SCUPmdPR_seas2_op", "SCUPmdPR_seas2_cl", "SCUPmdPR_2_bag", "SCUPmdPR_2_len",
                                            "SCUPmdSH_seas2_op", "SCUPmdSH_seas2_cl", "SCUPmdSH_2_bag", "SCUPmdSH_2_len"),
                                 value =  c(as.character(input$SCUPmd_seas1[1]), as.character(input$SCUPmd_seas1[2]), as.character(input$SCUPmd_1_bag), as.character(input$SCUPmd_1_len),
                                            as.character(input$SCUPmdFH_seas2[1]), as.character(input$SCUPmdFH_seas2[2]), as.character(input$SCUPmdFH_2_bag), as.character(input$SCUPmdFH_2_len),
                                            as.character(input$SCUPmdPR_seas2[1]), as.character(input$SCUPmdPR_seas2[2]), as.character(input$SCUPmdPR_2_bag), as.character(input$SCUPmdPR_2_len),
                                            as.character(input$SCUPmdSH_seas2[1]), as.character(input$SCUPmdSH_seas2[2]), as.character(input$SCUPmdSH_2_bag), as.character(input$SCUPmdSH_2_len)))
      }else{
        scupMDregs <-  data.frame(run_name = c(Run_Name()),
                                  state = c("MD"),
                                  input =  c( "SCUPmdFH_seas1_op", "SCUPmdFH_seas1_cl", "SCUPmdFH_1_bag", "SCUPmdFH_1_len",
                                              "SCUPmdPR_seas1_op", "SCUPmdPR_seas1_cl", "SCUPmdPR_1_bag", "SCUPmdPR_1_len",
                                              "SCUPmdSH_seas1_op", "SCUPmdSH_seas1_cl", "SCUPmdSH_1_bag", "SCUPmdSH_1_len",
                                              "SCUPmdFH_seas2_op", "SCUPmdFH_seas2_cl", "SCUPmdFH_2_bag", "SCUPmdFH_2_len",
                                              "SCUPmdPR_seas2_op", "SCUPmdPR_seas2_cl", "SCUPmdPR_2_bag", "SCUPmdPR_2_len",
                                              "SCUPmdSH_seas2_op", "SCUPmdSH_seas2_cl", "SCUPmdSH_2_bag", "SCUPmdSH_2_len"),
                                  value =  c(as.character(input$SCUPmdFH_seas1[1]), as.character(input$SCUPmdFH_seas1[2]), as.character(input$SCUPmdFH_1_bag), as.character(input$SCUPmdFH_1_len),
                                             as.character(input$SCUPmdPR_seas1[1]), as.character(input$SCUPmdPR_seas1[2]), as.character(input$SCUPmdPR_1_bag), as.character(input$SCUPmdPR_1_len),
                                             as.character(input$SCUPmdSH_seas1[1]), as.character(input$SCUPmdSH_seas1[2]), as.character(input$SCUPmdSH_1_bag), as.character(input$SCUPmdSH_1_len),
                                             as.character(input$SCUPmdFH_seas2[1]), as.character(input$SCUPmdFH_seas2[2]), as.character(input$SCUPmdFH_2_bag), as.character(input$SCUPmdFH_2_len),
                                             as.character(input$SCUPmdPR_seas2[1]), as.character(input$SCUPmdPR_seas2[2]), as.character(input$SCUPmdPR_2_bag), as.character(input$SCUPmdPR_2_len),
                                             as.character(input$SCUPmdSH_seas2[1]), as.character(input$SCUPmdSH_seas2[2]), as.character(input$SCUPmdSH_2_bag), as.character(input$SCUPmdSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfMDregs, bsbMDregs, scupMDregs)
    }

    if(any("VA" == input$state)){
      if(input$SF_VA_input_type == "All Modes Combined"){
        sfVAregs <- data.frame(run_name = c(Run_Name()),
                               state = c("VA"),
                               input =  c("SFva_seas1_op", "SFva_seas1_cl", "SFva_1_bag", "SFva_1_len",
                                          "SFva_seas2_op", "SFva_seas2_cl", "SFva_2_bag", "SFva_2_len",
                                          "SFvaFH_seas3_op", "SFvaFH_seas3_cl", "SFvaFH_3_bag", "SFvaFH_3_len",
                                          "SFvaPR_seas3_op", "SFvaPR_seas3_cl", "SFvaPR_3_bag", "SFvaPR_3_len",
                                          "SFvaSH_seas3_op", "SFvaSH_seas3_cl", "SFvaSH_3_bag", "SFvaSH_3_len"),
                               value =  c(as.character(input$SFva_seas1[1]), as.character(input$SFva_seas1[2]), as.character(input$SFva_1_bag), as.character(input$SFva_1_len),
                                          as.character(input$SFva_seas2[1]), as.character(input$SFva_seas2[2]), as.character(input$SFva_2_bag), as.character(input$SFva_2_len),
                                          as.character(input$SFvaFH_seas3[1]), as.character(input$SFvaFH_seas3[2]), as.character(input$SFvaFH_3_bag), as.character(input$SFvaFH_3_len),
                                          as.character(input$SFvaPR_seas3[1]), as.character(input$SFvaPR_seas3[2]), as.character(input$SFvaPR_3_bag), as.character(input$SFvaPR_3_len),
                                          as.character(input$SFvaSH_seas3[1]), as.character(input$SFvaSH_seas3[2]), as.character(input$SFvaSH_3_bag), as.character(input$SFvaSH_3_len)))
      }else{
        sfVAregs <-  data.frame(run_name = c(Run_Name()),
                                state = c("VA"),
                                input =  c("SFvaFH_seas1_op", "SFvaFH_seas1_cl", "SFvaFH_1_bag", "SFvaFH_1_len",
                                           "SFvaPR_seas1_op", "SFvaPR_seas1_cl", "SFvaPR_1_bag", "SFvaPR_1_len",
                                           "SFvaSH_seas1_op", "SFvaSH_seas1_cl", "SFvaSH_1_bag", "SFvaSH_1_len",
                                           "SFvaFH_seas2_op", "SFvaFH_seas2_cl", "SFvaFH_2_bag", "SFvaFH_2_len",
                                           "SFvaPR_seas2_op", "SFvaPR_seas2_cl", "SFvaPR_2_bag", "SFvaPR_2_len",
                                           "SFvaSH_seas2_op", "SFvaSH_seas2_cl", "SFvaSH_2_bag", "SFvaSH_2_len",
                                           "SFvaFH_seas3_op", "SFvaFH_seas3_cl", "SFvaFH_3_bag", "SFvaFH_3_len",
                                           "SFvaPR_seas3_op", "SFvaPR_seas3_cl", "SFvaPR_3_bag", "SFvaPR_3_len",
                                           "SFvaSH_seas3_op", "SFvaSH_seas3_cl", "SFvaSH_3_bag", "SFvaSH_3_len"),
                                value = c( as.character(input$SFvaFH_seas1[1]), as.character(input$SFvaFH_seas1[2]), as.character(input$SFvaFH_1_bag), as.character(input$SFvaFH_1_len),
                                           as.character(input$SFvaPR_seas1[1]), as.character(input$SFvaPR_seas1[2]), as.character(input$SFvaPR_1_bag), as.character(input$SFvaPR_1_len),
                                           as.character(input$SFvaSH_seas1[1]), as.character(input$SFvaSH_seas1[2]), as.character(input$SFvaSH_1_bag), as.character(input$SFvaSH_1_len),
                                           as.character(input$SFvaFH_seas2[1]), as.character(input$SFvaFH_seas2[2]), as.character(input$SFvaFH_2_bag), as.character(input$SFvaFH_2_len),
                                           as.character(input$SFvaPR_seas2[1]), as.character(input$SFvaPR_seas2[2]), as.character(input$SFvaPR_2_bag), as.character(input$SFvaPR_2_len),
                                           as.character(input$SFvaSH_seas2[1]), as.character(input$SFvaSH_seas2[2]), as.character(input$SFvaSH_2_bag), as.character(input$SFvaSH_2_len),
                                           as.character(input$SFvaFH_seas3[1]), as.character(input$SFvaFH_seas3[2]), as.character(input$SFvaFH_3_bag), as.character(input$SFvaFH_3_len),
                                           as.character(input$SFvaPR_seas3[1]), as.character(input$SFvaPR_seas3[2]), as.character(input$SFvaPR_3_bag), as.character(input$SFvaPR_3_len),
                                           as.character(input$SFvaSH_seas3[1]), as.character(input$SFvaSH_seas3[2]), as.character(input$SFvaSH_3_bag), as.character(input$SFvaSH_3_len)))
      }


      if(input$BSB_VA_input_type == "All Modes Combined"){
        bsbVAregs <- data.frame(run_name = c(Run_Name()),
                                state = c("VA"),
                                input =  c("BSBva_seas1_op", "BSBva_seas1_cl", "BSBva_1_bag", "BSBva_1_len",
                                           "BSBva_seas2_op", "BSBva_seas2_cl", "BSBva_2_bag", "BSBva_2_len",
                                           "BSBvaFH_seas3_op", "BSBvaFH_seas3_cl", "BSBvaFH_3_bag", "BSBvaFH_3_len",
                                           "BSBvaPR_seas3_op", "BSBvaPR_seas3_cl", "BSBvaPR_3_bag", "BSBvaPR_3_len",
                                           "BSBvaSH_seas3_op", "BSBvaSH_seas3_cl", "BSBvaSH_3_bag", "BSBvaSH_3_len"),
                                value =  c(as.character(input$BSBva_seas1[1]), as.character(input$BSBva_seas1[2]), as.character(input$BSBva_1_bag), as.character(input$BSBva_1_len),
                                           as.character(input$BSBva_seas2[1]), as.character(input$BSBva_seas2[2]), as.character(input$BSBva_2_bag), as.character(input$BSBva_2_len),
                                           as.character(input$BSBvaFH_seas3[1]), as.character(input$BSBvaFH_seas3[2]), as.character(input$BSBvaFH_3_bag), as.character(input$BSBvaFH_3_len),
                                           as.character(input$BSBvaPR_seas3[1]), as.character(input$BSBvaPR_seas3[2]), as.character(input$BSBvaPR_3_bag), as.character(input$BSBvaPR_3_len),
                                           as.character(input$BSBvaSH_seas3[1]), as.character(input$BSBvaSH_seas3[2]), as.character(input$BSBvaSH_3_bag), as.character(input$BSBvaSH_3_len)))
      }else{
        bsbVAregs <-  data.frame(run_name = c(Run_Name()),
                                 state = c("VA"),
                                 input =  c( "BSBvaFH_seas1_op", "BSBvaFH_seas1_cl", "BSBvaFH_1_bag", "BSBvaFH_1_len",
                                             "BSBvaPR_seas1_op", "BSBvaPR_seas1_cl", "BSBvaPR_1_bag", "BSBvaPR_1_len",
                                             "BSBvaSH_seas1_op", "BSBvaSH_seas1_cl", "BSBvaSH_1_bag", "BSBvaSH_1_len",
                                             "BSBvaFH_seas2_op", "BSBvaFH_seas2_cl", "BSBvaFH_2_bag", "BSBvaFH_2_len",
                                             "BSBvaPR_seas2_op", "BSBvaPR_seas2_cl", "BSBvaPR_2_bag", "BSBvaPR_2_len",
                                             "BSBvaSH_seas2_op", "BSBvaSH_seas2_cl", "BSBvaSH_2_bag", "BSBvaSH_2_len",
                                             "BSBvaFH_seas3_op", "BSBvaFH_seas3_cl", "BSBvaFH_3_bag", "BSBvaFH_3_len",
                                             "BSBvaPR_seas3_op", "BSBvaPR_seas3_cl", "BSBvaPR_3_bag", "BSBvaPR_3_len",
                                             "BSBvaSH_seas3_op", "BSBvaSH_seas3_cl", "BSBvaSH_3_bag", "BSBvaSH_3_len"),
                                 value = c( as.character(input$BSBvaFH_seas1[1]), as.character(input$BSBvaFH_seas1[2]), as.character(input$BSBvaFH_1_bag), as.character(input$BSBvaFH_1_len),
                                            as.character(input$BSBvaPR_seas1[1]), as.character(input$BSBvaPR_seas1[2]), as.character(input$BSBvaPR_1_bag), as.character(input$BSBvaPR_1_len),
                                            as.character(input$BSBvaSH_seas1[1]), as.character(input$BSBvaSH_seas1[2]), as.character(input$BSBvaSH_1_bag), as.character(input$BSBvaSH_1_len),
                                            as.character(input$BSBvaFH_seas2[1]), as.character(input$BSBvaFH_seas2[2]), as.character(input$BSBvaFH_2_bag), as.character(input$BSBvaFH_2_len),
                                            as.character(input$BSBvaPR_seas2[1]), as.character(input$BSBvaPR_seas2[2]), as.character(input$BSBvaPR_2_bag), as.character(input$BSBvaPR_2_len),
                                            as.character(input$BSBvaSH_seas2[1]), as.character(input$BSBvaSH_seas2[2]), as.character(input$BSBvaSH_2_bag), as.character(input$BSBvaSH_2_len),
                                            as.character(input$BSBvaFH_seas3[1]), as.character(input$BSBvaFH_seas3[2]), as.character(input$BSBvaFH_3_bag), as.character(input$BSBvaFH_3_len),
                                            as.character(input$BSBvaPR_seas3[1]), as.character(input$BSBvaPR_seas3[2]), as.character(input$BSBvaPR_3_bag), as.character(input$BSBvaPR_3_len),
                                            as.character(input$BSBvaSH_seas3[1]), as.character(input$BSBvaSH_seas3[2]), as.character(input$BSBvaSH_3_bag), as.character(input$BSBvaSH_3_len)))
      }


      if(input$SCUP_VA_input_type == "All Modes Combined"){
        scupVAregs <- data.frame(run_name = c(Run_Name()),
                                 state = c("VA"),
                                 input =  c("SCUPva_seas1_op", "SCUPva_seas1_cl", "SCUPva_1_bag", "SCUPva_1_len",
                                            "SCUPvaFH_seas2_op", "SCUPvaFH_seas2_cl", "SCUPvaFH_2_bag", "SCUPvaFH_2_len",
                                            "SCUPvaPR_seas2_op", "SCUPvaPR_seas2_cl", "SCUPvaPR_2_bag", "SCUPvaPR_2_len",
                                            "SCUPvaSH_seas2_op", "SCUPvaSH_seas2_cl", "SCUPvaSH_2_bag", "SCUPvaSH_2_len"),
                                 value =  c(as.character(input$SCUPva_seas1[1]), as.character(input$SCUPva_seas1[2]), as.character(input$SCUPva_1_bag), as.character(input$SCUPva_1_len),
                                            as.character(input$SCUPvaFH_seas2[1]), as.character(input$SCUPvaFH_seas2[2]), as.character(input$SCUPvaFH_2_bag), as.character(input$SCUPvaFH_2_len),
                                            as.character(input$SCUPvaPR_seas2[1]), as.character(input$SCUPvaPR_seas2[2]), as.character(input$SCUPvaPR_2_bag), as.character(input$SCUPvaPR_2_len),
                                            as.character(input$SCUPvaSH_seas2[1]), as.character(input$SCUPvaSH_seas2[2]), as.character(input$SCUPvaSH_2_bag), as.character(input$SCUPvaSH_2_len)))
      }else{
        scupVAregs <-  data.frame(run_name = c(Run_Name()),
                                  state = c("VA"),
                                  input =  c( "SCUPvaFH_seas1_op", "SCUPvaFH_seas1_cl", "SCUPvaFH_1_bag", "SCUPvaFH_1_len",
                                              "SCUPvaPR_seas1_op", "SCUPvaPR_seas1_cl", "SCUPvaPR_1_bag", "SCUPvaPR_1_len",
                                              "SCUPvaSH_seas1_op", "SCUPvaSH_seas1_cl", "SCUPvaSH_1_bag", "SCUPvaSH_1_len",
                                              "SCUPvaFH_seas2_op", "SCUPvaFH_seas2_cl", "SCUPvaFH_2_bag", "SCUPvaFH_2_len",
                                              "SCUPvaPR_seas2_op", "SCUPvaPR_seas2_cl", "SCUPvaPR_2_bag", "SCUPvaPR_2_len",
                                              "SCUPvaSH_seas2_op", "SCUPvaSH_seas2_cl", "SCUPvaSH_2_bag", "SCUPvaSH_2_len"),
                                  value =  c(as.character(input$SCUPvaFH_seas1[1]), as.character(input$SCUPvaFH_seas1[2]), as.character(input$SCUPvaFH_1_bag), as.character(input$SCUPvaFH_1_len),
                                             as.character(input$SCUPvaPR_seas1[1]), as.character(input$SCUPvaPR_seas1[2]), as.character(input$SCUPvaPR_1_bag), as.character(input$SCUPvaPR_1_len),
                                             as.character(input$SCUPvaSH_seas1[1]), as.character(input$SCUPvaSH_seas1[2]), as.character(input$SCUPvaSH_1_bag), as.character(input$SCUPvaSH_1_len),
                                             as.character(input$SCUPvaFH_seas2[1]), as.character(input$SCUPvaFH_seas2[2]), as.character(input$SCUPvaFH_2_bag), as.character(input$SCUPvaFH_2_len),
                                             as.character(input$SCUPvaPR_seas2[1]), as.character(input$SCUPvaPR_seas2[2]), as.character(input$SCUPvaPR_2_bag), as.character(input$SCUPvaPR_2_len),
                                             as.character(input$SCUPvaSH_seas2[1]), as.character(input$SCUPvaSH_seas2[2]), as.character(input$SCUPvaSH_2_bag), as.character(input$SCUPvaSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfVAregs, bsbVAregs, scupVAregs)
    }

    if(any("NC" == input$state)){
      if(input$SF_NC_input_type == "All Modes Combined"){
        sfNCregs <- data.frame(run_name = c(Run_Name()),
                               state = c("NC"),
                               input =  c("SFnc_seas1_op", "SFnc_seas1_cl", "SFnc_1_bag", "SFnc_1_len",
                                          "SFncFH_seas2_op", "SFncFH_seas2_cl", "SFncFH_2_bag", "SFncFH_2_len",
                                          "SFncPR_seas2_op", "SFncPR_seas2_cl", "SFncPR_2_bag", "SFncPR_2_len",
                                          "SFncSH_seas2_op", "SFncSH_seas2_cl", "SFncSH_2_bag", "SFncSH_2_len"),
                               value =  c(as.character(input$SFnc_seas1[1]), as.character(input$SFnc_seas1[2]), as.character(input$SFnc_1_bag), as.character(input$SFnc_1_len),
                                          as.character(input$SFncFH_seas2[1]), as.character(input$SFncFH_seas2[2]), as.character(input$SFncFH_2_bag), as.character(input$SFncFH_2_len),
                                          as.character(input$SFncPR_seas2[1]), as.character(input$SFncPR_seas2[2]), as.character(input$SFncPR_2_bag), as.character(input$SFncPR_2_len),
                                          as.character(input$SFncSH_seas2[1]), as.character(input$SFncSH_seas2[2]), as.character(input$SFncSH_2_bag), as.character(input$SFncSH_2_len)))
      }else{
        sfNCregs <-  data.frame(run_name = c(Run_Name()),
                                state = c("NC"),
                                input =  c("SFncFH_seas1_op", "SFncFH_seas1_cl", "SFncFH_1_bag", "SFncFH_1_len",
                                           "SFncPR_seas1_op", "SFncPR_seas1_cl", "SFncPR_1_bag", "SFncPR_1_len",
                                           "SFncSH_seas1_op", "SFncSH_seas1_cl", "SFncSH_1_bag", "SFncSH_1_len",
                                           "SFncFH_seas2_op", "SFncFH_seas2_cl", "SFncFH_2_bag", "SFncFH_2_len",
                                           "SFncPR_seas2_op", "SFncPR_seas2_cl", "SFncPR_2_bag", "SFncPR_2_len",
                                           "SFncSH_seas2_op", "SFncSH_seas2_cl", "SFncSH_2_bag", "SFncSH_2_len"),
                                value = c( as.character(input$SFncFH_seas1[1]), as.character(input$SFncFH_seas1[2]), as.character(input$SFncFH_1_bag), as.character(input$SFncFH_1_len),
                                           as.character(input$SFncPR_seas1[1]), as.character(input$SFncPR_seas1[2]), as.character(input$SFncPR_1_bag), as.character(input$SFncPR_1_len),
                                           as.character(input$SFncSH_seas1[1]), as.character(input$SFncSH_seas1[2]), as.character(input$SFncSH_1_bag), as.character(input$SFncSH_1_len),
                                           as.character(input$SFncFH_seas2[1]), as.character(input$SFncFH_seas2[2]), as.character(input$SFncFH_2_bag), as.character(input$SFncFH_2_len),
                                           as.character(input$SFncPR_seas2[1]), as.character(input$SFncPR_seas2[2]), as.character(input$SFncPR_2_bag), as.character(input$SFncPR_2_len),
                                           as.character(input$SFncSH_seas2[1]), as.character(input$SFncSH_seas2[2]), as.character(input$SFncSH_2_bag), as.character(input$SFncSH_2_len)))
      }


      if(input$BSB_NC_input_type == "All Modes Combined"){
        bsbNCregs <- data.frame(run_name = c(Run_Name()),
                                state = c("NC"),
                                input =  c("BSBnc_seas1_op", "BSBnc_seas1_cl", "BSBnc_1_bag", "BSBnc_1_len",
                                           "BSBnc_seas2_op", "BSBnc_seas2_cl", "BSBnc_2_bag", "BSBnc_2_len",
                                           "BSBncFH_seas3_op", "BSBncFH_seas3_cl", "BSBncFH_3_bag", "BSBncFH_3_len",
                                           "BSBncPR_seas3_op", "BSBncPR_seas3_cl", "BSBncPR_3_bag", "BSBncPR_3_len",
                                           "BSBncSH_seas3_op", "BSBncSH_seas3_cl", "BSBncSH_3_bag", "BSBncSH_3_len"),
                                value =  c(as.character(input$BSBnc_seas1[1]), as.character(input$BSBnc_seas1[2]), as.character(input$BSBnc_1_bag), as.character(input$BSBnc_1_len),
                                           as.character(input$BSBnc_seas2[1]), as.character(input$BSBnc_seas2[2]), as.character(input$BSBnc_2_bag), as.character(input$BSBnc_2_len),
                                           as.character(input$BSBncFH_seas3[1]), as.character(input$BSBncFH_seas3[2]), as.character(input$BSBncFH_3_bag), as.character(input$BSBncFH_3_len),
                                           as.character(input$BSBncPR_seas3[1]), as.character(input$BSBncPR_seas3[2]), as.character(input$BSBncPR_3_bag), as.character(input$BSBncPR_3_len),
                                           as.character(input$BSBncSH_seas3[1]), as.character(input$BSBncSH_seas3[2]), as.character(input$BSBncSH_3_bag), as.character(input$BSBncSH_3_len)))
      }else{
        bsbNCregs <-  data.frame(run_name = c(Run_Name()),
                                 state = c("NC"),
                                 input =  c( "BSBncFH_seas1_op", "BSBncFH_seas1_cl", "BSBncFH_1_bag", "BSBncFH_1_len",
                                             "BSBncPR_seas1_op", "BSBncPR_seas1_cl", "BSBncPR_1_bag", "BSBncPR_1_len",
                                             "BSBncSH_seas1_op", "BSBncSH_seas1_cl", "BSBncSH_1_bag", "BSBncSH_1_len",
                                             "BSBncFH_seas2_op", "BSBncFH_seas2_cl", "BSBncFH_2_bag", "BSBncFH_2_len",
                                             "BSBncPR_seas2_op", "BSBncPR_seas2_cl", "BSBncPR_2_bag", "BSBncPR_2_len",
                                             "BSBncSH_seas2_op", "BSBncSH_seas2_cl", "BSBncSH_2_bag", "BSBncSH_2_len",
                                             "BSBncFH_seas3_op", "BSBncFH_seas3_cl", "BSBncFH_3_bag", "BSBncFH_3_len",
                                             "BSBncPR_seas3_op", "BSBncPR_seas3_cl", "BSBncPR_3_bag", "BSBncPR_3_len",
                                             "BSBncSH_seas3_op", "BSBncSH_seas3_cl", "BSBncSH_3_bag", "BSBncSH_3_len"),
                                 value = c( as.character(input$BSBncFH_seas1[1]), as.character(input$BSBncFH_seas1[2]), as.character(input$BSBncFH_1_bag), as.character(input$BSBncFH_1_len),
                                            as.character(input$BSBncPR_seas1[1]), as.character(input$BSBncPR_seas1[2]), as.character(input$BSBncPR_1_bag), as.character(input$BSBncPR_1_len),
                                            as.character(input$BSBncSH_seas1[1]), as.character(input$BSBncSH_seas1[2]), as.character(input$BSBncSH_1_bag), as.character(input$BSBncSH_1_len),
                                            as.character(input$BSBncFH_seas2[1]), as.character(input$BSBncFH_seas2[2]), as.character(input$BSBncFH_2_bag), as.character(input$BSBncFH_2_len),
                                            as.character(input$BSBncPR_seas2[1]), as.character(input$BSBncPR_seas2[2]), as.character(input$BSBncPR_2_bag), as.character(input$BSBncPR_2_len),
                                            as.character(input$BSBncSH_seas2[1]), as.character(input$BSBncSH_seas2[2]), as.character(input$BSBncSH_2_bag), as.character(input$BSBncSH_2_len),
                                            as.character(input$BSBncFH_seas3[1]), as.character(input$BSBncFH_seas3[2]), as.character(input$BSBncFH_3_bag), as.character(input$BSBncFH_3_len),
                                            as.character(input$BSBncPR_seas3[1]), as.character(input$BSBncPR_seas3[2]), as.character(input$BSBncPR_3_bag), as.character(input$BSBncPR_3_len),
                                            as.character(input$BSBncSH_seas3[1]), as.character(input$BSBncSH_seas3[2]), as.character(input$BSBncSH_3_bag), as.character(input$BSBncSH_3_len)))
      }


      if(input$SCUP_NC_input_type == "All Modes Combined"){
        scupNCregs <- data.frame(run_name = c(Run_Name()),
                                 state = c("NC"),
                                 input =  c("SCUPnc_seas1_op", "SCUPnc_seas1_cl", "SCUPnc_1_bag", "SCUPnc_1_len",
                                            "SCUPncFH_seas2_op", "SCUPncFH_seas2_cl", "SCUPncFH_2_bag", "SCUPncFH_2_len",
                                            "SCUPncPR_seas2_op", "SCUPncPR_seas2_cl", "SCUPncPR_2_bag", "SCUPncPR_2_len",
                                            "SCUPncSH_seas2_op", "SCUPncSH_seas2_cl", "SCUPncSH_2_bag", "SCUPncSH_2_len"),
                                 value =  c(as.character(input$SCUPnc_seas1[1]), as.character(input$SCUPnc_seas1[2]), as.character(input$SCUPnc_1_bag), as.character(input$SCUPnc_1_len),
                                            as.character(input$SCUPncFH_seas2[1]), as.character(input$SCUPncFH_seas2[2]), as.character(input$SCUPncFH_2_bag), as.character(input$SCUPncFH_2_len),
                                            as.character(input$SCUPncPR_seas2[1]), as.character(input$SCUPncPR_seas2[2]), as.character(input$SCUPncPR_2_bag), as.character(input$SCUPncPR_2_len),
                                            as.character(input$SCUPncSH_seas2[1]), as.character(input$SCUPncSH_seas2[2]), as.character(input$SCUPncSH_2_bag), as.character(input$SCUPncSH_2_len)))
      }else{
        scupNCregs <-  data.frame(run_name = c(Run_Name()),
                                  state = c("NC"),
                                  input =  c( "SCUPncFH_seas1_op", "SCUPncFH_seas1_cl", "SCUPncFH_1_bag", "SCUPncFH_1_len",
                                              "SCUPncPR_seas1_op", "SCUPncPR_seas1_cl", "SCUPncPR_1_bag", "SCUPncPR_1_len",
                                              "SCUPncSH_seas1_op", "SCUPncSH_seas1_cl", "SCUPncSH_1_bag", "SCUPncSH_1_len",
                                              "SCUPncFH_seas2_op", "SCUPncFH_seas2_cl", "SCUPncFH_2_bag", "SCUPncFH_2_len",
                                              "SCUPncPR_seas2_op", "SCUPncPR_seas2_cl", "SCUPncPR_2_bag", "SCUPncPR_2_len",
                                              "SCUPncSH_seas2_op", "SCUPncSH_seas2_cl", "SCUPncSH_2_bag", "SCUPncSH_2_len"),
                                  value =  c(as.character(input$SCUPncFH_seas1[1]), as.character(input$SCUPncFH_seas1[2]), as.character(input$SCUPncFH_1_bag), as.character(input$SCUPncFH_1_len),
                                             as.character(input$SCUPncPR_seas1[1]), as.character(input$SCUPncPR_seas1[2]), as.character(input$SCUPncPR_1_bag), as.character(input$SCUPncPR_1_len),
                                             as.character(input$SCUPncSH_seas1[1]), as.character(input$SCUPncSH_seas1[2]), as.character(input$SCUPncSH_1_bag), as.character(input$SCUPncSH_1_len),
                                             as.character(input$SCUPncFH_seas2[1]), as.character(input$SCUPncFH_seas2[2]), as.character(input$SCUPncFH_2_bag), as.character(input$SCUPncFH_2_len),
                                             as.character(input$SCUPncPR_seas2[1]), as.character(input$SCUPncPR_seas2[2]), as.character(input$SCUPncPR_2_bag), as.character(input$SCUPncPR_2_len),
                                             as.character(input$SCUPncSH_seas2[1]), as.character(input$SCUPncSH_seas2[2]), as.character(input$SCUPncSH_2_bag), as.character(input$SCUPncSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfNCregs, bsbNCregs, scupNCregs)

    }


    readr::write_csv(regulations, file = here::here(paste0("saved_regs/regs_", input$Run_Name, ".csv")))
    print("saved_inputs")

    enqueue_simple_sas(input$Run_Name)

    return(regulations)

  })

  observeEvent(input$runmeplease, {
    output$message <- renderText("Regulations saved - we will run these soon be sure to change run name before clicking again.")
  })

  # Get list of files from the folder
  available_files <- reactive({
    folder_path <- here::here("output/")
    if (dir.exists(folder_path)) {
      files <- list.files(folder_path, full.names = FALSE)
      if (length(files) > 0) {
        return(files)
      }
    }
    return(character(0))
  })

  file_mapping <- reactive({
    files <- available_files()
    if (length(files) > 0) {
      # Remove file extensions for display names
      display_names <- files %>%
        stringr::str_remove("^output_") %>%         # remove prefix
        stringr::str_remove("_[0-9]+")  %>%
        stringr::str_remove("_[0-9]+") %>%
        stringr::str_remove(".csv")
      # Create named vector: display_name = full_filename
      names(files) <- display_names
      return(files)
    }
    return(character(0))
  })

  # Update dropdown choices when app starts
  observe({
    file_map <- file_mapping()
    if (length(file_map) > 0) {
      updateSelectInput(
        session,
        "file_choice",
        choices = file_map,
        selected = file_map[1]
      )
    } else {
      updateSelectInput(
        session,
        "file_choice",
        choices = "No files available",
        selected = NULL
      )
    }
  })

  # Display file information
  output$file_info <- renderText({
    if (is.null(input$file_choice) || input$file_choice == "No files available") {
      return("No file selected or no files available.")
    }

    file_path <- file.path("output", input$file_choice)

    if (file.exists(file_path)) {
      file_info <- file.info(file_path)
      # Get the display name (without extension) for the selected file
      display_name <- tools::file_path_sans_ext(input$file_choice)
      paste(
        "Display name:", display_name,
        "\nFull filename:", input$file_choice,
        "\nFile size:", round(file_info$size / 1024, 2), "KB",
        "\nLast modified:", format(file_info$mtime, "%Y-%m-%d %H:%M:%S"),
        sep = "\n"
      )
    } else {
      "File not found."
    }
  })

  # Handle file download
  output$download_file <- downloadHandler(
    filename = function() {
      # Return the selected filename (full filename with extension)
      if (!is.null(input$file_choice) && input$file_choice != "No files available") {
        return(input$file_choice)
      } else {
        return("file.txt")  # Fallback filename
      }
    },
    content = function(file) {
      # Copy the selected file to the download location
      if (!is.null(input$file_choice) && input$file_choice != "No files available") {
        file_path <- file.path("output", input$file_choice)
        if (file.exists(file_path)) {
          file.copy(file_path, file)
        } else {
          # If file doesn't exist, create an error file
          writeLines("Error: File not found.", file)
        }
      } else {
        writeLines("Error: No file selected.", file)
      }
    }
  )

  ##### Catch ###########
  which_catch_out<- reactiveVal(TRUE)
  catch_agg <- reactive({

    catch_agg<- predictions() %>%
      #dat %>%
      dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                    number_weight == "Weight") %>%
      dplyr::group_by(option, Category, draw_out) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::mutate(under_acl = dplyr::case_when(Category == "cod" & Value <= cod_acl() ~ 1, TRUE ~ 0),
                    under_acl = dplyr::case_when(Category == "had" & Value <= had_acl() ~ 1, TRUE ~ under_acl)) %>%
      dplyr::group_by(option, Category) %>%
      dplyr::summarise(under_acl = sum(under_acl),
                       Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = c(option), values_from = c(Value, under_acl)) %>%
      dplyr::mutate(Category = dplyr::recode(Category, "cod" = "Cod",
                                             "had" = "Haddock")) %>%
      dplyr::select(Category, Value_SQ, under_acl_SQ, Value_alt, under_acl_alt) %>%
      dplyr::rename(Species = Category, `SQ Total Mortality (mt)` = Value_SQ, `SQ % Under ACL (Out of 100 runs)` = under_acl_SQ,
                    `Alternative Total Mortality (mt)` = Value_alt, `Atlernative % Under ACL (Out of 100 runs)` = under_acl_alt)

    return(catch_agg)
  })

  catch_by_mode <- reactive({

    print("start catch mode")
    catch_by_mode<- predictions() %>%
      #dat %>% #test %>%
      dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                    number_weight == "Weight") %>%
      dplyr::group_by(option, Category, draw_out, mode) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::mutate(under_acl = dplyr::case_when(Category == "cod" & Value <= 99000 ~ 1, TRUE ~ 0),
                    under_acl = dplyr::case_when(Category == "had" & Value <= 1075000 ~ 1, TRUE ~ under_acl)) %>%
      dplyr::group_by(option, Category, mode) %>%
      dplyr::summarise(under_acl = sum(under_acl),
                       Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = c(option), values_from = c(Value, under_acl)) %>%
      dplyr::mutate(Category = dplyr::recode(Category, "cod" = "Cod",
                                             "had" = "Haddock"),
                    mode = dplyr::recode(mode, "fh" = "For Hire",
                                         "pr" = "Private")) %>%
      dplyr::select(Category, Value_SQ, Value_alt,  mode) %>%
      dplyr::rename(Species = Category, `SQ Total Mortality (mt)` = Value_SQ,
                    `Alternative Total Mortality (mt)` = Value_alt, `Mode` = mode)

    return(catch_by_mode)
  })

  #### keep release discards ####
  which_keep_out<- reactiveVal(TRUE)
  keep_agg <- reactive({

# sq<- read.csv(here::here("data-raw/sq_predictions_cm.csv"))%>%
#   dplyr::mutate(Value = dplyr::case_when(number_weight == "Weight" ~ as.numeric(Value)/2205, TRUE ~ as.numeric(Value)))
#  out<- read.csv(here::here("output/output_alt1_20250113_102706.csv"))%>%
#    dplyr::mutate(Value = dplyr::case_when(number_weight == "Weight" ~ as.numeric(Value)/2205, TRUE ~ as.numeric(Value)))
#  dat<- rbind(sq, out)

    keep_agg<- predictions() %>%
      #dat %>% #redictions_out %>%
      dplyr::filter(catch_disposition %in% c("keep", "release", "Discmortality")) %>%
      dplyr::group_by(option, Category, catch_disposition, number_weight, draw_out) %>%
      dplyr::summarise(Value = sum(as.numeric(Value))) %>%
      dplyr::group_by(option, Category, catch_disposition, number_weight) %>%
      tidyr::pivot_wider(names_from = c(option, number_weight), values_from = Value) %>%
      dplyr::mutate(perc_diff_num = ((alt_Number-SQ_Number)/SQ_Number) * 100,
                    perc_diff_wt = ((alt_Weight-SQ_Weight)/SQ_Weight) * 100) %>%
      dplyr::filter(!perc_diff_num == "NA",
                    !perc_diff_wt == "NA") %>%
      dplyr::summarise(SQ_Number = median(SQ_Number), SQ_Weight = median(SQ_Weight),
                       alt_Number = median(alt_Number), alt_Weight = median(alt_Weight),
                       perc_diff_num = median(perc_diff_num), perc_diff_wt = median(perc_diff_wt)) %>%
      dplyr::select(!c(SQ_Number, SQ_Weight)) %>%
      dplyr::mutate(Category = dplyr::recode(Category, "cod" = "Cod",
                                             "had" = "Haddock"),
                    catch_disposition = dplyr::recode(catch_disposition, "keep" = "Harvest",
                                                      "Discmortality" = "Dead Discards",
                                                      "release" = "Discards")) %>%
      dplyr::select(Category, catch_disposition, alt_Number, perc_diff_num, alt_Weight, perc_diff_wt) %>%
      dplyr::rename(Species = Category, Variable = catch_disposition,
                    `Total number of fish` = alt_Number, `% difference from SQ for number of fish` = perc_diff_num,
                    `Total Weight (mt)` = alt_Weight, `% difference from SQ for weight of fish` = perc_diff_wt)

    return(keep_agg)

    })


  keep_by_mode <- reactive({
    keep_by_mode<- predictions() %>%
      #dat %>% #predictions_out %>%
      dplyr::filter(catch_disposition %in% c("keep", "release", "Discmortality")) %>%
      dplyr::group_by(option, Category, catch_disposition, number_weight, draw_out, mode) %>%
      dplyr::summarise(Value = sum(as.numeric(Value))) %>%
      dplyr::group_by(option, Category, catch_disposition, number_weight, mode) %>%
      dplyr::summarise(Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = c(option, number_weight), values_from = Value) %>%
      dplyr::mutate(perc_diff_num = ((alt_Number-SQ_Number)/SQ_Number) * 100,
                    perc_diff_wt = ((alt_Weight-SQ_Weight)/SQ_Weight) * 100) %>%
      dplyr::group_by(Category, catch_disposition, mode) %>%
      dplyr::filter(!perc_diff_num == "NA",
                    !perc_diff_wt == "NA") %>%
      dplyr::summarise(SQ_Number = median(SQ_Number), SQ_Weight = median(SQ_Weight),
                       alt_Number = median(alt_Number), alt_Weight = median(alt_Weight),
                       perc_diff_num = median(perc_diff_num), perc_diff_wt = median(perc_diff_wt)) %>%
      dplyr::select(!c(SQ_Number, SQ_Weight)) %>%
      dplyr::mutate(Category = dplyr::recode(Category, "cod" = "Cod",
                                             "had" = "Haddock"),
                    catch_disposition = dplyr::recode(catch_disposition, "keep" = "Harvest",
                                                      "Discmortality" = "Dead Discards",
                                                      "release" = "Discards"),
                    mode = dplyr::recode(mode, "fh" = "For Hire",
                                         "pr" = "Private")) %>%
      dplyr::select(Category, catch_disposition, mode, alt_Number, perc_diff_num, alt_Weight, perc_diff_wt) %>%
      dplyr::rename(Species = Category, Variable = catch_disposition,
                    `Total Number of fish` = alt_Number, `% difference in number of fish` = perc_diff_num,
                    `Total Weight (mt)` = alt_Weight, `% difference in weight of fish` = perc_diff_wt, `Mode` = mode)
    return(keep_by_mode)
  })
#####################

  ##### Ntrips & welfare #######
  which_welfare_out<- reactiveVal(TRUE)
  welfare_agg <- reactive({

#     sq<- read.csv(here::here("data-raw/sq_predictions_cm.csv"))
#      out<- read.csv(here::here("predictions2.csv")) %>%
#        dplyr::select(!X)
#      dat<- rbind(sq, out)
# #
    welfare2_agg <- predictions() %>%
      #dat %>%
      dplyr::filter(Category =="CV")%>%
      dplyr::group_by( draw_out, option) %>%
      dplyr::summarise(Value = sum(as.numeric(Value))) %>%
      tidyr::pivot_wider(names_from = option, values_from = Value) %>%
      dplyr::mutate(Value_diff = SQ - alt) %>%
      dplyr::filter(!Value_diff == "NA") %>%
      dplyr::ungroup() %>%
      dplyr::summarise(median_cv = median(Value_diff)) %>%
      dplyr::rename(`Relative change in Angler Satisfaction ($)` = median_cv)


    trips_agg<- predictions() %>%
      #dat %>%
      dplyr::filter(Category =="ntrips" & option == "alt") %>%
      dplyr::group_by( draw_out) %>%
      dplyr::summarise(Value = sum(as.numeric(Value))) %>%
      dplyr::summarise(Value = median(Value)) %>%
      dplyr::select(Value) %>%
      dplyr::ungroup() %>%
      dplyr::rename(`Total number of Angler Trips` = Value) %>%
      dplyr::select(`Total number of Angler Trips`)


    welfare_agg<- cbind(welfare2_agg, trips_agg)

    return(welfare_agg)

  })


  welfare_by_mode <- reactive({

    welfare_by_mode2 <- predictions() %>%
      #predictions_out %>%
      dplyr::filter(Category == "CV") %>%
      dplyr::group_by( draw_out, option, mode) %>%
      dplyr::summarise(Value = sum(as.numeric(Value))) %>%
      tidyr::pivot_wider(names_from = option, values_from = Value) %>%
      dplyr::mutate(Value_diff = SQ - alt) %>%
      dplyr::filter(!Value_diff == "NA") %>%
      dplyr::group_by(mode) %>%
      dplyr::summarise(median_cv = median(Value_diff)) %>%
      dplyr::rename(`Relative change in Angler Satisfaction ($)` = median_cv) %>%
      dplyr::ungroup()


    trips_by_mode<- predictions() %>%
      #predictions_out %>%
      dplyr::filter(Category =="ntrips" & option == "alt") %>%
      dplyr::group_by(draw_out, mode) %>%
      dplyr::summarise(Value = sum(as.numeric(Value))) %>%
      dplyr::group_by( mode) %>%
      dplyr::summarise(Value = median(Value)) %>%
      dplyr::select(Value) %>%
      dplyr::ungroup() %>%
      dplyr::rename(`Total number of Angler Trips` = Value) %>%
      dplyr::select(`Total number of Angler Trips`)


    welfare_by_mode<- cbind(welfare_by_mode2, trips_by_mode) %>%
      dplyr::mutate(mode = dplyr::recode(mode, "fh" = "For Hire",
                                         "pr" = "Private"))
    return(welfare_by_mode)
  })

  ###Output Tables
  output$regtableout <- renderTable({
    regs_agg()
  })

  #### Catch tables
  observeEvent(input$bymode, {
    which_catch_out(!which_catch_out())
  })

  which_catch<- reactive({
    if(which_catch_out()){
      catch_agg()
    } else{
      catch_by_mode()
    }
  })

  output$catch_tableout <- renderTable({
    which_catch()
  })

  ### Keep Release
  observeEvent(input$bymode, {
    which_keep_out(!which_keep_out())
  })

  which_keep<- reactive({
    if(which_keep_out()){
      keep_agg()
    } else{
      keep_by_mode()
    }
  })

  output$keep_tableout <- renderTable({
    which_keep()
  })


  #### Welfare
  observeEvent(input$bymode, {
    which_welfare_out(!which_welfare_out())
  })

  which_welfare<- reactive({
    if(which_welfare_out()){
      welfare_agg()
    } else{
      welfare_by_mode()
    }
  })

  output$welfare_tableout <- renderTable({
    which_welfare()
  })

  ### Save data
  observeEvent(input$runmeplease, {
    dat<- pred()

    Regs<- data.frame(Category = c("Cod1_FH_bag", "Cod1_FH_size", "Cod1_FH_Season",
                              "Cod1_PR_bag", "Cod1_PR_size", "Cod1_PR_Season",
                              "Had1_FH_bag", "Had1_FH_size", "Had1_FH_Season",
                              "Had1_PR_bag", "Had1_PR_size", "Had1_PR_Season",
                              "Had2_FH_bag", "Had2_FH_size", "Had2_FH_Season",
                              "Had2_PR_bag", "Had2_PR_size", "Had2_PR_Season",
                              "Cod2_FH_bag", "Cod2_FH_size", "Cod2_FH_Season",
                              "Cod2_PR_bag", "Cod2_PR_size", "Cod2_PR_Season",
                              "Had3_FH_bag", "Had3_FH_size", "Had3_FH_Season",
                              "Had3_PR_bag", "Had3_PR_size", "Had3_PR_Season"),
                      Value = c(input$CodFH_1_bag, input$CodFH_1_len, paste0(input$CodFH_seas1[1], " - ", input$CodFH_seas1[2]),
                              input$CodPR_1_bag, input$CodPR_1_len, paste0(input$CodPR_seas1[1], " - ", input$CodPR_seas1[2]),
                              input$HadFH_1_bag, input$HadFH_1_len, paste0(input$HadFH_seas1[1], " - ", input$HadFH_seas1[2]),
                              input$HadPR_1_bag, input$HadPR_1_len, paste0(input$HadPR_seas1[1], " - ", input$HadPR_seas1[2]),
                              input$HadFH_2_bag, input$HadFH_2_len, paste0(input$HadFH_seas2[1], " - ", input$HadFH_seas2[2]),
                              input$HadPR_2_bag, input$HadPR_2_len, paste0(input$HadPR_seas2[1], " - ", input$HadPR_seas2[2]),
                              input$CodFH_2_bag, input$CodFH_2_len, paste0(input$CodFH_seas2[1], " - ", input$CodFH_seas2[2]),
                              input$CodPR_2_bag, input$CodPR_2_len, paste0(input$CodPR_seas2[1], " - ", input$CodPR_seas2[2]),
                              input$HadFH_3_bag, input$HadFH_3_len, paste0(input$HadFH_seas3[1], " - ", input$HadFH_seas3[2]),
                              input$HadPR_3_bag, input$HadPR_3_len, paste0(input$HadPR_seas3[1], " - ", input$HadPR_seas3[2])),
                      mode = c("NA"), catch_disposition = c("NA"), param = c("NA"), number_weight = c("NA"),
                      season = c("NA"), draw_out = c("NA"), mrip_index = c("NA"),option= c("NA"))

    dat_out<- dat %>% rbind(Regs)
    Run_Name = Run_Name()
    readr::write_csv(dat_out, file = here::here(paste0("output/output_", Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))

    })

  output$downloadData <- downloadHandler(
    filename = function(){"RecDSToutput.xlsx"},
    content = function(filename) {
      df_list <- list(Regulations=regs_agg(), Catch_Mortality_aggregated = catch_agg(), Catch_Mortality_by_mode = catch_by_mode(),
                      Keep_Release_aggregated = keep_agg(), Keep_Release_by_mode = keep_by_mode(),
                      Satisfaction_trips_aggregated = welfare_agg(), Satisfaction_trips_by_mode = welfare_by_mode())
      openxlsx::write.xlsx(append = TRUE, x = df_list , file = filename, row.names = FALSE)
      # wb <- openxlsx::createWorkbook()
      # sheet_1 <- xlsx::createSheet(wb, "Regulations")
      #
      # sheet_2 <- xlsx::createSheet(wb, "Catch_Mortality_aggregated")
      #
      # openxlsx::addDataFrame(
      #   regulations(),
      #   sheet         = sheet_1,
      #   row.names     = FALSE
      # )
      #
      # openxlsx::addDataFrame(
      #   catch_agg(),
      #   sheet         = sheet_2,
      #   row.names     = FALSE
      # )
      #
      # openxlsx::saveWorkbook(wb, file)


      #openxlsx::saveWorkbook(g,filename)
    })

}
shiny::shinyApp(ui = ui, server = server)
