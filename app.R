# Required packages - everything else uses package:: found in r/required_packages.R
library(shiny)
library(shinyjs)
library(dplyr)
library(tidyverse)
library(DT)
library(plotly)

#### Start UI ####
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Gulf of Maine Cod and Haddock Recreational Fisheries Decision Support Tool"),
  #### Regulation Selection ####
  tabsetPanel(
    tabPanel("Cod and Haddock Model Summary",
             p("This page summarizes models results for sets of policies that have been run to date. These are
               intended as a jumping off point for your own model runs."),
             p("The results of your own model runs are stored. Please give it a name (please no underscores “_” ).
               At the end of the day, they are added to this page."),
             p("Hover over each point to view the detailed statistics for each model run. The first section contains
               a table of recreational management measures. The second section contains graphs of mortality. The third
               section has graphs of other performance measures, including Economic Surplus, Trips, and Discards."),

             DTOutput(outputId = "DTout"),

             p("This figure plots the predicted Cod and Haddock recreational mortality for previously simulated management measures."),
             plotlyOutput(outputId = "totCatch"),

             shinyWidgets::awesomeCheckboxGroup(
               inputId = "fig",
               label = "Supplimenatal Figures",
               choices = c( "Consumer Surplus","Releases", "Trips"),
               inline = TRUE,
               status = "danger"),
             uiOutput("addCVCod"),
             uiOutput("addCVHad"),
             uiOutput("addReleaseCod"),
             uiOutput("addReleaseHad"),
             uiOutput("addTripsCod"),
             uiOutput("addTripsHad")),





    tabPanel( "Regulation Selection",
              strong(div("Future Notes Here", style = "color:blue")), # Warning for users
              #Run Button
              actionButton("runmeplease", "Run Me"),
              textInput("Run_Name", "Name this run"),

              fluidRow(
                column(6,
                       titlePanel("Cod"),
                       sliderInput(inputId = "CodFH_seas1", label ="For Hire Open Season 1",
                                   min = as.Date("2024-05-01","%Y-%m-%d"),
                                   max = as.Date("2025-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2024-09-01","%Y-%m-%d"),as.Date("2024-10-31","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "CodFH_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 1)),
                         column(5,
                                sliderInput(inputId = "CodFH_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 22, step = .5))),
                       sliderInput(inputId = "CodPR_seas1", label ="Private Open Season 1",
                                   min = as.Date("2024-05-01","%Y-%m-%d"),
                                   max = as.Date("2025-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2024-09-01","%Y-%m-%d"),as.Date("2024-10-31","%Y-%m-%d")),
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
                                                        min = as.Date("2024-05-01","%Y-%m-%d"),
                                                        max = as.Date("2025-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2025-04-30","%Y-%m-%d"),as.Date("2025-04-30","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "CodFH_2_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "CodFH_2_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 10, step = .5))),
                                            sliderInput(inputId = "CodPR_seas2", label ="Private Open Season 2",
                                                        min = as.Date("2024-05-01","%Y-%m-%d"),
                                                        max = as.Date("2025-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2025-04-30","%Y-%m-%d"),as.Date("2025-04-30","%Y-%m-%d")),
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
                                   min = as.Date("2024-05-01","%Y-%m-%d"),
                                   max = as.Date("2025-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2024-05-01","%Y-%m-%d"),as.Date("2025-02-28","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadFH_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadFH_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 18, step = .5))),
                       sliderInput(inputId = "HadPR_seas1", label ="Private Open Season 1",
                                   min = as.Date("2024-05-01","%Y-%m-%d"),
                                   max = as.Date("2025-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2024-05-01","%Y-%m-%d"),as.Date("2025-02-28","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadPR_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 10)),
                         column(5,
                                sliderInput(inputId = "HadPR_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 17, step = .5))),

                       sliderInput(inputId = "HadFH_seas2", label ="For Hire Open Season 2",
                                   min = as.Date("2024-05-01","%Y-%m-%d"),
                                   max = as.Date("2025-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2025-04-01","%Y-%m-%d"),as.Date("2025-04-30","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadFH_2_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadFH_2_len", label = "Min Length",
                                            min = 15, max = 30, value = 18, step = .5))),
                       sliderInput(inputId = "HadPR_seas2", label ="Private Open Season 2",
                                   min = as.Date("2024-05-01","%Y-%m-%d"),
                                   max = as.Date("2025-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2025-04-01","%Y-%m-%d"),as.Date("2025-04-30","%Y-%m-%d")),
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
                                                        min = as.Date("2024-05-01","%Y-%m-%d"),
                                                        max = as.Date("2025-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2025-04-30","%Y-%m-%d"),as.Date("2025-04-30","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "HadFH_3_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "HadFH_3_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 10, step = .5))),
                                            sliderInput(inputId = "HadPR_seas3", label ="Private Open Season 3",
                                                        min = as.Date("2024-05-01","%Y-%m-%d"),
                                                        max = as.Date("2025-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2025-04-30","%Y-%m-%d"),as.Date("2025-04-30","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "HadPR_3_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "HadPR_3_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 10, step = .5)))))))),


    #### Results ####
    tabPanel("Results",
             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                              tags$div("Calculating...This will take ~15-20 min per state selected.",id="loadmessage")), #Warning for users

             downloadButton(outputId = "downloadData", "Download"),
             actionButton("bymode", "Results by Mode"),
             # Add table outputs
             ## KB - Make tables DTs - should fix RMD documentation issue
             tableOutput(outputId = "regtableout"),

             tableOutput(outputId = "catch_tableout"),
             #tableOutput(outputId = "catchmode"),

             tableOutput(outputId = "welfare_tableout"),
             #tableOutput(outputId = "welfaremode"),

             tableOutput(outputId = "keep_tableout")),
             #tableOutput(outputId = "keepmode")),
     #### By Mode ####
     # tabPanel("Results - By Mode",
     #          tableOutput(outputId = "regtableout"),
     #          tableOutput(outputId = "catchmode"),
     #          tableOutput(outputId = "welfaremode"),
     #          tableOutput(outputId = "keepmode"))#,

    ### Documentation ####
    tabPanel("Documentation",
             htmlOutput("documentation"))

  ))

####### Start Server ###################
server <- function(input, output, session){

  library(magrittr)
  library(webshot)

  df2 <- function(){
    fnames <- list.files(path=here::here("output/"),pattern = "*.csv",full.names = T)

    fnames2<- as.data.frame(fnames) %>%
      tidyr::separate(fnames, into = c("a", "b", "c"), sep = "_") %>%
      dplyr::mutate(c = ifelse(stringr::str_detect(c, "20241"),  "NA", c),
                    d = c(1:nrow(.)),
                    run_name = dplyr::case_when(c != "NA" ~ c, TRUE ~ as.character(d))) %>%
      dplyr::select(run_name)

    df <- fnames %>%
      map_df(~data.table::fread(.,stringsAsFactors=F,check.names=T,strip.white=T))


    df2<- df %>% dplyr::mutate(run_number = as.character(rep(fnames2$run_name, each = 90)))
    return(df2)

  }


  output$DTout <- renderDT({

    SQ_regulations <- read.csv(here::here("data-raw/SQ_regulations.csv")) %>%
      dplyr::rename(Category = Var,
                    SQ = Val)

    df3<- df2() %>% dplyr::filter(!Category %in% c("CV", "ntrips", "nchoiceoccasions","cod" , "had")) %>%
      dplyr::select(Category, Value, run_number) %>%
      dplyr::left_join(SQ_regulations, by = c("Category"))

    seas<- df3 %>% dplyr::filter(stringr::str_detect(Category, "Season")) %>%
      tidyr::separate(Value, into = c("Value1", "Value2"), sep = " - ") %>%
      tidyr::separate(SQ, into = c("SQ1", "SQ2"), sep = " - ") %>%
      dplyr::mutate(Value = as.integer(lubridate::ymd(Value2)-lubridate::ymd(Value1)),
                    SQ = as.integer(lubridate::ymd(SQ2)-lubridate::ymd(SQ1))) %>%
      dplyr::mutate(Diff_from_SQ = dplyr::case_when(Value < SQ ~ "Shorter_Season", TRUE ~ ""),
                    Diff_from_SQ = dplyr::case_when(Value > SQ ~ "Longer_Season", TRUE ~ Diff_from_SQ),
                    Value = paste0(Value1, " - ", Value2)) %>%
      dplyr::select(Category, Diff_from_SQ, run_number)

    bag<- df3 %>% dplyr::filter(stringr::str_detect(Category, "bag")) %>%
      dplyr::mutate(Diff_from_SQ = dplyr::case_when(as.numeric(Value) < as.numeric(SQ) ~ "Smaller Bag", TRUE ~ ""),
                    Diff_from_SQ = dplyr::case_when(as.numeric(Value) > as.numeric(SQ) ~ "Larger Bag", TRUE ~ Diff_from_SQ)) %>%
      dplyr::select(Category, Diff_from_SQ, run_number)

    size<- df3 %>% dplyr::filter(stringr::str_detect(Category, "size")) %>%
      dplyr::mutate(Diff_from_SQ = dplyr::case_when(as.numeric(Value) < as.numeric(SQ) ~ "Smaller Min Length", TRUE ~ ""),
                    Diff_from_SQ = dplyr::case_when(as.numeric(Value) > as.numeric(SQ) ~ "Larger Min Length", TRUE ~ Diff_from_SQ)) %>%
      dplyr::select(Category, Diff_from_SQ, run_number)

    df4<- rbind(seas, bag, size) %>%
      dplyr::ungroup()

    Regs_out <- df3 %>%
      dplyr::left_join(df4, by = c("Category", "run_number")) %>%
      dplyr::select(!SQ) %>%
      dplyr::select(!Opt) %>%
      tidyr::separate(Category, into =c("Species", "mode", "Var"), sep = "_") %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = Var, values_from = c(Value, Diff_from_SQ)) %>%
      dplyr::mutate(Value_Season = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Value_Season),
                    Value_size = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Value_size),
                    Diff_from_SQ_bag = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Diff_from_SQ_bag),
                    Diff_from_SQ_size = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Diff_from_SQ_size),
                    Diff_from_SQ_Season = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Diff_from_SQ_Season),
                    Value_bag = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Value_bag)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Diff_from_SQ = paste0(Diff_from_SQ_bag,Diff_from_SQ_size,Diff_from_SQ_Season)) %>%
      dplyr::select(!c(Diff_from_SQ_bag,Diff_from_SQ_size,Diff_from_SQ_Season)) %>%
      tidyr::pivot_wider(names_from = Species, values_from = c(Diff_from_SQ, Value_bag, Value_size, Value_Season)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cod_bag = paste0(Value_bag_Cod1, " , ", Value_bag_Cod2),
                    cod_size = paste0(Value_size_Cod1, " , ", Value_size_Cod2),
                    cod_season = paste0(Value_Season_Cod1, " , ", Value_Season_Cod2),
                    had_bag = paste0(Value_bag_Had1, " , ", Value_bag_Had2, " , ", Value_bag_Had3),
                    had_size = paste0(Value_size_Had1, " , ", Value_size_Had2, " , ", Value_size_Had3),
                    had_season = paste0(Value_Season_Had1, " , ", Value_Season_Had2, " , ", Value_Season_Had3),
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
                    had_season = stringr::str_remove(had_season, "NA ,"),
                    Diff_from_SQ_cod = paste0(Diff_from_SQ_Cod1, " , ", Diff_from_SQ_Cod2),
                    Diff_from_SQ_had = paste0(Diff_from_SQ_Had1, " , ", Diff_from_SQ_Had2, " , ", Diff_from_SQ_Had3),
                    Diff_from_SQ_cod = stringr::str_remove(Diff_from_SQ_cod, " , NA"),
                    Diff_from_SQ_cod = stringr::str_remove(Diff_from_SQ_cod, "NANA"),

                    Diff_from_SQ_cod = stringr::str_remove(Diff_from_SQ_cod, "NA ,"),
                    Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, "NA ,"),
                    Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, " , NA"),
                    Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, " , NANA"),
                    Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, "NANA")) %>%
      dplyr::select(mode, run_number, Diff_from_SQ_cod, Diff_from_SQ_had, cod_bag, cod_size, cod_season, had_bag, had_size, had_season) %>%
    dplyr::mutate(cod_season = stringr::str_remove(cod_season, "2024-"),
                  cod_season = stringr::str_remove(cod_season, "2025-"),
                  had_season = stringr::str_remove(had_season, "2024-"),
                  had_season = stringr::str_remove(had_season, "2025-"),
                  cod_season = stringr::str_remove(cod_season, "2024-"),
                  cod_season = stringr::str_remove(cod_season, "2025-"),
                  had_season = stringr::str_remove(had_season, "2024-"),
                  had_season = stringr::str_remove(had_season, "2025-"),
                  cod_season = stringr::str_remove(cod_season, "2024-"),
                  cod_season = stringr::str_remove(cod_season, "2025-"),
                  had_season = stringr::str_remove(had_season, "2024-"),
                  had_season = stringr::str_remove(had_season, "2025-"),
                  cod_season = stringr::str_remove(cod_season, "2024-"),
                  cod_season = stringr::str_remove(cod_season, "2025-"),
                  had_season = stringr::str_remove(had_season, "2024-"),
                  had_season = stringr::str_remove(had_season, "2025-"))


    DT::datatable(Regs_out)
  })

  output$totCatch <- renderPlotly({

    catch_agg<- df2() %>%
      dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                    number_weight == "Weight") %>%
      dplyr::group_by(run_number, Category) %>%
      dplyr::summarise(Value = sum(as.numeric(Value))) %>%
      dplyr::mutate(Value = Value * lb_to_mt) %>%
      dplyr::mutate(under_acl = dplyr::case_when(Category == "cod" & Value <= cod_acl ~ 1, TRUE ~ 0),
                    under_acl = dplyr::case_when(Category == "had" & Value <= had_acl ~ 1, TRUE ~ under_acl)) %>%
      dplyr::group_by(run_number, Category) %>%
      dplyr::summarise(under_acl = sum(under_acl),
                       Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = Category, values_from = c(Value, under_acl))

    test<- 1:5
    p<- catch_agg %>%
      dplyr::mutate(under_acl_cod = as.integer(under_acl_cod)) %>%
      ggplot2::ggplot(aes(x = Value_cod, y = Value_had))+
      geom_point(aes(label = run_number, colour = test)) +
      scale_colour_gradient2(low = "white", high = "darkgreen") +
      #geom_text(aes(label = run_number, y = Value_had + 0.25))+
      geom_text(aes(label=run_number))+
      #geom_text(aes(label=ifelse(Value_cod>cod_acl & Value_had > had_acl, as.character(run_number), ' '), hjust=1, vjust=1))+
      geom_vline( xintercept =cod_acl, linetype="dashed")+
      geom_hline( yintercept =had_acl, color="grey45")+
      scale_colour_gradient(low = "white", high = "darkgreen")+
      ggtitle("Cod and Haddock Mortality")+
      ylab("Total Haddock Mortality (mt)")+
      xlab("Total Cod Mortality (mt)")

    fig<- plotly::ggplotly(p,
                           tooltip = c("x", "y", "colour")) %>%
      plotly::style(textposition = "top")
    fig
  })

  output$addCVCod <- renderUI({

    if(any("Consumer Surplus" == input$fig)){

      renderPlotly({
        welfare <-  df2() %>%
          dplyr::filter(Category %in% c("CV")) %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::group_by(run_number,option, Category) %>%
          dplyr::summarise(CV = median(Value))


        catch<- df2() %>%
          dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * 0.000454) %>%
          dplyr::group_by(run_number, option, Category) %>%
          dplyr::summarise(Value = median(Value)) %>%
          tidyr::pivot_wider(names_from = Category, values_from = Value) %>%
          dplyr::left_join(welfare) %>%
          dplyr::select(!Category)

        p1<- catch %>% ggplot2::ggplot(aes(x = cod, y = CV))+
          geom_point() +
          geom_vline( xintercept =cod_acl)+
          geom_text(aes(label=run_number, hjust=1, vjust=1))+
          ggtitle("Cod - Consumer Surplus")+
          ylab("Consumer Surplus ($)")+
          xlab("Total Cod Mortality")+
          theme(legend.position = "none")

        fig1<- ggplotly(p1) %>%
          plotly::style(textposition = "top")

        fig1
      })

    }
  })


  output$addCVHad <- renderUI({

    if(any("Consumer Surplus" == input$fig)){

      renderPlotly({
        welfare <-  df2() %>%
          dplyr::filter(Category %in% c("CV")) %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::group_by(run_number,option, Category) %>%
          dplyr::summarise(CV = median(Value))

        catch<- df2() %>%
          dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * 0.000454) %>%
          dplyr::group_by(run_number, option, Category) %>%
          dplyr::summarise(Value = median(Value)) %>%
          tidyr::pivot_wider(names_from = Category, values_from = Value) %>%
          dplyr::left_join(welfare) %>%
          dplyr::select(!Category)

        p2<- catch %>% ggplot2::ggplot(aes(x = had, y = CV))+
          geom_point() +
          geom_vline( xintercept =had_acl)+
          geom_text(aes(label=run_number, hjust=1, vjust=1))+
          ggtitle("Haddock - Consumer Surplus")+
          ylab("Consumer Surplus ($)")+
          xlab("Total Haddock Mortality")+
          theme(legend.position = "none")

        fig2<- ggplotly(p2) %>%
          plotly::style(textposition = "top")
        fig2
      })

    }
  })

  output$addReleaseCod <- renderUI({

    if(any("Releases" == input$fig)){

      renderPlotly({
        release <-  df2() %>%
          dplyr::filter(catch_disposition %in% c("release")) %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::group_by(run_number,option, Category) %>%
          dplyr::summarise(release = median(Value))


        catch<- df2() %>%
          dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt) %>%
          dplyr::group_by(run_number, option, Category) %>%
          dplyr::summarise(Value = median(Value)) %>%
          dplyr::left_join(release) %>%
          tidyr::pivot_wider(names_from = Category, values_from = c(Value, release))

        p3<- catch %>% ggplot2::ggplot(aes(x = Value_cod, y = release_cod))+
          geom_point() +
          geom_vline( xintercept =cod_acl)+
          geom_text(aes(label=run_number, hjust=0, nudge_x=50))+
          ggtitle("Cod Releases")+
          ylab("Released Cod")+
          xlab("Total Cod Mortality (mt)")+
          theme(legend.position = "none")

        fig3<- ggplotly(p3)%>%
          plotly::style(textposition = "top")
        fig3
      })
    }
  })

      output$addReleaseHad <- renderUI({
        if(any("Releases" == input$fig)){


      renderPlotly({
        release <-  df2() %>%
          dplyr::filter(catch_disposition %in% c("release")) %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::group_by(run_number,option, Category) %>%
          dplyr::summarise(release = median(Value))


        catch<- df2() %>%
          dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt) %>%
          dplyr::group_by(run_number, option, Category) %>%
          dplyr::summarise(Value = median(Value)) %>%
          dplyr::left_join(release) %>%
          tidyr::pivot_wider(names_from = Category, values_from = c(Value, release))

        p4<- catch %>% ggplot2::ggplot(aes(x = Value_had, y = release_had))+
          geom_point() +
          geom_vline( xintercept = had_acl)+
          geom_text(aes(label=run_number), nudge_x = 0.25, nudge_y = 0.25, check_overlap = T)+
          ggtitle("Haddock Releases")+
          ylab("Released Haddock")+
          xlab("Total Haddock Mortality (mt)")+
          theme(legend.position = "none")

        fig4<- ggplotly(p4)%>%
          plotly::style(textposition = "top")
        fig4
    })
    }
  })


      output$addTripsCod <- renderUI({
        if(any("Trips" == input$fig)){

          renderPlotly({
            trips <-  df2() %>%
              dplyr::filter(Category %in% c("ntrips")) %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::group_by(run_number,option, Category) %>%
              dplyr::summarise(trips = median(Value))


            catch<- df2() %>%
              dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                            number_weight == "Weight") %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::mutate(Value = Value * lb_to_mt) %>%
              dplyr::group_by(run_number, option, Category) %>%
              dplyr::summarise(Value = median(Value)) %>%
              tidyr::pivot_wider(names_from = Category, values_from = Value) %>%
              dplyr::left_join(trips) %>%
              dplyr::select(!Category)


            p5<- catch %>% ggplot2::ggplot(aes(x = cod, y = trips))+
              geom_point() +
              #geom_vline( xintercept = had_acl)+
              geom_text(aes(label=run_number), nudge_x = 0.25, nudge_y = 0.25, check_overlap = T)+
              ggtitle("Cod - Total Number of Trips")+
              ylab("Number of Trips")+
              xlab("Total Cod Mortality (mt)")+
              theme(legend.position = "none")

            fig5<- ggplotly(p5)%>%
              plotly::style(textposition = "top")
            fig5

          })
        }
      })


      output$addTripsHad <- renderUI({
        if(any("Trips" == input$fig)){

          renderPlotly({
            trips <-  df2() %>%
              dplyr::filter(Category %in% c("ntrips")) %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::group_by(run_number,option, Category) %>%
              dplyr::summarise(trips = median(Value))


            catch<- df2() %>%
              dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                            number_weight == "Weight") %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::mutate(Value = Value * lb_to_mt) %>%
              dplyr::group_by(run_number, option, Category) %>%
              dplyr::summarise(Value = median(Value)) %>%
              tidyr::pivot_wider(names_from = Category, values_from = Value) %>%
              dplyr::left_join(trips) %>%
              dplyr::select(!Category)

            p6<- catch %>% ggplot2::ggplot(aes(x = had, y = trips))+
              geom_point() +
              #geom_vline( xintercept = had_acl)+
              geom_text(aes(label=run_number), nudge_x = 0.25, nudge_y = 0.25, check_overlap = T)+
              ggtitle("Haddock - Total Number of Trips")+
              ylab("Number of Trips")+
              xlab("Total Haddock Mortality (mt)")+
              theme(legend.position = "none")

            fig6<- ggplotly(p6)%>%
              plotly::style(textposition = "top")
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

    predictions_out <- read.csv(here::here("data-raw/sq_predictions_cm.csv")) %>%
      #dplyr::mutate(option = c("SQ")) %>%
      dplyr::select(!X) %>%
      #rbind(predictions_out10) %>%
      rbind(pred()) %>%
      dplyr::mutate(Value = dplyr::case_when(number_weight == "Weight" ~ Value/2205, TRUE ~ Value))
    return(predictions_out)
  })

  #### Regulations ####
  regs_agg <- reactive({

    print("start regs")
    SQ_regulations <- read.csv(here::here("data-raw/SQ_regulations.csv"))

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

    Regs1<- Regs %>% rbind(SQ_regulations)


    Regs_out <- Regs1 %>%
      #SQ_regulations %>%
      tidyr::separate(Var, into =c("Species", "mode", "Var"), sep = "_") %>%
      tidyr::pivot_wider(names_from = Var, values_from = Val) %>%
      dplyr::filter(!bag == 0) %>%
      #tidyr::pivot_wider(names_from = Species, values_from = c(bag, size, Season)) %>%
      dplyr::rename(Option = Opt,
                    Mode = mode,
                    `Bag Limit` = bag,
                    `Min Size (in)` = size) %>%
      tidyr::separate(Species, into = c("Species"), sep = "(?<=[A-Za-z])") %>%
      dplyr::mutate(Species = dplyr::recode(Species, "C" = "Cod", "H" = "Haddock"),
                    Mode = dplyr::recode(Mode, "FH" = "For Hire", "PR" = "Private"))

      # dplyr::mutate(had_bag = dplyr::case_when(bag_Had1 == bag_Had2 ~ paste0(bag_Had1), TRUE ~ paste0(bag_Had1, " , ", bag_Had2)),
      #               had_size = dplyr::case_when(size_Had1 == size_Had2 ~ paste0(size_Had1), TRUE ~ paste0(size_Had1, " , ", size_Had2)),
      #               had_Season = dplyr::case_when(Season_Had1 == Season_Had2 ~ paste0(Season_Had1), TRUE ~ paste0(Season_Had1, " , ", Season_Had2)))

      # dplyr::mutate(all_regs = paste0( bag, "_", size, "_", Season),
      #               bag_size = paste0( bag, "_", size),
      #               bag_season = paste0( bag,"_", Season),
      #               size_season = paste0( size, "_", Season)) %>%
      # dplyr::group_by(Species,Opt) %>%
      # dplyr::distinct(all_regs, .keep_all = TRUE) %>%
      # dplyr::mutate(mode = dplyr::case_when(length(Species) == 1 ~ "All", TRUE ~ mode)) %>%
      # dplyr::select(!all_regs) %>%
      # dplyr::mutate(Species = stringr::str_extract(Species, "[:alpha:]+"))

    print("regs out")
    return(Regs_out)
    })

  ##### Catch ###########
  which_catch_out<- reactiveVal(TRUE)
  catch_agg <- reactive({

    catch_agg<- predictions() %>%
      #predictions_out %>%
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

    print("start catch mode")
    catch_by_mode<- predictions() %>%
      #predictions_out %>%
      dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                    number_weight == "Weight") %>%
      dplyr::group_by(option, Category, draw_out, mode) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::mutate(under_acl = dplyr::case_when(Category == "cod" & Value <= 99000 ~ 1, TRUE ~ 0),
                    under_acl = dplyr::case_when(Category == "had" & Value <= 1405000 ~ 1, TRUE ~ under_acl)) %>%
      dplyr::group_by(option, Category, mode) %>%
      dplyr::summarise(under_acl = sum(under_acl),
                       Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = c(option), values_from = c(Value, under_acl)) %>%
      dplyr::select(Category, Value_SQ, Value_alt,  mode) %>%
      dplyr::rename(Species = Category, `SQ Catch Total Mortality (mt)` = Value_SQ,
                    `Alternative Total Mortality (mt)` = Value_alt)

    return(catch_by_mode)
  })

  #### keep release discards ####
  which_keep_out<- reactiveVal(TRUE)
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
  which_welfare_out<- reactiveVal(TRUE)
  welfare_agg <- reactive({

    welfare_agg<- predictions() %>%
      #predictions_out %>%
      dplyr::filter(Category %in% c("CV", "ntrips")) %>%
      dplyr::group_by(option, Category, draw_out) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::group_by(option, Category) %>%
      dplyr::summarise(Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = option, values_from = Value) %>%
      dplyr::select(!SQ) %>%
      dplyr::mutate(Category = dplyr::recode(Category, CV = "Change in Consumer Surplus ($)",
                                             ntrips = "Angler Trips (N)")) %>%
      tidyr::pivot_wider(names_from = Category, values_from = alt)

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
      dplyr::select(!SQ) %>%
      dplyr::mutate(Category = dplyr::recode(Category, CV = "Change in Consumer Surplus ($)",
                                             ntrips = "Angler Trips (N)")) %>%
      tidyr::pivot_wider(names_from = Category, values_from = alt)
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
    Run_Name = input$Run_Name
    readr::write_csv(dat_out, file = here::here(paste0("output/output_", Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))

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
