

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
             p("The results of your own model runs are stored. Please give it a short, unique name."),
             p("Hover over each point to view the detailed statistics for each model run. The first section contains
               a table of recreational management measures. The second section contains graphs of mortality. The third
               section has graphs of other performance measures, including Economic Surplus, Trips, and Discards."),


             p("The first figure plots the predicted median Cod and Haddock recreational mortality for previously simulated management measures."),

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
                                                        value=c(as.Date("2026-04-30","%Y-%m-%d"),as.Date("2026-04-30","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "CodFH_2_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "CodFH_2_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 15, step = 1))),
                                            sliderInput(inputId = "CodPR_seas2", label ="Private Open Season 2",
                                                        min = as.Date("2025-05-01","%Y-%m-%d"),
                                                        max = as.Date("2026-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2026-04-30","%Y-%m-%d"),as.Date("2026-04-30","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "CodPR_2_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "CodPR_2_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 15, step = 1)))))),
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
                                                        value=c(as.Date("2026-04-30","%Y-%m-%d"),as.Date("2026-04-30","%Y-%m-%d")),
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
                                                        value=c(as.Date("2026-04-30","%Y-%m-%d"),as.Date("2026-04-30","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "HadPR_3_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "HadPR_3_len", label ="Min Length",
                                                                 min = 15, max = 30, value = 18, step = 1)))))))),


    #### Results ####
    tabPanel("Results",
             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                              tags$div("Calculating...This will take ~45-50 min to run a set of regulations. Please do not log out or close your browser window.",id="loadmessage")), #Warning for users

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

    df <- fnames %>%
      purrr::map_df(~data.table::fread(.,stringsAsFactors=F,check.names=T,strip.white=T))


    df2<- df %>% dplyr::mutate(run_number = as.character(rep(fnames2$run_name, each = 6030)))
    return(df2)
  }

  cod_acl <- function(){
    cod_acl = 99
    return(cod_acl)
  }

  had_acl <- function(){
    had_acl = 1075
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


  output$DTout <- DT::renderDT({

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
      dplyr::mutate(Diff_from_SQ = dplyr::case_when(Value < SQ ~ "Shorter Season", TRUE ~ ""),
                    Diff_from_SQ = dplyr::case_when(Value > SQ ~ "Longer Season", TRUE ~ Diff_from_SQ),
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
      dplyr::mutate(cod_season = stringr::str_remove_all(cod_season, "202.-"),
                    had_season = stringr::str_remove_all(had_season, "202.-")) %>%
      dplyr::mutate(mode = dplyr::recode(mode, "FH" = "For Hire",
                    "PR" = "Private")) %>%
      dplyr::select(run_number, mode, cod_bag, cod_size, cod_season, Diff_from_SQ_cod,
                    had_bag, had_size, had_season, Diff_from_SQ_had) %>%
      dplyr::rename(Mode = mode,
                    `Run Identifier` = run_number,
                    `Cod Bag Limit` = cod_bag,
                    `Cod Minimum Size (in)` = cod_size,
                    `Cod Season(s)` = cod_season,
                    `Haddock Bag Limit` = had_bag,
                    `Haddock Minimum Size (in)` = had_size,
                    `Haddock Season(s)` = had_season,
                    `Difference from Cod SQ` = Diff_from_SQ_cod,
                    `Difference from haddock SQ` = Diff_from_SQ_had)


    DT::datatable(Regs_out)
  })

  output$totCatch <- plotly::renderPlotly({

    catch_agg<- df2() %>%
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
      tidyr::pivot_wider(names_from = Category, values_from = c(Value, under_acl))%>%
      dplyr::rename(`Cod Mortality`=Value_cod) %>%
      dplyr::rename(`Haddock Mortality`=Value_had)

    under_50 <- ifelse(as.numeric(catch_agg$under_acl_cod) < 50, "solid_color", "gradient")

    # catch_agg <- data.frame(run_number = c("SQ", "test"),
    #                         Cod_Mortality = c(43, 130),
    #                         Haddock_Mortality = c(810, 1099),
    #                         under_acl_cod = c(100, 40),
    #                         under_acl_had = c(94, 60)) %>%
    #   dplyr::rename(`Cod Mortality`=Cod_Mortality, `Haddock Mortality`=Haddock_Mortality)

    p<- catch_agg %>%
      dplyr::mutate(under_acl_cod = as.numeric(under_acl_cod),
                    under_acl_had = as.numeric(under_acl_had)) %>%
      ggplot2::ggplot(ggplot2::aes(x = `Cod Mortality`, y = `Haddock Mortality`, label = run_number,text = paste0('under_acl_had: ', under_acl_cod, '%', '</br>under_acl_cod: ', under_acl_had, '%')   )  )+
      #geom_point(aes(label = run_number, colour = test)) +
      #ggplot2::geom_point(ggplot2::aes(color = "red3"))+
      ggplot2::geom_point(ggplot2::aes(colour = under_acl_cod)) +
      ggplot2::scale_colour_gradient2("% Under Cod ACL", low = "white", high = "darkgreen", limits=c(50,100)) +
      #ggplot2::scale_fill_manual(values = c("solid_color" = "red3", "gradient" = "transparent")) +
      #ggrepel::geom_text_repel(ggplot2::aes(`Cod Mortality`, `Haddock Mortality`, label = run_number))+
      #geom_text(aes(label = run_number, y = `Haddock Mortality` + 0.25))+
      ggplot2::geom_text(ggplot2::aes(label=run_number), position=ggplot2::position_jitter(width=1,height=1), check_overlap = TRUE)+
      #geom_text(aes(label=ifelse(`Cod Mortality`>cod_acl() & `Haddock Mortality` > had_acl(), as.character(run_number), ' '), hjust=1, vjust=1))+
      ggplot2::geom_vline( xintercept =cod_acl(), linetype="dashed")+
      ggplot2::geom_hline( yintercept =had_acl(), color="grey45")+
      ggplot2::geom_text(ggplot2::aes(x=99, label="Cod ACL", y=1200), angle=90) +
      ggplot2::geom_text(ggplot2::aes(x=80, label="Haddock ACL", y=1075))+

      #ggplot2::scale_colour_gradient(low = "white", high = "darkgreen")+
      ggplot2::ggtitle("Cod and Haddock Mortality")+
      ggplot2::ylab("Median Recreational Haddock Mortality (mt)")+
      ggplot2::xlab("Median Recreational Cod Mortality (mt)")

    fig<- plotly::ggplotly(p,
                           tooltip = c("x", "y", "text")) %>%
      plotly::style(textposition = "top center")#,
    #jitter = 0.4) #%>%
    #plotly::style(textposition = "top")
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
          dplyr::summarise(median_cv = median(Value_diff))

        catch<- df2() %>%
          dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(run_number, option, Category) %>%
          dplyr::summarise(Value =round(median(Value),0)) %>%
          tidyr::pivot_wider(names_from = Category, values_from = Value) %>%
          dplyr::left_join(welfare)

        p1<- catch %>% ggplot2::ggplot(ggplot2::aes(x = median_cv, y = cod))+
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
          dplyr::summarise(median_cv = median(Value_diff))

        catch<- df2() %>%
          dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(run_number, option, Category) %>%
          dplyr::summarise(Value =round(median(Value),0)) %>%
          tidyr::pivot_wider(names_from = Category, values_from = Value) %>%
          dplyr::left_join(welfare)

        p2<- catch %>% ggplot2::ggplot(ggplot2::aes(x = median_cv, y = had))+
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
          dplyr::filter(catch_disposition %in% c("release"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(run_number,option, Category) %>%
          dplyr::summarise(release = round(median(Value),0))


        catch<- df2() %>%
          dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                        number_weight == "Weight") %>%
          dplyr::group_by(run_number, option, Category, draw_out) %>%
          dplyr::summarise(Value = sum(as.numeric(Value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(run_number, option, Category) %>%
          dplyr::summarise(Value =round(median(Value),0)) %>%
          dplyr::left_join(release) %>%
          tidyr::pivot_wider(names_from = Category, values_from = c(Value, release))%>%
          dplyr::rename(`Cod Mortality`=Value_cod) %>%
          dplyr::rename(`Haddock Mortality`=Value_had)%>%
          dplyr::rename(`Cod Release`=release_cod) %>%
          dplyr::rename(`Haddock Release`=release_had)

        p3<- catch %>% ggplot2::ggplot(ggplot2::aes(x = `Cod Release`, y = `Cod Mortality`))+
          ggplot2::geom_point() +
          ggplot2::geom_hline( yintercept =cod_acl())+
          ggplot2::geom_text(ggplot2::aes(label=run_number), check_overlap = TRUE)+
          ggplot2::geom_text(ggplot2::aes(y=cod_acl(), label="Cod ACL", x=240)) +
          ggplot2::xlab("Cod Discards (mt)")+
          ggplot2::ylab("Total Recreational Cod Mortality (mt)")+
          ggplot2::labs(title = "Cod Mortality (mt) compared to Total Discarded Cod (mt)",
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
              dplyr::filter(catch_disposition %in% c("release"),
                            number_weight == "Weight") %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::mutate(Value = Value * lb_to_mt()) %>%
              dplyr::group_by(run_number,option, Category) %>%
              dplyr::summarise(release = round(median(Value),0))


            catch<- df2() %>%
              dplyr::filter(catch_disposition %in% c("keep", "Discmortality"),
                            number_weight == "Weight") %>%
              dplyr::group_by(run_number, option, Category, draw_out) %>%
              dplyr::summarise(Value = sum(as.numeric(Value))) %>%
              dplyr::mutate(Value = Value * lb_to_mt()) %>%
              dplyr::group_by(run_number, option, Category) %>%
              dplyr::summarise(Value = round(median(Value),0)) %>%
              dplyr::left_join(release) %>%
              tidyr::pivot_wider(names_from = Category, values_from = c(Value, release))%>%
              dplyr::rename(`Cod Mortality`=Value_cod) %>%
              dplyr::rename(`Haddock Mortality`=Value_had)%>%
              dplyr::rename(`Cod Release`=release_cod) %>%
              dplyr::rename(`Haddock Release`=release_had)


            p4<- catch %>% ggplot2::ggplot(ggplot2::aes(x = `Haddock Release` , y = `Haddock Mortality`))+
              ggplot2::geom_point() +
              ggplot2::geom_hline( yintercept = had_acl())+
              ggplot2::geom_text(ggplot2::aes(label=run_number), check_overlap = TRUE)+
              ggplot2::geom_text(ggplot2::aes(y=had_acl(), label="Had ACL", x=600)) +
              ggplot2::xlab("Haddock Discards (mt)")+
              ggplot2::ylab("Total Recreational Haddock Mortality (mt)")+
              ggplot2::labs(title = "Haddock Mortality (mt) compared to Total Discarded Haddock (mt)",
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

    predictions_out <- read.csv(here::here("data-raw/sq_predictions_cm.csv")) %>%
      #dplyr::mutate(option = c("SQ")) %>%
      #dplyr::select(!X) %>%
      #rbind(test) %>%
      rbind(pred()) %>%
      dplyr::mutate(Value = dplyr::case_when(number_weight == "Weight" ~ Value/2205, TRUE ~ Value))
    return(predictions_out)
  })

  #### Regulations ####
  regs_agg <- reactive({

    Run_Name = if(Run_Name() != ""){
      Run_Name = Run_Name()
    }else {
      Run_Name = "alt"
    }

    print("start regs")
    SQ_regulations <- read.csv(here::here("data-raw/SQ_regulations.csv"))
    #Regs <- read.csv(here::here("data-raw/SQ_regulations.csv")) %>%
    #  mutate(Opt = c("alt"))

    Regs<- data.frame(Opt = c(Run_Name),
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
      dplyr::mutate(Season = dplyr::case_when(bag == 0 ~"NA", TRUE ~ Season),
                    size = dplyr::case_when(bag == 0 ~"NA", TRUE ~ size),
                    bag = dplyr::case_when(bag == 0 ~"NA", TRUE ~ bag)) %>%
      #dplyr::filter(!bag == 0) %>%
      tidyr::pivot_wider(names_from = Species, values_from = c(bag, size, Season)) %>%
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
      dplyr::select(Opt, mode, cod_bag, cod_size, cod_season, had_bag, had_size, had_season) %>%
      dplyr::mutate(cod_season = stringr::str_remove_all(cod_season, "202.-"),
                    had_season = stringr::str_remove_all(had_season, "202.-")) %>%
      dplyr::mutate(mode = dplyr::recode(mode, "FH" = "For Hire",
                                         "PR" = "Private")) %>%
      dplyr::rename(Mode = mode,
                    `Option` = Opt,
                    `Cod Bag Limit` = cod_bag,
                    `Cod Minimum Size (in)` = cod_size,
                    `Cod Season(s)` = cod_season,
                    `Haddock Bag Limit` = had_bag,
                    `Haddock Minimum Size (in)` = had_size,
                    `Haddock Season(s)` = had_season)

    print("regs out")
    return(Regs_out)
    })

  ##### Catch ###########
  which_catch_out<- reactiveVal(TRUE)
  catch_agg <- reactive({

    catch_agg<- predictions() %>%
      #predictions_out10 %>%
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
      dplyr::select(Category, Value_SQ, Value_alt, under_acl_alt) %>%
      dplyr::rename(Species = Category, `SQ Catch Total Mortality (mt)` = Value_SQ,
                    `Alternative Total Mortality (mt)` = Value_alt, `Atlernative % Under ACL (Out of 100 runs)` = under_acl_alt)

    return(catch_agg)
  })

  catch_by_mode <- reactive({

    print("start catch mode")
    catch_by_mode<- predictions() %>%
      #test %>%
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
      dplyr::mutate(Category = dplyr::recode(Category, "cod" = "Cod",
                                             "had" = "Haddock"),
                    mode = dplyr::recode(mode, "fh" = "For Hire",
                                         "pr" = "Private")) %>%
      dplyr::select(Category, Value_SQ, Value_alt,  mode) %>%
      dplyr::rename(Species = Category, `SQ Catch Total Mortality (mt)` = Value_SQ,
                    `Alternative Total Mortality (mt)` = Value_alt, `Mode` = mode)

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
      dplyr::mutate(Category = dplyr::recode(Category, "cod" = "Cod",
                                             "had" = "Haddock"),
                    catch_disposition = dplyr::recode(catch_disposition, "keep" = "Harvest",
                                                      "Discmortality" = "Dead Discards",
                                                      "release" = "Discards")) %>%
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
      dplyr::mutate(Category = dplyr::recode(Category, "cod" = "Cod",
                                             "had" = "Haddock"),
                    catch_disposition = dplyr::recode(catch_disposition, "keep" = "Harvest",
                                                      "Discmortality" = "Dead Discards",
                                                      "release" = "Discards"),
                    mode = dplyr::recode(mode, "fh" = "For Hire",
                                         "pr" = "Private")) %>%
      dplyr::select(Category, catch_disposition, mode, alt_Number, perc_diff_num, alt_Weight, perc_diff_wt) %>%
      dplyr::rename(Species = Category, Variable = catch_disposition,
                    `Total fish (N)` = alt_Number, `Percent difference in number of fish` = perc_diff_num,
                    `Total Weight (mt)` = alt_Weight, `Percent difference in weight of fish` = perc_diff_wt, `Mode` = mode)
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
      dplyr::mutate(Category = dplyr::recode(Category, CV = "Change in Angler Satisfaction ($)",
                                             ntrips = "Angler Trips (N)")) %>%
      tidyr::pivot_wider(names_from = Category, values_from = alt)

    return(welfare_agg)

  })


  welfare_by_mode <- reactive({
    welfare_by_mode<- predictions() %>%
      #predictions_out10 %>%
      dplyr::filter(Category %in% c("CV", "ntrips")) %>%
      dplyr::group_by(option, Category, draw_out, mode) %>%
      dplyr::summarise(Value = sum(Value)) %>%
      dplyr::group_by(option, Category, mode) %>%
      dplyr::summarise(Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = option, values_from = Value) %>%
      dplyr::select(!SQ) %>%
      dplyr::mutate(Category = dplyr::recode(Category, CV = "Change in Angler Satisfaction ($)",
                                             ntrips = "Angler Trips (N)"),
                    mode = dplyr::recode(mode, "fh" = "For Hire",
                                         "pr" = "Private")) %>%
      tidyr::pivot_wider(names_from = Category, values_from = alt) %>%
      dplyr::rename(`Mode` = mode)
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
      df_list <- list(Regulations=regulations(), Catch_Mortality_aggregated = catch_agg(), Catch_Mortality_by_mode = catch_by_mode(),
                      Keep_Release_aggregated = keep_agg(), Keep_Release_by_mode = keep_by_mode(),
                      Satisfaction_trips_aggregated = welfare_agg(), Satisfaction_trips_by_mode = welfare_mode())
      openxlsx::write.xlsx(x = df_list , file = filename, row.names = FALSE)
    })

}
shiny::shinyApp(ui = ui, server = server)
