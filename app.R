

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
             #uiOutput("summary_regs_table"),
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
                       sliderInput(inputId = "CodFH_seas1", label ="For Hire Season 1",
                                   min = as.Date("2026-05-01","%Y-%m-%d"),
                                   max = as.Date("2027-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2026-05-01","%Y-%m-%d"),as.Date("2026-05-31","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "CodFH_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 1)),
                         column(5,
                                sliderInput(inputId = "CodFH_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 23, step = 1))),

                       sliderInput(inputId = "CodPR_seas1", label ="Private Season 1",
                                   min = as.Date("2026-05-01","%Y-%m-%d"),
                                   max = as.Date("2027-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2026-05-01","%Y-%m-%d"),as.Date("2026-05-31","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "CodPR_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 1)),
                         column(5,
                                sliderInput(inputId = "CodPR_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 23, step = 1))),


                       sliderInput(inputId = "CodFH_seas2", label ="For Hire Season 2",
                                   min = as.Date("2026-05-01","%Y-%m-%d"),
                                   max = as.Date("2027-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2026-09-01","%Y-%m-%d"),as.Date("2026-10-31","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "CodFH_2_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 1)),
                         column(5,
                                sliderInput(inputId = "CodFH_2_len", label = "Min Length",
                                            min = 15, max = 30, value = 23, step = 1))),
                       sliderInput(inputId = "CodPR_seas2", label ="Private Open Season 2",
                                   min = as.Date("2026-05-01","%Y-%m-%d"),
                                   max = as.Date("2027-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2026-09-01","%Y-%m-%d"),as.Date("2026-10-31","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "CodPR_2_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 1)),
                         column(5,
                                sliderInput(inputId = "CodPR_2_len", label = "Min Length",
                                            min = 15, max = 30, value = 23, step = 1))),



                       actionButton("CODaddSeason", "Add Season"),
                       shinyjs::hidden( div(ID = "CodSeason3",
                                            sliderInput(inputId = "CodFH_seas3", label ="For Hire Season 3",
                                                        min = as.Date("2026-05-01","%Y-%m-%d"),
                                                        max = as.Date("2027-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2027-01-01","%Y-%m-%d"),as.Date("2027-01-01","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "CodFH_3_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "CodFH_3_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 23, step = 1))),
                                            sliderInput(inputId = "CodPR_seas3", label ="Private Season 3",
                                                        min = as.Date("2026-05-01","%Y-%m-%d"),
                                                        max = as.Date("2027-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2027-01-01","%Y-%m-%d"),as.Date("2027-01-01","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "CodPR_3_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "CodPR_3_len", label ="Min Length",
                                                                 min = 15, max = 25, value = 23, step = 1)))))),
                column(6,
                       titlePanel("Haddock"),
                       sliderInput(inputId = "HadFH_seas1", label ="For Hire Season 1",
                                   min = as.Date("2026-05-01","%Y-%m-%d"),
                                   max = as.Date("2027-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2026-05-01","%Y-%m-%d"),as.Date("2027-02-28","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadFH_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadFH_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 17, step = 1))),
                       sliderInput(inputId = "HadPR_seas1", label ="Private Season 1",
                                   min = as.Date("2026-05-01","%Y-%m-%d"),
                                   max = as.Date("2027-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2026-05-01","%Y-%m-%d"),as.Date("2027-02-28","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadPR_1_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadPR_1_len", label = "Min Length",
                                            min = 15, max = 30, value = 17, step = 1))),

                       sliderInput(inputId = "HadFH_seas2", label ="For Hire Season 2",
                                   min = as.Date("2026-05-01","%Y-%m-%d"),
                                   max = as.Date("2027-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2027-04-01","%Y-%m-%d"),as.Date("2027-04-30","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadFH_2_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadFH_2_len", label = "Min Length",
                                            min = 15, max = 30, value = 17, step = 1))),
                       sliderInput(inputId = "HadPR_seas2", label ="Private Season 2",
                                   min = as.Date("2026-05-01","%Y-%m-%d"),
                                   max = as.Date("2027-04-30","%Y-%m-%d"),
                                   value =c(as.Date("2027-04-01","%Y-%m-%d"),as.Date("2027-04-30","%Y-%m-%d")),
                                   timeFormat = "%Y-%m-%d", ticks = FALSE),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "HadPR_2_bag", label = "Bag Limit",
                                             min = 0, max = 100, value = 15)),
                         column(5,
                                sliderInput(inputId = "HadPR_2_len", label = "Min Length",
                                            min = 15, max = 30, value = 17, step = 1))),

                       actionButton("HADaddSeason", "Add Season"),
                       shinyjs::hidden( div(ID = "HadSeason3",
                                            sliderInput(inputId = "HadFH_seas3", label ="For Hire Season 3",
                                                        min = as.Date("2026-05-01","%Y-%m-%d"),
                                                        max = as.Date("2027-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2027-01-01","%Y-%m-%d"),as.Date("2027-01-01","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "HadFH_3_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "HadFH_3_len", label ="Min Length",
                                                                 min = 15, max = 30, value = 17, step = 1))),
                                            sliderInput(inputId = "HadPR_seas3", label ="Private Season 3",
                                                        min = as.Date("2026-05-01","%Y-%m-%d"),
                                                        max = as.Date("2027-04-30","%Y-%m-%d"),
                                                        value=c(as.Date("2027-01-01","%Y-%m-%d"),as.Date("2027-01-01","%Y-%m-%d")),
                                                        timeFormat = "%Y-%m-%d", ticks = FALSE),
                                            fluidRow(
                                              column(4,
                                                     numericInput(inputId = "HadPR_3_bag", label ="Bag Limit",
                                                                  min = 0, max = 20, value = 0)),
                                              column(6,
                                                     sliderInput(inputId = "HadPR_3_len", label ="Min Length",
                                                                 min = 15, max = 30, value = 17, step = 1))))))))#,

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

  outputs <- function(){
    fnames <- list.files(path=here::here("output/"),pattern = "*.csv",full.names = T)

    fnames2<- as.data.frame(fnames) %>%
      tidyr::separate(fnames, into = c("a", "b"), sep = "_") %>%
      dplyr::mutate(b = ifelse(stringr::str_detect(b, "202501"),  "NA", b),
                    c=c(1:nrow(.)),
                    run_name = dplyr::case_when(b != "NA" ~ b, TRUE ~ as.character(c))) %>%
      dplyr::select(run_name)

    df <- fnames %>%
      purrr::map_df(~data.table::fread(.,stringsAsFactors=F,check.names=T,strip.white=T))

    return(df)
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

    print("get regs1")
    regs_data <- flist %>%
      purrr::map_dfr(readr::read_csv)
    print("get regs2")
    return(regs_data)
  }

  output$DTout <- DT::renderDT({
    catch_agg<- outputs() %>%
      #dat %>%
      dplyr::filter(metric %in% c("keep_weight", "discmort_weight"),
                    mode == "all modes") %>%
      dplyr::group_by(model, species,draw) %>%
      dplyr::summarise(Value = sum(as.numeric(value))) %>%
      dplyr::mutate(Value = Value * lb_to_mt()) %>%
      dplyr::mutate(under_acl = dplyr::case_when(species == "cod" & Value <= cod_acl() ~ 1, TRUE ~ 0),
                    under_acl = dplyr::case_when(species == "hadd" & Value <= had_acl() ~ 1, TRUE ~ under_acl)) %>%
      dplyr::group_by(model, species) %>%
      dplyr::summarise(under_acl = sum(under_acl),
                       Value = round(median(Value),0)) %>%
      tidyr::pivot_wider(names_from = species, values_from = c(Value, under_acl))

    # regs<- regs %>%

    regs1 <- regs() %>%
      dplyr::rename("model" = "run_name") %>%
      dplyr::left_join(catch_agg, by = c("model")) %>%
      tidyr::separate(input, into =c("Species", "season", "Var"), sep = "_") %>%
      tidyr::pivot_wider(names_from = Var, values_from = c(value)) %>%
      tidyr::separate(Species, into = c("Species", "mode"), sep = 3)


    seasons <- regs1 %>%
      dplyr::filter(!is.na(op), !is.na(cl)) %>%
      dplyr::mutate(season = paste(op, cl, sep = "-"),
                    Species = ifelse(grepl("cod", Species), "cod", "haddock")) %>%
      dplyr::group_by(model, mode, Species) %>%
      dplyr::summarise(season = paste(unique(season), collapse = ";"), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Species, values_from = season, names_glue = "{Species}season")

    bags <- regs1 %>%
      dplyr::filter(!is.na(bag)) %>%
      dplyr::mutate(Species = ifelse(grepl("cod", Species), "cod", "haddock")) %>%
      dplyr::group_by(model, mode, Species) %>%
      dplyr::summarise(baglimit = max(bag), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Species, values_from = baglimit, names_glue = "{Species}baglimit")

    minsizes <- regs1 %>%
      dplyr::filter(!is.na(len)) %>%
      dplyr::mutate(Species = ifelse(grepl("cod", Species), "cod", "haddock")) %>%
      dplyr::group_by(model, mode, Species) %>%
      dplyr::summarise(minsize = max(len), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Species,values_from = minsize,names_glue = "{Species}minsize")

    acl <- regs1 %>%
      dplyr::select(model, mode, under_acl_cod, under_acl_hadd) %>%
      dplyr::distinct()

    final_table <- seasons %>%
      dplyr::left_join(bags, by = c("model", "mode")) %>%
      dplyr::left_join(minsizes, by = c("model", "mode")) %>%
      dplyr::left_join(acl, by = c("model", "mode")) %>%
      dplyr::rename(Mode = mode,
                    `Run Identifier` = model,
                    `Cod Bag Limit` = codbaglimit,
                    `Cod Minimum Size (in)` = codminsize,
                    `Cod Season(s)` = codseason,
                    `Haddock Bag Limit` = haddockbaglimit,
                    `Haddock Minimum Size (in)` = haddockminsize,
                    `Haddock Season(s)` = haddockseason,
                    `% under Cod ACL` = under_acl_cod,
                    `% under Haddock ACL` = under_acl_hadd)


    DT::datatable(final_table)
  })

  output$totCatch <- plotly::renderPlotly({

    catch_agg<- outputs() %>%
      #dat %>%
      dplyr::filter(metric %in% c("keep_weight", "discmort_weight"),
                    mode == "all modes")%>%
      dplyr::group_by(model, species,draw) %>%
      dplyr::summarise(Value = sum(as.numeric(value))) %>%
      dplyr::mutate(Value = Value * lb_to_mt()) %>%
      dplyr::mutate(under_acl = dplyr::case_when(species == "cod" & Value <= cod_acl() ~ 1, TRUE ~ 0),
                    under_acl = dplyr::case_when(species == "had" & Value <= had_acl() ~ 1, TRUE ~ under_acl)) %>%
      dplyr::group_by(model, species) %>%
      dplyr::summarise(under_acl = sum(under_acl),
                       Value = round(median(Value),0)) %>%
      tidyr::pivot_wider(names_from = species, values_from = c(Value, under_acl))

    catch_agg2<- catch_agg %>%
      dplyr::mutate(under_acl_cod2 = dplyr::case_when(under_acl_cod < 50 ~ "Less than 50%", TRUE ~ ""),
                    under_acl_cod2 = dplyr::case_when(under_acl_cod >= 50 & under_acl_cod < 60 ~ "50-59%", TRUE ~ under_acl_cod2),
                    under_acl_cod2 = dplyr::case_when(under_acl_cod >= 60 & under_acl_cod < 70~ "60-69%", TRUE ~ under_acl_cod2),
                    under_acl_cod2 = dplyr::case_when(under_acl_cod >= 70 & under_acl_cod < 80 ~ "70-79%", TRUE ~ under_acl_cod2),
                    under_acl_cod2 = dplyr::case_when(under_acl_cod >= 80 & under_acl_cod < 90 ~ "80-89%", TRUE ~ under_acl_cod2),
                    under_acl_cod2 = dplyr::case_when(under_acl_cod >= 90 & under_acl_cod <=100 ~ "90-100%", TRUE ~ under_acl_cod2)) %>%
      dplyr::mutate(under_acl_had2 = dplyr::case_when(under_acl_hadd < 50 ~ "Less than 50%", TRUE ~ ""),
                    under_acl_had2 = dplyr::case_when(under_acl_hadd >= 50 & under_acl_hadd < 60 ~ "50-59%", TRUE ~ under_acl_had2),
                    under_acl_had2 = dplyr::case_when(under_acl_hadd >= 60 & under_acl_hadd < 70~ "60-69%", TRUE ~ under_acl_had2),
                    under_acl_had2 = dplyr::case_when(under_acl_hadd >= 70 & under_acl_hadd < 80 ~ "70-79%", TRUE ~ under_acl_had2),
                    under_acl_had2 = dplyr::case_when(under_acl_hadd >= 80 & under_acl_hadd < 90 ~ "80-89%", TRUE ~ under_acl_had2),
                    under_acl_had2 = dplyr::case_when(under_acl_hadd >= 90 & under_acl_hadd <=100 ~ "90-100%", TRUE ~ under_acl_had2)) %>%
      dplyr::rename(`Cod Mortality`=Value_cod) %>%
      dplyr::rename(`Haddock Mortality`=Value_hadd) %>%
      dplyr::ungroup()

    p<- catch_agg2 %>%
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
      ggplot2::geom_text(ggplot2::aes(label = model), check_overlap = TRUE)+
      ggplot2::geom_vline( xintercept =cod_acl(), linetype="dashed")+
      ggplot2::geom_hline( yintercept =had_acl(), color="grey45")+
      ggplot2::annotate(geom="text", x=cod_acl(), label="Cod ACL", y=1200) +
      ggplot2::annotate(geom="text", y=had_acl(), label="Had ACL", x=100) +
      ggplot2::guides(size = "none")+
      ggplot2::ggtitle("Cod and Haddock Mortality")+
      ggplot2::ylab("Median Recreational Haddock Mortality (mt)")+
      ggplot2::xlab("Median Recreational Cod Mortality (mt)")

    fig<- plotly::ggplotly(p) %>% #,
      plotly::style(textposition = "top center")
    fig
  })

  output$addCVCod <- renderUI({

    if(any("Angler Satisfaction" == input$fig)){

      plotly::renderPlotly({

        welfare <-  outputs() %>%
          dplyr::filter(metric == c("CV"),
                        mode == "all modes") %>%
          dplyr::group_by(model,  draw) %>%
          dplyr::summarise(value = sum(as.numeric(value))) %>%
          tidyr::pivot_wider(names_from = model, values_from = value) %>%
          tidyr::pivot_longer(-draw, names_to = "model", values_to = "value") %>%
          dplyr::group_by(draw) %>%
          dplyr::mutate(SQ_value = (value[model == "SQproposed"]),
                 pct_diff = 100 * (value - SQ_value) / SQ_value) %>%
          dplyr::ungroup()

        catch<- outputs() %>%
          #dat %>%
          dplyr::filter(metric %in% c("keep_weight", "discmort_weight"),
                        mode == "all modes")%>%
          dplyr::group_by(model, species,draw) %>%
          dplyr::summarise(Value = sum(as.numeric(value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(model, draw, species) %>%
          dplyr::summarise(Value =round(median(Value),0)) %>%
          tidyr::pivot_wider(names_from = species, values_from = Value) %>%
          dplyr::left_join(welfare) %>%
          dplyr::group_by(model) %>%
          dplyr::summarise(`Angler Satisfaction($)` = median(pct_diff),
                           cod = median(cod),
                           hadd = median(hadd))

        p1<- catch %>% ggplot2::ggplot(ggplot2::aes(x = `Angler Satisfaction($)`, y = cod))+
          ggplot2::geom_point() +
          ggplot2::geom_hline( yintercept =cod_acl())+
          ggplot2::geom_text(ggplot2::aes(label=model), check_overlap = TRUE)+
          ggplot2::geom_text(ggplot2::aes(y=cod_acl(), label="Cod ACL", x=0)) +
          ggplot2::xlab("Relative Change in Angler Satisfaction ($)")+
          ggplot2::ylab("Total Recreational Cod Mortality (mt)")+
          ggplot2::labs(title = "Cod Mortality (mt) compared to Angler Satisfaction (Compared to status-quo regulations, how much better- or worse-off are anglers, in dollars?)",
                        subtitle = "testing")+
          ggplot2::theme(legend.position = "none")

        fig1<- plotly::ggplotly(p1) %>%
          plotly::style(textposition = "top center")

        fig1
      })

    }
  })


  output$addCVHad <- renderUI({

    if(any("Angler Satisfaction" == input$fig)){

      plotly::renderPlotly({
        welfare <-  outputs() %>%
          dplyr::filter(metric == c("CV"),
                        mode == "all modes") %>%
          dplyr::group_by(model,  draw) %>%
          dplyr::summarise(value = sum(as.numeric(value))) %>%
          tidyr::pivot_wider(names_from = model, values_from = value) %>%
          tidyr::pivot_longer(-draw, names_to = "model", values_to = "value") %>%
          dplyr::group_by(draw) %>%
          dplyr::mutate(SQ_value = (value[model == "SQproposed"]),
                 pct_diff = 100 * (value - SQ_value) / SQ_value) %>%
          dplyr::ungroup()

        catch<- outputs() %>%
          #dat %>%
          dplyr::filter(metric %in% c("keep_weight", "discmort_weight"),
                        mode == "all modes")%>%
          dplyr::group_by(model, species,draw) %>%
          dplyr::summarise(Value = sum(as.numeric(value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(model, draw, species) %>%
          dplyr::summarise(Value =round(median(Value),0)) %>%
          tidyr::pivot_wider(names_from = species, values_from = Value) %>%
          dplyr::left_join(welfare) %>%
          dplyr::group_by(model) %>%
          dplyr::summarise(`Angler Satisfaction($)` = median(pct_diff),
                           cod = median(cod),
                           hadd = median(hadd))

        p2<- catch %>% ggplot2::ggplot(ggplot2::aes(x = `Angler Satisfaction($)`, y = hadd))+
          ggplot2::geom_point() +
          ggplot2::geom_hline( yintercept =had_acl())+
          ggplot2::geom_text(ggplot2::aes(label=model), check_overlap = TRUE)+
          ggplot2::xlab("Relative Change in Angler Satisfaction ($)")+
          ggplot2::ylab("Total Recreational Haddock Mortality (mt)")+
          ggplot2::geom_text(ggplot2::aes(x=had_acl(), label="Had ACL", y=1146)) +
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

        discmort <-  outputs() %>%
          dplyr::filter(metric == c("discmort_weight"),
                        mode == "all modes") %>%
          dplyr::group_by(model,  draw) %>%
          dplyr::mutate(disc_mort = value * lb_to_mt())  %>%
          dplyr::select(!c(metric,value))

        catch<- outputs() %>%
          #dat %>%
          dplyr::filter(metric %in% c("keep_weight", "discmort_weight"),
                        mode == "all modes")%>%
          dplyr::group_by(model, species,draw) %>%
          dplyr::summarise(Value = sum(as.numeric(value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(model, draw, species) %>%
          dplyr::summarise(tot_mort =round(median(Value),0)) %>%

          dplyr::left_join(discmort) %>%
          dplyr::group_by(model, draw, mode) %>%
          tidyr::pivot_wider(names_from = species, values_from = c(tot_mort, disc_mort)) %>%
          dplyr::group_by(model) %>%
          dplyr::summarise(`Cod Total Mortality` = median(tot_mort_cod),
                           `Haddock Total Mortality` = median(tot_mort_hadd),
                           `Cod Discard Mortality` = median(disc_mort_cod),
                           `Haddock Discard Mortality` = median(disc_mort_hadd))

        p3<- catch %>% ggplot2::ggplot(ggplot2::aes(x = `Cod Discard Mortality`, y = `Cod Total Mortality`))+
          ggplot2::geom_point() +
          ggplot2::geom_hline( yintercept =cod_acl())+
          ggplot2::geom_text(ggplot2::aes(label=model), check_overlap = TRUE)+
          ggplot2::geom_text(ggplot2::aes(y=cod_acl(), label="Cod ACL", x=35)) +
          ggplot2::xlab("Cod Discard Mortality (mt)")+
          ggplot2::ylab("Total Recreational Cod Mortality (mt)")+
          ggplot2::labs(title = "Total Cod Mortality (mt) compared to Discard Mortality(mt)",
                        subtitle = "")+
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
        discmort <-  outputs() %>%
          dplyr::filter(metric == c("discmort_weight"),
                        mode == "all modes") %>%
          dplyr::group_by(model,  draw) %>%
          dplyr::mutate(disc_mort = value * lb_to_mt())  %>%
          dplyr::select(!c(metric,value))

        catch<- outputs() %>%
          #dat %>%
          dplyr::filter(metric %in% c("keep_weight", "discmort_weight"),
                        mode == "all modes")%>%
          dplyr::group_by(model, species,draw) %>%
          dplyr::summarise(Value = sum(as.numeric(value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(model, draw, species) %>%
          dplyr::summarise(tot_mort =round(median(Value),0)) %>%

          dplyr::left_join(discmort) %>%
          dplyr::group_by(model, draw, mode) %>%
          tidyr::pivot_wider(names_from = species, values_from = c(tot_mort, disc_mort)) %>%
          dplyr::group_by(model) %>%
          dplyr::summarise(`Cod Total Mortality` = median(tot_mort_cod),
                           `Haddock Total Mortality` = median(tot_mort_hadd),
                           `Cod Discard Mortality` = median(disc_mort_cod),
                           `Haddock Discard Mortality` = median(disc_mort_hadd))

        p4<- catch %>% ggplot2::ggplot(ggplot2::aes(x = `Haddock Discard Mortality`, y = `Haddock Total Mortality`))+
          ggplot2::geom_point() +
          ggplot2::geom_hline( yintercept =had_acl())+
          ggplot2::geom_text(ggplot2::aes(label=model), check_overlap = TRUE)+
          ggplot2::geom_text(ggplot2::aes(y=had_acl(), label="Had ACL", x=290)) +
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

        trips <-  outputs() %>%
          dplyr::filter(metric == c("predicted_trips"),
                        mode == "all modes") %>%
          dplyr::group_by(model,  draw) %>%
          dplyr::summarise(value = sum(as.numeric(value))) %>%
          dplyr::ungroup()

        catch<- outputs() %>%
          #dat %>%
          dplyr::filter(metric %in% c("keep_weight", "discmort_weight"),
                        mode == "all modes")%>%
          dplyr::group_by(model, species,draw) %>%
          dplyr::summarise(Value = sum(as.numeric(value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(model, draw, species) %>%
          dplyr::summarise(Value =round(median(Value),0)) %>%
          tidyr::pivot_wider(names_from = species, values_from = Value) %>%
          dplyr::left_join(trips) %>%
          dplyr::group_by(model) %>%
          dplyr::summarise(Trips = median(value),
                           cod = median(cod),
                           hadd = median(hadd))

        p5<- catch %>% ggplot2::ggplot(ggplot2::aes(x = Trips, y = `cod`))+
          ggplot2::geom_point() +
          ggplot2::geom_hline( yintercept = cod_acl())+
          ggplot2::geom_text(ggplot2::aes(label=model), check_overlap = TRUE)+
          ggplot2::geom_text(ggplot2::aes(y=cod_acl(), label="Cod ACL", x=220000), angle=90) +
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
        trips <- outputs() %>%
          dplyr::filter(metric == c("predicted_trips"),
                        mode == "all modes") %>%
          dplyr::group_by(model,  draw) %>%
          dplyr::summarise(value = sum(as.numeric(value))) %>%
          dplyr::ungroup()

        catch<- outputs() %>%
          #dat %>%
          dplyr::filter(metric %in% c("keep_weight", "discmort_weight"),
                        mode == "all modes") %>%
          dplyr::group_by(model, species,draw) %>%
          dplyr::summarise(Value = sum(as.numeric(value))) %>%
          dplyr::mutate(Value = Value * lb_to_mt()) %>%
          dplyr::group_by(model, draw, species) %>%
          dplyr::summarise(Value =round(median(Value),0)) %>%
          tidyr::pivot_wider(names_from = species, values_from = Value) %>%
          dplyr::left_join(trips) %>%
          dplyr::group_by(model) %>%
          dplyr::summarise(Trips = median(value),
                           cod = median(cod),
                           hadd = median(hadd))


        p6<- catch %>% ggplot2::ggplot(ggplot2::aes(x =Trips , y = hadd))+
          ggplot2::geom_point() +
          ggplot2::geom_hline( yintercept = had_acl())+
          ggplot2::geom_text(ggplot2::aes(label=model), check_overlap = TRUE)+
          ggplot2::geom_text(ggplot2::aes(y=had_acl(), label="Had ACL", x=220000)) +
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
                   shinyjs::toggle(id = "CodSeason3", anim = TRUE))
  shinyjs::onclick("HADaddSeason",
                   shinyjs::toggle(id = "HadSeason3", anim = TRUE))

  #### Regulations ####
  regulations <- observeEvent(input$runmeplease,{
    library(httr)
    library(jsonlite)
    library(openssl)
    library(uuid)

    enqueue_simple_sas <- function(run_name, queue_url_sas = Sys.getenv("GROUNDFISH_AZURE_STORAGE_QUEUE_URL")) {
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
    codregs <- data.frame(run_name = c(Run_Name()),
                          input =  c("codFH_seas1_op", "codFH_seas1_cl", "codPR_seas1_op", "codPR_seas1_cl",
                                     "codFH_seas2_op", "codFH_seas2_cl", "codPR_seas2_op", "codPR_seas2_cl",
                                     "codFH_seas3_op", "codFH_seas3_cl", "codPR_seas3_op", "codPR_seas3_cl",

                                     "codFH_1_bag",   "codFH_1_len",    "codPR_1_bag",   "codPR_1_len",
                                     "codFH_2_bag" ,  "codFH_2_len",    "codPR_2_bag",   "codPR_2_len",
                                     "codFH_3_bag",   "codFH_3_len",    "codPR_3_bag",   "codPR_3_len"),
                          value =  c(as.character(input$CodFH_seas1[1]), as.character(input$CodFH_seas1[2]),
                                     as.character(input$CodFH_seas2[1]), as.character(input$CodFH_seas2[2]),
                                     as.character(input$CodFH_seas3[1]), as.character(input$CodFH_seas3[2]),

                                     as.character(input$CodPR_seas1[1]), as.character(input$CodPR_seas1[2]),
                                     as.character(input$CodPR_seas2[1]), as.character(input$CodPR_seas2[2]),
                                     as.character(input$CodPR_seas3[1]), as.character(input$CodPR_seas3[2]),

                                     as.character(input$CodFH_1_bag), as.character(input$CodPR_1_bag),
                                     as.character(input$CodFH_2_bag), as.character(input$CodPR_2_bag),
                                     as.character(input$CodFH_3_bag), as.character(input$CodPR_3_bag),

                                     as.character(input$CodFH_1_len), as.character(input$CodPR_1_len),
                                     as.character(input$CodFH_2_len), as.character(input$CodPR_2_len),
                                     as.character(input$CodFH_3_len), as.character(input$CodPR_3_len)))


    hadregs <- data.frame(run_name = c(Run_Name()),
                          input =  c("hadFH_seas1_op", "hadFH_seas1_cl", "hadPR_seas1_op", "hadPR_seas1_cl",
                                     "hadFH_seas2_op", "hadFH_seas2_cl", "hadPR_seas2_op", "hadPR_seas2_cl",
                                     "hadFH_seas3_op", "hadFH_seas3_cl", "hadPR_seas3_op", "hadPR_seas3_cl",

                                     "hadFH_1_bag",   "hadFH_1_len",    "hadPR_1_bag",   "hadPR_1_len",
                                     "hadFH_2_bag" ,  "hadFH_2_len",    "hadPR_2_bag",   "hadPR_2_len",
                                     "hadFH_3_bag",   "hadFH_3_len",    "hadPR_3_bag",   "hadPR_3_len"),


                          value =  c(as.character(input$HadFH_seas1[1]), as.character(input$HadFH_seas1[2]),
                                     as.character(input$HadFH_seas2[1]), as.character(input$HadFH_seas2[2]),
                                     as.character(input$HadFH_seas3[1]), as.character(input$HadFH_seas3[2]),

                                     as.character(input$HadPR_seas1[1]), as.character(input$HadPR_seas1[2]),
                                     as.character(input$HadPR_seas2[1]), as.character(input$HadPR_seas2[2]),
                                     as.character(input$HadPR_seas3[1]), as.character(input$HadPR_seas3[2]),

                                     as.character(input$HadFH_1_bag), as.character(input$HadPR_1_bag),
                                     as.character(input$HadFH_2_bag), as.character(input$HadPR_2_bag),
                                     as.character(input$HadFH_3_bag), as.character(input$HadPR_3_bag),

                                     as.character(input$HadFH_1_len), as.character(input$HadPR_1_len),
                                     as.character(input$HadFH_2_len), as.character(input$HadPR_2_len),
                                     as.character(input$HadFH_3_len), as.character(input$HadPR_3_len)))


    regulations <- regulations %>% rbind(codregs, hadregs)

    readr::write_csv(regulations, file = here::here(paste0("saved_regs/regs_", input$Run_Name, ".csv")))
    print("saved_inputs")

    enqueue_simple_sas(input$Run_Name)

    return(regulations)

  })

  observeEvent(input$runmeplease, {
    output$message <- renderText("Regulations saved - your model run has been queued. Results will appear in the output folder once processing completes. Be sure to change the run name before submitting another job.")
  })


  ###Output Tables
  output$regtableout <- renderTable({
    regs()
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


  # output$downloadData <- downloadHandler(
  #   filename = function(){"RecDSToutput.xlsx"},
  #   content = function(filename) {
  #     df_list <- list(Regulations=regs_agg(), Catch_Mortality_aggregated = catch_agg(), Catch_Mortality_by_mode = catch_by_mode(),
  #                     Keep_Release_aggregated = keep_agg(), Keep_Release_by_mode = keep_by_mode(),
  #                     Satisfaction_trips_aggregated = welfare_agg(), Satisfaction_trips_by_mode = welfare_by_mode())
  #     openxlsx::write.xlsx(append = TRUE, x = df_list , file = filename, row.names = FALSE)
  #   })

}
shiny::shinyApp(ui = ui, server = server)
