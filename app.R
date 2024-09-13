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
    tabPanel("Results",

             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                              tags$div("Calculating...This will take ~15-20 min per state selected.",id="loadmessage")), #Warning for users

             downloadButton(outputId = "downloadData", "Download"),
             # Add table outputs
             ## KB - Make tables DTs - should fix RMD documentation issue
             tableOutput(outputId = "regtableout"),
             tableOutput(outputId = "welfare_tableout"),
             tableOutput(outputId = "keep_tableout"),
             tableOutput(outputId = "releaseout"),
             tableOutput(outputId = "ntrips_tableout"),
             plotOutput(outputId = "fig")),

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

  #source(here::here(paste0("model_run.R")), local = TRUE)
  #predictions_1 <- predictions_1 %>% rbind(predictions)

  #### Regulations ####
  regulations <- eventReactive(input$runmeplease,{

    sq <- read.csv(here::here("output_test.csv")) %>%
      dplyr::mutate(season = as.character(season))


    dat <- NULL
    #### regs ####

    # ###### testing ##################################################################################################################################################################
    # CodFH_1_bag <- 1
    # CodFH_2_bag <- 3
    # CodFH_1_len <- 22
    # CodFH_2_len <- 23
    # CodPR_1_bag <- 1
    # CodPR_2_bag <- 3
    # CodPR_1_len <- 22
    # CodPR_2_len <- 23
    # CodFH_seas1 <- c("2023-09-01", "2023-10-31")
    # CodFH_seas2 <- c("2024-01-01", "2024-03-15")
    # CodPR_seas1 <- c("2023-09-01", "2023-10-31")
    # CodPR_seas2 <- c("2024-01-01", "2024-03-15")
    #
    # cod_alt_FH_1 <- data.frame(Option = c("alt"), season = c("1"), Mode = c("For Hire"), Cod_Limit = c(CodFH_1_bag), Cod_Size = c(CodFH_1_len),
    #                            Cod_open = c(paste(CodFH_seas1[1], "-", CodFH_seas1[2])), Cod_mortality_mt = "Value", Angler_trips = "Value", Cod_per_under = "Value")
    # cod_alt_PR_1 <- data.frame(Option = c("alt"), season = c("1"), Mode = c("Private"), Cod_Limit = c(CodPR_1_bag), Cod_Size = c(CodPR_1_len),
    #                            Cod_open = c(paste(CodPR_seas1[1], "-", CodPR_seas1[2])), Cod_mortality_mt = "Value", Angler_trips = "Value", Cod_per_under = "Value")
    # cod_alt_FH_2 <- data.frame(Option = c("alt"), season = c("2"), Mode = c("For Hire"), Cod_Limit = c(CodFH_2_bag), Cod_Size = c(CodFH_2_len),
    #                            Cod_open = c(paste(CodFH_seas2[1], "-", CodFH_seas2[2])), Cod_mortality_mt = "Value", Angler_trips = "Value", Cod_per_under = "Value")
    # cod_alt_PR_2 <- data.frame(Option = c("alt"), season = c("2"), Mode = c("Private"), Cod_Limit = c(CodPR_2_bag), Cod_Size = c(CodPR_2_len),
    #                            Cod_open = c(paste(CodPR_seas2[1], "-", CodPR_seas2[2])), Cod_mortality_mt = "Value", Angler_trips = "Value", Cod_per_under = "Value")
    #
    # HadFH_1_bag <- 1
    # HadFH_2_bag <- 3
    # HadFH_3_bag <- 5
    # HadFH_1_len <- 22
    # HadFH_2_len <- 23
    # HadFH_3_len <- 24
    # HadPR_1_bag <- 1
    # HadPR_2_bag <- 3
    # HadPR_3_bag <- 5
    # HadPR_1_len <- 22
    # HadPR_2_len <- 23
    # HadPR_3_len <- 24
    # HadFH_seas1 <- c("2023-05-01", "2023-02-28")
    # HadFH_seas2 <- c("2024-04-01", "2024-04-14")
    # HadFH_seas3 <- c("2024-04-15", "2024-04-29")
    # HadPR_seas1 <- c("2023-05-01", "2023-02-31")
    # HadPR_seas2 <- c("2024-04-01", "2024-04-14")
    # HadPR_seas3 <- c("2024-04-15", "2024-04-29")
    #
    #
    #
    # had_alt_FH_1 <- data.frame(Option = c("alt"), season = c("1"), Mode = c("For Hire"), Had_Limit = c(HadFH_1_bag), Had_Size = c(HadFH_1_len),
    #                            Had_open = c(paste(HadFH_seas1[1], "-", HadFH_seas1[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")
    # had_alt_PR_1 <- data.frame(Option = c("alt"), season = c("1"), Mode = c("Private"), Had_Limit = c(HadPR_1_bag), Had_Size = c(HadPR_1_len),
    #                            Had_open = c(paste(HadPR_seas1[1], "-", HadPR_seas1[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")
    # had_alt_FH_2 <- data.frame(Option = c("alt"), season = c("2"), Mode = c("For Hire"), Had_Limit = c(HadFH_2_bag), Had_Size = c(HadFH_2_len),
    #                            Had_open = c(paste(HadFH_seas2[1], "-", HadFH_seas2[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")
    # had_alt_PR_2 <- data.frame(Option = c("alt"), season = c("2"), Mode = c("Private"), Had_Limit = c(HadPR_2_bag), Had_Size = c(HadPR_2_len),
    #                            Had_open = c(paste(HadPR_seas2[1], "-", HadPR_seas2[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")
    # had_alt_FH_3 <- data.frame(Option = c("alt"), season = c("3"), Mode = c("For Hire"), Had_Limit = c(HadFH_3_bag), Had_Size = c(HadFH_3_len),
    #                            Had_open = c(paste(HadFH_seas3[1], "-", HadFH_seas3[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")
    # had_alt_PR_3 <- data.frame(Option = c("alt"), season = c("3"), Mode = c("Private"), Had_Limit = c(HadPR_3_bag), Had_Size = c(HadPR_3_len),
    #                            Had_open = c(paste(HadPR_seas3[1], "-", HadPR_seas3[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")
    #
    #################################################################################################################################################################################




    cod_alt_FH_1 <- data.frame(Option = c("alt"), season = c("1"), Mode = c("For Hire"), Cod_Limit = c(input$CodFH_1_bag), Cod_Size = c(input$CodFH_1_len),
                            Cod_open = c(paste(input$CodFH_seas1[1], "-", input$CodFH_seas1[2])), Cod_mortality_mt = "Value", Angler_trips = "Value", Cod_per_under = "Value")
    cod_alt_PR_1 <- data.frame(Option = c("alt"), season = c("1"), Mode = c("Private"), Cod_Limit = c(input$CodPR_1_bag), Cod_Size = c(input$CodPR_1_len),
                               Cod_open = c(paste(input$CodPR_seas1[1], "-", input$CodPR_seas1[2])), Cod_mortality_mt = "Value", Angler_trips = "Value", Cod_per_under = "Value")
    cod_alt_FH_2 <- data.frame(Option = c("alt"), season = c("2"), Mode = c("For Hire"), Cod_Limit = c(input$CodFH_2_bag), Cod_Size = c(input$CodFH_2_len),
                               Cod_open = c(paste(input$CodFH_seas2[1], "-", input$CodFH_seas2[2])), Cod_mortality_mt = "Value", Angler_trips = "Value", Cod_per_under = "Value")
    cod_alt_PR_2 <- data.frame(Option = c("alt"), season = c("2"), Mode = c("Private"), Cod_Limit = c(input$CodPR_2_bag), Cod_Size = c(input$CodPR_2_len),
                               Cod_open = c(paste(input$CodPR_seas2[1], "-", input$CodPR_seas2[2])), Cod_mortality_mt = "Value", Angler_trips = "Value", Cod_per_under = "Value")

    cod <- rbind(cod_alt_FH_1, cod_alt_FH_2, cod_alt_PR_1, cod_alt_PR_2) %>%
      dplyr::filter(Cod_Limit > 0)

    had_alt_FH_1 <- data.frame(Option = c("alt"), season = c("1"), Mode = c("For Hire"), Had_Limit = c(input$HadFH_1_bag), Had_Size = c(input$HadFH_1_len),
                               Had_open = c(paste(input$HadFH_seas1[1], "-", input$HadFH_seas1[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")
    had_alt_PR_1 <- data.frame(Option = c("alt"), season = c("1"), Mode = c("Private"), Had_Limit = c(input$HadPR_1_bag), Had_Size = c(input$HadPR_1_len),
                               Had_open = c(paste(input$HadPR_seas1[1], "-", input$HadPR_seas1[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")
    had_alt_FH_2 <- data.frame(Option = c("alt"), season = c("2"), Mode = c("For Hire"), Had_Limit = c(input$HadFH_2_bag), Had_Size = c(input$HadFH_2_len),
                               Had_open = c(paste(input$HadFH_seas2[1], "-", input$HadFH_seas2[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")
    had_alt_PR_2 <- data.frame(Option = c("alt"), season = c("2"), Mode = c("Private"), Had_Limit = c(input$HadPR_2_bag), Had_Size = c(input$HadPR_2_len),
                               Had_open = c(paste(input$HadPR_seas2[1], "-", input$HadPR_seas2[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")
    had_alt_FH_3 <- data.frame(Option = c("alt"), season = c("3"), Mode = c("For Hire"), Had_Limit = c(input$HadFH_3_bag), Had_Size = c(input$HadFH_3_len),
                               Had_open = c(paste(input$HadFH_seas3[1], "-", input$HadFH_seas3[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")
    had_alt_PR_3 <- data.frame(Option = c("alt"), season = c("3"), Mode = c("Private"), Had_Limit = c(input$HadPR_3_bag), Had_Size = c(input$HadPR_3_len),
                               Had_open = c(paste(input$HadPR_seas3[1], "-", input$HadPR_seas3[2])), Had_mortality_mt = "Value", Angler_trips = "Value", Had_per_under = "Value")

    had <- rbind(had_alt_FH_1, had_alt_PR_1, had_alt_FH_2, had_alt_PR_2, had_alt_FH_3, had_alt_PR_3) %>%
      dplyr::filter(Had_Limit > 0)

    out<- cod %>% dplyr::full_join(had, by = c("Mode", "season", "Option", "Angler_trips"))

    regs_output <- rbind(sq,out) %>%
     #sq %>%
      dplyr::select(Option,season,Mode,Cod_Limit,Cod_Size,Cod_open,Cod_mortality_mt,Cod_per_under,
                    Had_Limit,Had_Size,Had_open,Had_mortality_mt,Had_per_under,Angler_trips) %>%
      #dplyr::mutate(Cod_open = stringr::str_replace_all(Cod_open, "2023-", ""),
      #              Cod_open = stringr::str_replace_all(Cod_open, "2024-", ""),
      #              Had_open = stringr::str_replace_all(Had_open, "2023-", ""),
      #              Had_open = stringr::str_replace_all(Had_open, "2024-", "")) %>%
      dplyr::group_by(Option, Mode) %>%
      dplyr::summarise(Cod_Limit = paste0(sort(unique(Cod_Limit)), collapse = " , "),
                       Cod_Size = paste0(sort(unique(Cod_Size)), collapse = " , "),
                       Cod_open = paste0(sort(unique(Cod_open)), collapse = " , "),
                       Cod_mortality_mt = paste0(sort(unique(Cod_mortality_mt)), collapse = " , "),
                       Cod_per_under = paste0(sort(unique(Cod_per_under)), collapse = " , "),
                       Had_Limit = paste0(sort(unique(Had_Limit)), collapse = " , "),
                       Had_Size = paste0(sort(unique(Had_Size)), collapse = " , "),
                       Had_open = paste0(sort(unique(Had_open)), collapse = " , "),
                       Had_mortality_mt = paste0(sort(unique(Had_mortality_mt)), collapse = " , "),
                       Had_per_under = paste0(sort(unique(Had_per_under)), collapse = " , "),
                       Angler_trips = paste0(sort(unique(Angler_trips)), collapse = " , "),
                       Cod_open = dplyr::case_when(Option == "SQ" ~ stringr::str_replace_all(Cod_open, "^.*?,", ""), TRUE ~ Cod_open),
                       Angler_trips = stringr::str_replace_all(Angler_trips, "^.*?,", "")) %>%
      dplyr::rename("Cod Size" = Cod_Size, "Cod Limit" = Cod_Limit, "Cod Open Season" = Cod_open,
                    "Cod Total Mortality mt (median)" = Cod_mortality_mt,
                    "% Under Cod ACL (out of 100 smulations)" = Cod_per_under,
                    "Had Size" = Had_Size, "Had Limit" = Had_Limit, "Had Open Season" = Had_open,
                    "Had Total Mortality mt (median)" = Had_mortality_mt,
                    "% Under Had ACL (out of 100 smulations)" = Had_per_under, "Angler Trips (median)" = Angler_trips) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    return(regs_output)
    })

  ###Output Tables
  output$regtableout <- renderTable({
    regulations()
  })

  # observeEvent(input$runmeplease, {
  #
  #   #httr::write_disk(here::here("output2.csv"))
  #
  #   dat<- htmlTable::htmlTable(regulations(), rnames = FALSE)
  #
  #   mailR::send.mail(from="kimberly.bastille@gmail.com",to="kimberly.bastille@noaa.gov",subject="subject",
  #            body= dat,
  #            smtp=list(host.name='http://smtp.gmail.com', user.name = "kimberly.bastille@noaa.gov", passwd = "passowrd"), # Change password
  #            authenticate = T,
  #            send = T)#,
  #            #attach.files = c(here::here("output2.csv")))
  # })

  output$downloadData <- downloadHandler(
    filename = function(){"RecDSToutput.xlsx"},
    content = function(filename) {
      df_list <- list(Regulations=regulations())
      openxlsx::write.xlsx(x = df_list , file = filename, row.names = FALSE)
    })

}
shiny::shinyApp(ui = ui, server = server)
