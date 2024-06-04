# Required packages - everything else uses package:: found in r/required_packages.R
library(shiny)
library(shinyjs)
library(dplyr)

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

  source(here::here(paste0("model_run.R")), local = TRUE)
  predictions_1 <- predictions_1 %>% rbind(predictions)

  #### Regulations ####
  regulations <- eventReactive(input$runmeplease,{

    dat <- NULL
    #### MA regs ####
    CodFHseason1 <- data.frame(Species = c("Cod"), Mode = c("For Hire"),
                               Season = paste(input$CodFH_seas1[1], "-", input$CodFH_seas1[2]),
                               BagLimit = paste(input$CodFH_1_bag),
                               Length = paste(input$CodFH_1_len))
    CodPRseason1 <- data.frame(Species = c("Cod"), Mode = c("Private"),
                               Season = paste(input$CodPR_seas1[1], "-", input$CodPR_seas1[2]),
                               BagLimit = paste(input$CodPR_1_bag),
                               Length = paste(input$CodPR_1_len))

    CodFHseason2 <- data.frame(Species = c("Cod"), Mode = c("For Hire"),
                               Season = paste(input$CodFH_seas2[1], "-", input$CodFH_seas2[2]),
                               BagLimit = paste(input$CodFH_2_bag),
                               Length = paste(input$CodFH_2_len))
    CodPRseason2 <- data.frame(Species = c("Cod"), Mode = c("Private"),
                               Season = paste(input$CodPR_seas2[1], "-", input$CodPR_seas2[2]),
                               BagLimit = paste(input$CodPR_2_bag),
                               Length = paste(input$CodPR_2_len))
    Codreg <- rbind(CodFHseason1, CodFHseason2, CodPRseason1,  CodPRseason2)

    HadFHseason1 <- data.frame(Species = c("Haddock"), Mode = c("For Hire"),
                               Season = paste(input$HadFH_seas1[1], "-", input$HadFH_seas1[2]),
                               BagLimit = paste(input$HadFH_1_bag),
                               Length = paste(input$HadFH_1_len))
    HadPRseason1 <- data.frame(Species = c("Haddock"), Mode = c("Private"),
                               Season = paste(input$HadPR_seas1[1], "-", input$HadPR_seas1[2]),
                               BagLimit = paste(input$HadPR_1_bag),
                               Length = paste(input$HadPR_1_len))

    HadFHseason2 <- data.frame(Species = c("Haddock"), Mode = c("For Hire"),
                               Season = paste(input$HadFH_seas2[1], "-", input$HadFH_seas2[2]),
                               BagLimit = paste(input$HadFH_2_bag),
                               Length = paste(input$HadFH_2_len))
    HadPRseason2 <- data.frame(Species = c("Haddock"), Mode = c("Private"),
                               Season = paste(input$HadPR_seas2[1], "-", input$HadPR_seas2[2]),
                               BagLimit = paste(input$HadPR_2_bag),
                               Length = paste(input$HadPR_2_len))
    HadFHseason3 <- data.frame(Species = c("Haddock"), Mode = c("For Hire"),
                               Season = paste(input$HadFH_seas3[1], "-", input$HadFH_seas3[2]),
                               BagLimit = paste(input$HadFH_3_bag),
                               Length = paste(input$HadFH_3_len))
    HadPRseason3 <- data.frame(Species = c("Haddock"), Mode = c("Private"),
                               Season = paste(input$HadPR_seas3[1], "-", input$HadPR_seas3[2]),
                               BagLimit = paste(input$HadPR_3_bag),
                               Length = paste(input$HadPR_3_len))
    Hadreg <- rbind(HadFHseason1, HadFHseason2,  HadFHseason3, HadPRseason1,  HadPRseason2, HadPRseason3)

    regs_output<- rbind(Codreg, Hadreg) %>%
      dplyr::filter(!BagLimit == "0",
                    !BagLimit == "0 , 0") %>%
      dplyr::mutate(Season = stringr::str_remove(Season, pattern = "2023-"),
                    Season = stringr::str_remove(Season, pattern = "2023-"))
    return(regs_output)
    })

}
shiny::shinyApp(ui = ui, server = server)
