library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Kornkammer",
                   theme = shinytheme("united"),
                   
                   tabPanel("Warenbestand",
                            titlePanel("Warenbestand der Kornkammer"),
                            
                            sidebarPanel(
                              helpText("Was willst du sehen?"),
                                         width = 3,
                              checkboxInput("settings",
                                            label = "Graphische Einstellungen",
                                            value = FALSE),
                              conditionalPanel(condition = "input.settings == true",
                                               checkboxGroupInput("optional", label ="",
                                                                  choices = list("Von" = 1,
                                                                                 "Bis" = 2,
                                                                                 "span" = 3,
                                                                                 "Punktfarbe" = 4,
                                                                                 "Linienfarbe" = 4))),
                              checkboxInput("to",
                                            label = "Bis",
                                            value = FALSE),
                              conditionalPanel(condition = "input.to == true",
                                               dateInput("date_to",
                                                         label = "Bis zum")),
                              selectInput("var", 
                                          label = "Choose a variable to display",
                                          choices = c("Percent White", "Percent Black",
                                                      "Percent Hispanic", "Percent Asian"),
                                          selected = "Percent White")
                            ),
                            
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Getreideprodukte", textOutput("grund")),
                                          tabPanel("Hülsenfrüchte", plotOutput("huelsenfruechte")),
                                          tabPanel("Ölsaaten", verbatimTextOutput("oelsaaten")),
                                          tabPanel("Gewürze", tableOutput("gewuerze")),
                                          tabPanel("Öl und Essig", plotOutput("oel.essig")),
                                          tabPanel("Getränke", plotOutput("getraenke")),
                                          tabPanel("Aufstriche", plotOutput("aufstriche")),
                                          tabPanel("Sonstiges", plotOutput("sonstiges"))
                              )
                            )),
                   
                   tabPanel("Zukunftsprognose",
                            titlePanel("Zukunftsprognose für den Warenbestand"),
                            
                            sidebarPanel(
                              helpText("Was willst du sehen?"),
                              width = 3,
                              
                              checkboxInput("from",
                                            label = "Von",
                                            value = FALSE),
                              conditionalPanel(condition = "input.from == true",
                                               dateInput("date_from",
                                                         label = "Vom")),
                              checkboxInput("to",
                                            label = "Bis",
                                            value = FALSE),
                              conditionalPanel(condition = "input.to == true",
                                               dateInput("date_to",
                                                         label = "Bis zum"))
                            ),
                            
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Getreideprodukte", plotOutput("plot.grund")),
                                          tabPanel("Hülsenfrüchte", plotOutput("huelsenfruechte")),
                                          tabPanel("Ölsaaten", verbatimTextOutput("oelsaaten")),
                                          tabPanel("Gewürze", tableOutput("gewuerze")),
                                          tabPanel("Öl und Essig", plotOutput("oel.essig")),
                                          tabPanel("Getränke", plotOutput("getraenke")),
                                          tabPanel("Aufstriche", plotOutput("aufstriche")),
                                          tabPanel("Sonstiges", plotOutput("sonstiges"))
                              )
                            )),
                   navbarMenu("Erweiterte Einstellungen",
                              tabPanel("Produktzusammenführung"),
                              tabPanel("Produktgruppenerstellung"))
))