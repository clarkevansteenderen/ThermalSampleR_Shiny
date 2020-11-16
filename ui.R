
mypackages <- c("shiny", "shinyhelper", "magrittr", "shinyFiles", "ggplot2", "svglite", "MASS", "ggplot2", "tidyr", "dplyr", "cowplot", "purrr")
checkpkg <- mypackages[!(mypackages %in% installed.packages()[,"Package"])]
if(length(checkpkg)) install.packages(checkpkg, dependencies = TRUE)

library(tidyr)
library(purrr)
library(rlang)
library(cowplot)
library(ggplot2)
library(svglite)
library(shinyhelper)
library(shiny)
library(magrittr)
library(ggplot2)
library(shinyFiles)
library(dplyr)
library(rhandsontable)

ggthemes = list("Classic" = theme_classic(),
                "Dark" = theme_dark(),
                "Minimal" = theme_minimal(),
                "Grey" = theme_grey(),
                "Light" = theme_light(),
                "Black/White" = theme_bw(),
                "Void" = theme_void())

shinyUI(fluidPage(

    #img(src='coldbug.jpg',  height = '250px', width = '200px', style = 'float:right;'),
    
    # Application title
    titlePanel(h1(strong("ThermalSampleR",style = "color:red; font-size:130%"))),

    br(), br(),
    sidebarLayout(

        sidebarPanel(
            strong("Click to view the help file:", style = "color:darkblue; font-size:120%")
            %>% helper(type = "markdown", content = "thermalsampler_manual", colour = "darkblue", icon = "question-circle"),
            br(), br(),
            fileInput("inFile", "Select a thermal data .csv file", accept = c(".csv")),
            br(),
            selectizeInput("example_data", strong("Select Example File to Download:"), choices = list.files("example_data/")),
            downloadButton("download_example", "Download Example"),
            br(), br(),
            selectInput("col.group", "Select Group Column:", choices=NULL),
            selectInput("col.response", "Select Response Column:", choices=NULL),
            htmlOutput("warning"), tags$head(tags$style("#warning{color: red; font-size: 16px;}")),
            selectInput("col.inc", "Select Which Group/s to Include:", choices=NULL, multiple = T),
            h4(strong("BOOTSTRAP SIMULATION SETTINGS:")),
            br(),
            radioButtons("setseed", "Set a seed?", choiceNames  = c("No", "Yes"), choiceValues = c(1,2), selected = 1),
            strong("Setting a seed will ensure consistent output if this analysis is run again at a later stage (provided all parameters remain the same).", style = "color:darkblue"),
            br(), br(),
            numericInput("seedval", "If yes, set a seed value:", value = 123, min = 0),
            numericInput("iter", "Bootstrap Iterations:", value = 499, min = 1),
            numericInput("nminboots", "Minimum sample size to extrapolate simulations:", value = 3, min = 1),
            numericInput("nmaxboots", "Maximum sample size to extrapolate simulations:", value = 30),
            actionButton("simulate", strong("BOOTSTRAP"), style="color: #fff; background-color: darkblue; border-color: white; font-size:120%",  icon("check")),
            br(), br(),
            htmlOutput("done"), tags$head(tags$style("#done{color: forestgreen; font-size: 18px;}")),
            br(), br(),
            h4(strong("PLOT SETTINGS:")),
            numericInput("nminplot", "Minimum sample size to extrapolate simulations", value = 3, min = 1),
            numericInput("nmaxplot", "Maximum sample size to extrapolate simulations", value = 15),
            selectInput("colour_exp", "Select Experimental Data Colour:", choices = c(colors()), selected = "blue"),
            selectInput("colour_extrap", "Select Extrapolation Data Colour:", choices = c(colors()), selected = "red"),
            sliderInput("alpha", "Colour Transparency:", min = 0, max = 1, value = 0.2),
            selectInput("legend", "Legend Position:", choices = c("top", "right", "bottom", "left", "none"), selected = "top"),
            selectInput("ggtheme", "Select ggplot theme:", choices = names(ggthemes), selected = ggthemes["Classic"]),
            br(),
            actionButton("plot", strong(" PLOT"), style="color: #fff; background-color: darkblue; border-color: white; font-size:120%", icon("pencil"))

        ),

    mainPanel(
        tabsetPanel(
            tabPanel(
                strong("PLOT ONE GROUP"),
                br(), br(),
                downloadButton("downloadplot_onegroup", "Download Plot", style="color: #fff; background-color: darkblue; border-color: white; font-size:120%"),
                plotOutput("plotone", width = 1000, height = 500) ),

            tabPanel(
                strong("PLOT TWO GROUPS"),
                br(), br(),
                downloadButton("downloadplot_twogroups", "Download Plot", style="color: #fff; background-color: darkblue; border-color: white; font-size:120%"),
                plotOutput("plottwo") )
            )
    )
)))
