
mypackages <- c("shiny", "shinyhelper", "magrittr", "shinyFiles", "ggplot2", "svglite", "shinyalert",
                "MASS", "ggplot2", "tidyr", "dplyr", "cowplot", "purrr", "shinybusy", "janitor", "sn", "EnvStats", "shinytest")
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
library(shinyFiles)
library(dplyr)
library(shinybusy)
library(janitor)
library(sn)
library(EnvStats)
library(shinyalert)
library(shinytest)

ggthemes = list("Classic" = theme_classic(),
                "Dark" = theme_dark(),
                "Minimal" = theme_minimal(),
                "Grey" = theme_grey(),
                "Light" = theme_light(),
                "Black/White" = theme_bw(),
                "Void" = theme_void())

shinyUI(fluidPage(
    #add_busy_spinner(spin = "fading-circle"),
    #img(src='coldbug.jpg',  height = '250px', width = '200px', style = 'float:right;'),
    
    tags$style(HTML("
                  .tabbable > .nav > li > a                  {font-weight: bold; background-color: skyblue;  color:black}
                  .tabbable > .nav > li[class=active]    > a {background-color: gold; color:black}
                  ")),
    
    # Application title
    titlePanel(h1(strong("ThermalSampleR",style = "color:black; font-size:85%"))),

    br(), br(),
    # sidebarLayout(
    # 
    #     sidebarPanel(
    #         strong("Click the i symbol to view the help file:", style = "color:darkblue; font-size:120%")
    #         %>% helper(type = "markdown", content = "thermalsampler_manual", colour = "darkblue", icon = "info-circle")
    #         
    #         
    #     ),



    mainPanel(width = 12,
        tabsetPanel(
            
            tabPanel(strong("HOME"),
                     br(), br(),
                     div( img(src='thermalsampler_logo.png',  height = '350px', width = '450px'), style="display: block; margin-left: auto; margin-right: auto; text-align: center;" ),
                     br(),
                     wellPanel(
                     h3(strong("ABOUT")),
                     br(),
                     h5(strong("Navigate using the tabs at the top of the window."), br(),
                     h5(strong("The R package version of ThermalSampleR is available on CRAN and GitHub.")),
                     HTML("<p align = 'left'><img src = 'GitHub.png' width = '20px' height = 'auto'> <a target='_blank' rel='noopener noreferrer' href='https://github.com/clarkevansteenderen/ThermalSampleR'> GitHub Link </a></p>"),
                     h5(strong("Please cite as: Owen, C., Sutton, G., Martin, G., van Steenderen, C., and Coetzee, J.
                               Sample size planning for insect critical thermal limit studies. 2022. Physiological Entomology. Under review.")), br(),
                     ), # end of wellpanel
                     ),
                     
            ),
            
            tabPanel(strong("1. INPUT DATA"),
                     br(), br(),
                     h3(strong("UPLOAD YOUR DATA FILE HERE")),
                     br(),
                     
                     fluidRow(
                         column(5,
                     fileInput("inFile", "Select a thermal data .csv file", accept = c(".csv")),
                     
                     selectInput("col.group", "Select Group Column:", choices=NULL),
                     selectInput("col.response", "Select Response Column:", choices=NULL),
                     htmlOutput("warning"), tags$head(tags$style("#warning{color: red; font-size: 16px;}")),
                     selectInput("col.inc", "Select Which Group/s to Include:", choices=NULL, multiple = T),
                         ),
                     column(4,
                     selectizeInput("example_data", strong("Select Example File to Download:"), choices = list.files("example_data/")),
                     downloadButton("download_example", "Download Example", style="color: black; background-color: lightgrey; border-color: black; font-size:100%")
                     ),
                         )
            ),
            
            tabPanel(strong("2. SIMULATE"),
                     br(), br(),
                     h3(strong("BOOTSTRAP SIMULATION SETTINGS:")),
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
                     br(), br()   
            ),
            
            tabPanel(strong("3. PLOT TWEAKS"),
                     br(), br(),
                     h3(strong("PLOT SETTINGS:")),
                     br(),
                     fluidRow(
                         column(5,
                     numericInput("nminplot", "Minimum sample size to extrapolate simulations", value = 3, min = 1),
                     numericInput("nmaxplot", "Maximum sample size to extrapolate simulations", value = 15),
                     selectInput("colour_exp", "Select Experimental Data Colour:", choices = c(colors()), selected = "blue"),
                     selectInput("colour_extrap", "Select Extrapolation Data Colour:", choices = c(colors()), selected = "red"),
                     sliderInput("alpha", "Colour Transparency:", min = 0, max = 1, value = 0.2),
                     sliderInput("line_width", "Line width:", min = 0.1, max = 5, value = 1),
                         ),
                     
                         column(4,
                     selectInput("point_shape", "Point Shape (For Two Groups Plot):", choices = c("Squares" = 15, "Circles" = 16, "Triangles" = 17, "Diamonds" = 18, "Crosses" = 4, "Stars" = 8), selected = 16),
                     numericInput("point_size", "Point Size (For Two Groups Plot):", value = 2, min = 0),
                     selectInput("legend", "Legend Position:", choices = c("top", "right", "bottom", "left", "none"), selected = "top"),
                     selectInput("ggtheme", "Select ggplot Theme:", choices = names(ggthemes), selected = ggthemes["Classic"]),
                     br(), br(), br(),
                     actionButton("plot", strong("CREATE PLOT/S"), style="color: #fff; background-color: darkblue; border-color: white; font-size:120%", icon("pencil"))
                        )),
            ),
            
            tabPanel(
                strong("VIEW PLOT FOR ONE GROUP"),
                br(), br(),
                h4(strong("View the plot for one group below.")),
                br(),
                
                h3(strong("DOWNLOAD")),
                
                wellPanel(
                    
                textInput("file_name_one_group", "File name: ", "one_group_plot"),
                
                fluidRow(
                    column(width = 2,
                           selectInput("plot_format_one_group", "Image format:", choices = c("pdf", "png", "svg"), width = "150px"),
                    ),
                    column(width = 2,
                           textInput("w_plot_one_group", "Width: ", 20, width = "150px"),
                    ),
                    column(width = 2,
                           textInput("h_plot_one_group", "Height: ", 15, width = "150px"),
                    ),
                    column(width = 2,
                           selectInput("unit_plot_one_group", "Unit: ", choices=c("cm", "in"), width = "150px"),
                    ),
                    column(width = 2,
                           conditionalPanel(
                               condition = "input.plot_format_one_group == 'png'",
                               textInput("res_plot_one_group", "Res (dpi): ", 300), width = "150px")
                    ),
                ),
                
                downloadButton("downloadplot_onegroup", "Download Plot", style="color: black; background-color: lightgrey; border-color: black; font-size:100%"),
                downloadButton("download_table_one", "Download Summary Table", style="color: black; background-color: lightgrey; border-color: black; font-size:100%"),
                
                ), # end of wellpanel
                
                br(), br(), br(),
                plotOutput("plotone", width = 1000, height = 500),
                br(), br()),


            tabPanel(
                strong("VIEW PLOT FOR TWO GROUPS"),
                br(), br(),
                h4(strong("View the plot for two groups below.")),
                br(),
                
                h3(strong("DOWNLOAD")),
               
                wellPanel(
                textInput("file_name_two_groups", "File name: ", "two_groups_plot"),
                
                fluidRow(
                    column(width = 2,
                           selectInput("plot_format_two_groups", "Image format:", choices = c("pdf", "png", "svg"), width = "150px"),
                    ),
                    column(width = 2,
                           textInput("w_plot_two_groups", "Width: ", 20, width = "150px"),
                    ),
                    column(width = 2,
                           textInput("h_plot_two_groups", "Height: ", 15, width = "150px"),
                    ),
                    column(width = 2,
                           selectInput("unit_plot_two_groups", "Unit: ", choices=c("cm", "in"), width = "150px"),
                    ),
                    column(width = 2,
                           conditionalPanel(
                               condition = "input.plot_format_two_groups == 'png'",
                               textInput("res_plot_two_groups", "Res (dpi): ", 300), width = "150px")
                    ),
                ),
                
                downloadButton("downloadplot_twogroups", "Download Plot", style="color: black; background-color: lightgrey; border-color: black; font-size:100%"),
                downloadButton("download_table_two", "Download Summary Table", style="color: black; background-color: lightgrey; border-color: black; font-size:100%"),
                
                ), # end of wellpanel
                
                br(), br(), br(),
                plotOutput("plottwo"), br(), br()),
            
            tabPanel(
                strong("4. TEST OF TOTAL EQUIVALENCY (TTE)"),
                br(), br(),
                h3(strong("TEST OF TOTAL EQUIVALENCY (TTE) SETTINGS")),
                br(),
                fluidRow(
                    column(5,
                selectInput("col.inc.tte", "Select Which Group to Include:", choices=NULL, multiple = F),
                br(),
                textInput("skews", "Skewness parameters (enter values separated by a comma)", value = c("0, 1, 2, 10, 50")),
                br(),
                selectInput("tte_colrs", "Select colours for each skewness value", 
                            choices = c("blue", "red", "orange", "forestgreen", "lightgrey", "lightblue", "lightpink", "yellow", "black", "purple"), 
                            multiple = TRUE),
                br(),
                actionButton("plot_tte", strong("RUN ANALYSIS"), style="color: #fff; background-color: darkblue; border-color: white; font-size:120%"),
                
                #sliderInput("alpha_tte", "Colour Transparency:", min = 0, max = 1, value = 0.3),
                #sliderInput("line_width_tte", "Line width:", min = 0.1, max = 5, value = 1.5),
                    ),
                column(7,
                numericInput("equiv_margin", "Equivalence margin of subsets to full population of CT estimate (degrees C)", value = 1, step = 0.1),
                br(), 
                numericInput("pop_n", "Population size to sample", value = 30, step = 1, min = 1),
                br(),
                selectInput("ggtheme_tte", "Select ggplot Theme:", choices = names(ggthemes), selected = ggthemes["Classic"]),
                br(),
                
                ),
                ), # end of fluidrow
                
                br(),
                h3(strong("DOWNLOAD")),
                
                wellPanel(
                br(), 
                textInput("file_name_tte", "File name: ", "tte"),
                
                fluidRow(
                    column(width = 2,
                           selectInput("plot_format_tte", "Image format:", choices = c("pdf", "png", "svg"), width = "150px"),
                    ),
                    column(width = 2,
                           textInput("w_plot_tte", "Width: ", 20, width = "150px"),
                    ),
                    column(width = 2,
                           textInput("h_plot_tte", "Height: ", 15, width = "150px"),
                    ),
                    column(width = 2,
                           selectInput("unit_plot_tte", "Unit: ", choices=c("cm", "in"), width = "150px"),
                    ),
                    column(width = 2,
                           conditionalPanel(
                               condition = "input.plot_format_tte == 'png'",
                               textInput("res_plot_tte", "Res (dpi): ", 300), width = "150px")
                    ),
                ),
                
                downloadButton("downloadplot_tte", "Download Plot",  style="color: black; background-color: lightgrey; border-color: black; font-size:100%"),
                
                ),
                
                br(), br(),
                plotOutput("tte_plot", width = 1000, height = 500) )

            )
    )

))
