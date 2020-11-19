

ggthemes = list("Classic" = theme_classic(),
                "Dark" = theme_dark(),
                "Minimal" = theme_minimal(),
                "Grey" = theme_grey(),
                "Light" = theme_light(),
                "Black/White" = theme_bw(),
                "Void" = theme_void()) 

# needed to add in the session argument here to get the updateSelectInput function to work
shinyServer(function(session, input, output) {


    observe_helpers(help_dir = "helpfile")

    ###################################################
    # LET USER DOWNLOAD SAMPLE COREID DATA IF DESIRED
    ###################################################
    getname <- reactive(basename(input$example_data))

    output$download_example <- downloadHandler(
        filename = function() {
            paste(input$example_data)
        },
        content = function(file) {
            file.copy(paste("example_data/", getname(), sep=""), file)
        })
    ###################################################

    ###################################################


    ###################################################
    # START THE PROCESS BY READING IN A CSV FILE
    ###################################################

    observe({ # start of first observe

        # read in the csv file from the user's local PC
        file1 = input$inFile
        if (is.null(file1)) {
            return(NULL)
        } # stops the app crashing while the user hasn't uploaded anything yet
        
        
        data = read.csv(file1$datapath)

        # update the drop-down selection menus to show the column names in the uploaded data
        updateSelectInput(session,"col.group", choices=colnames(data))
        updateSelectInput(session,"col.response", choices=colnames(data))

        # do the following when the user selects their group column
        observeEvent(input$col.group,
                     {
                         groups = as.factor(data[[input$col.group]])

                         if(length(levels(groups)) > 2)
                             output$warning = renderText(c("<b>WARNING: Your data contains ", length(levels(groups)), "groups, or you have selected the incorrect group column. Please refresh the app and re-upload data with one, or two groups, or select the correct grouping column."))
                         else output$warning = renderText("")

                         updateSelectInput(session, "col.inc", choices=levels(groups) ) # double brackets to access column by its name (alternative to number, or $ sign)

                         req(input$col.inc) # wait for the user to select the names they want

                     })

        # do the following when the "Simulate and Plot" button is clicked
        observeEvent(input$simulate,
                     {
                         data[[input$col.group]] = as.factor(data[[input$col.group]])
                         data[[input$col.response]] = as.numeric(data[[input$col.response]])

                         groups_col = as.name( input$col.group ) # converting the string to an as.name() class worked!
                         response = as.name( input$col.response )

                         groups_which = input$col.inc
                         iter = input$iter
                         n_minboots = input$nminboots
                         n_maxboots = input$nmaxboots
                         
                         if(length(groups_which)==0) output$warning = renderText("<b>Please select which group/s to include in the analysis.")
                         else output$warning = renderText("")
                         
                         if(input$setseed == 2) set.seed(input$seedval)

                         #output$text = renderText(length(groups_which))

                         # if(length(groups_which) >2)
                         #     output$warning = renderText("You have selected more than two groups. Please select only one, or two.")
                         # else{ # ***

                         #################################################################
                         # START OF BOOTSTRAPPING FOR ONE SAMPLE
                         #################################################################
                        

                         if(length(groups_which)==1){

                             #####################
                             # START PROGRESS BAR
                             ####################

                             # show_modal_progress_line() # show the modal window
                             # update_modal_progress(0.2) # update progress bar value
                             # remove_modal_progress() # remove it when done
                             
                             
                             progress <- Progress$new(session, min=1, max=2)
                             on.exit(progress$close())

                             progress$set(message = 'Bootstrapping for one group....',
                                          detail = 'Please wait.')

                             for (i in 1:2) {
                                 progress$set(value = i)
                                 Sys.sleep(0.5)
                             }

                             ###################
                             # END PROGRESS BAR
                             ###################

                             boot_data <- {{ data }} %>%
                                 dplyr::group_by( {{ groups_col }} ) %>%
                                 dplyr::filter({{ groups_col }} %in% c( {{ groups_which }} )) %>%
                                 tidyr::nest() %>%
                                 tidyr::crossing(sample_size = c({{ n_minboots }} : {{ n_maxboots }}),
                                                 iter = seq(1: {{ iter }} )) %>%
                                 # Added sampling with replacement to code below
                                 dplyr::mutate(sample_data = purrr::map2(data,
                                                                         sample_size,
                                                                         ~dplyr::sample_n(.x, .y,
                                                                                          # Sample with replacement
                                                                                          replace = TRUE))) %>%
                                 dplyr::mutate(calc = purrr::map(sample_data,
                                                                 ~dplyr::summarize(.,
                                                                                   mean_val = mean( {{ response }} ),
                                                                                   sd_val = stats::sd(( {{ response }} ))))) %>%
                                 dplyr::select({{ groups_col }}, sample_size, iter, calc) %>%
                                 tidyr::unnest(cols = calc)

                             # Estimate population CT value per species
                             median_vals <- boot_data %>%
                                 dplyr::group_by({{ groups_col }}) %>%
                                 dplyr::filter(sample_size == max(sample_size)) %>%
                                 dplyr::mutate(median_pop_val = mean_val) %>%
                                 dplyr::slice(1) %>%
                                 dplyr::select({{ groups_col }}, median_pop_val)
                             median_vals

                             # Add median value per species to the bootstrapped dataset
                             # Join the two datsets by the {{ groups_col }} column
                             boot_proc <- suppressMessages(dplyr::full_join(boot_data,
                                                           median_vals))
                             boot_proc

                             # Add CI's to raw bootstrap samples
                             boot_comb <- boot_proc %>%
                                 dplyr::group_by({{ groups_col }}) %>%
                                 # Add standard errors
                                 dplyr::mutate(std_error = sd_val/sqrt(sample_size)) %>%
                                 # Now calculate error
                                 dplyr::mutate(error = stats::qt(0.975, df = sample_size - 1) * std_error) %>%
                                 # Calculate lower and upper 95% CI limits
                                 dplyr::mutate(lower_ci = mean_val - error,
                                               upper_ci  = mean_val + error) %>%
                                 dplyr::ungroup()

                             # Calculate proportion of bootstrap samples containing median CT value
                             boot_comb <- boot_comb %>%
                                 dplyr::group_by({{ groups_col }}, sample_size) %>%
                                 dplyr::mutate(ci_falls = dplyr::case_when(
                                     median_pop_val < lower_ci ~ 0,
                                     median_pop_val > upper_ci ~ 0,
                                     median_pop_val > lower_ci & median_pop_val < upper_ci ~ 1,
                                     median_pop_val < upper_ci & median_pop_val > lower_ci ~ 1)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::group_by({{ groups_col }}, sample_size) %>%
                                 dplyr::mutate(prop_correct = sum(ci_falls)/max( {{ iter }} )) %>%
                                 dplyr::ungroup()

                             # Calculate summary statistics
                             data_sum <- suppressMessages(boot_comb %>%
                                 dplyr::group_by({{ groups_col }}, sample_size) %>%
                                 dplyr::summarise(mean_low_ci    = mean(lower_ci),
                                                  mean_upp_ci    = mean(upper_ci),
                                                  mean_ct        = mean(mean_val),
                                                  width_ci       = mean_upp_ci - mean_low_ci,
                                                  sd_width       = stats::sd(upper_ci - lower_ci),
                                                  sd_width_lower = width_ci - sd_width,
                                                  sd_width_upper = width_ci + sd_width,
                                                  median_pop_val = max(median_pop_val),
                                                  prop_ci_contain = sum(ci_falls)/ {{ iter}},
                                                  iter=iter,
                                                  lower_ci=lower_ci,
                                                  upper_ci=upper_ci))

                             # data_sum object is used to plot for one group

                             output$done = renderText("<b>BOOTSTRAPPING COMPLETE FOR ONE SAMPLE :)")
                             
                             #################################################################
                             # DOWNLOAD THE DATA TABLE FOR ONE GROUP
                             #################################################################
                             
                             
                             output$download_table_one <- downloadHandler(
                                 filename = function (){paste("one_group_data", "csv", sep = '.')},
                                 content = function(file){write.csv(data_sum, file, row.names = F)}
                                 
                             )
                             
                             #################################################################
                             
                             #################################################################

                         } # end of if statement for one group

                             #################################################################
                             # END  OF BOOTSTRAPPING FOR ONE SAMPLE
                             #################################################################
                         
                        

                         else if (length(groups_which == 2)){

                             group1 = groups_which[1]
                             group2 = groups_which[2]

                             #################################################################
                             # START OF BOOTSTRAPPING FOR TWO SAMPLES
                             #################################################################

                             #####################
                             # START PROGRESS BAR
                             ####################

                             progress <- Progress$new(session, min=1, max=2)
                             on.exit(progress$close())

                             progress$set(message = 'Bootstrapping for two groups....',
                                          detail = 'Please wait.')

                             for (i in 1:2) {
                                 progress$set(value = i)
                                 Sys.sleep(0.5)
                             }

                             ###################
                             # END PROGRESS BAR
                             ###################

                             # Filter which species to plot
                             df <- dplyr::filter({{ data }},
                                                 {{ groups_col }} %in% c( {{group1}}, {{ group2 }} ))


                             # Perform boostrap sampling
                             boot_data <- suppressMessages(df %>%
                                 dplyr::group_by( {{ groups_col }} ) %>%
                                 tidyr::nest() %>%
                                 tidyr::crossing(sample_size = c({{ n_minboots }} : {{ n_maxboots }}),
                                                 iter = seq(1: {{ iter }} )) %>%
                                 # Added sampling with replacement to code below
                                 dplyr::mutate(sample_data = purrr::map2(data,
                                                                         sample_size,
                                                                         ~ dplyr::sample_n(.x, .y,
                                                                                           # Sample with replacement
                                                                                           replace = TRUE))) %>%
                                 dplyr::mutate(calc = purrr::map(sample_data,
                                                                 ~ dplyr::summarise(.,
                                                                                    mean_val = mean( {{ response }} ),
                                                                                    sd_val = stats::sd(( {{ response }} ))))) %>%
                                 dplyr::select({{ groups_col }}, sample_size, iter, calc) %>%
                                 tidyr::unnest(cols = calc))

                             # Add CI's to raw bootstrap samples
                             boot_data <- boot_data %>%
                                 # Add standard errors
                                 dplyr::mutate(std_error = sd_val/sqrt(sample_size)) %>%
                                 # Now find error
                                 dplyr::mutate(error = stats::qt(0.975, df = sample_size - 1) * std_error) %>%
                                 # Calculate lower and upper 95% CI limits
                                 dplyr::mutate(lower_ci  = mean_val - error,
                                               upper_ci  = mean_val + error) %>%
                                 dplyr::ungroup()

                             # Split data from two groups
                             sp1_data <- boot_data %>%
                                 dplyr::filter({{ groups_col }} == {{ group1 }})
                             sp2_data <- boot_data %>%
                                 dplyr::filter({{ groups_col }} == {{ group2 }})

                             # Convert data into wide format
                             comb_data <- dplyr::left_join(sp1_data,
                                                           sp2_data,
                                                           by = c("sample_size", "iter"))

                             # Let's keep only the columns we need, as things are about to get real
                             comb_data <- comb_data %>%
                                 dplyr::select(sample_size,
                                               iter,
                                               col.x,
                                               mean_val.x,
                                               sd_val.x,
                                               col.y,
                                               mean_val.y,
                                               sd_val.y)

                             # Add student t CI's
                             comb_data <- comb_data %>%
                                 dplyr::mutate(mean_diff    = mean_val.x - mean_val.y,
                                               first_term   = (sample_size - 1) * sd_val.x^2,
                                               second_term  = (sample_size - 1) * sd_val.y^2,
                                               pooled_df    = (sample_size * 2) - 2,
                                               s_pooled     = sqrt((first_term + second_term) / pooled_df),
                                               se_diff      = s_pooled * sqrt((1 / sample_size) + (1 / sample_size)),
                                               error        = stats::qt(0.975, df = pooled_df) * se_diff,
                                               lower_ci     = mean_diff - error,
                                               upper_ci     = mean_diff + error)

                             # Calculate summary statistics
                             comb_data_sum <- suppressMessages(comb_data %>%
                                 dplyr::group_by(sample_size) %>%
                                 dplyr::summarise(mean_low_ci = mean(lower_ci),
                                                  mean_upp_ci    = mean(upper_ci),
                                                  mean_diff      = mean(mean_diff),
                                                  width_ci       = mean_upp_ci - mean_low_ci,
                                                  sd_width       = stats::sd(upper_ci - lower_ci),
                                                  sd_width_lower = width_ci - sd_width,
                                                  sd_width_upper = width_ci + sd_width,
                                                  iter=iter,
                                                  lower_ci=lower_ci,
                                                  upper_ci=upper_ci))

                             # comb_data_sum is what is fed into the code for plotting two groups

                             output$done = renderText("<b>BOOTSTRAPPING COMPLETE FOR TWO SAMPLES :)")
                             
                             #################################################################
                             # DOWNLOAD THE DATA TABLE FOR TWO GROUPS
                             #################################################################
                             
                             
                             output$download_table_two <- downloadHandler(
                                 filename = function (){paste("two_group_data", "csv", sep = '.')},
                                 content = function(file){write.csv(comb_data_sum, file, row.names = F)}
                                 
                             )
                             
                             #################################################################
                             
                             #################################################################
                             
                             

                         } # end of else if (length(groups_which == 2)){

                             #################################################################
                             # END OF BOOTSTRAPPING FOR TWO SAMPLES
                             #################################################################
                         

                         observeEvent(input$plot, {

                             #groups_which = input$col.inc

                             if(length(groups_which)==1){

                                 #################################################################
                                 # START  OF PLOTTING FOR ONE SAMPLE
                                 #################################################################

                                 n_minplot = input$nminplot
                                 n_maxplot = input$nmaxplot
                                 x = data_sum
                                 colour_exp = input$colour_exp
                                 colour_extrap = input$colour_extrap
                                 

                                 exp_data <- {{ x }} %>%
                                     dplyr::filter(dplyr::between(sample_size, {{ n_minplot }}, {{ n_maxplot }}))

                                 # Create dataframe for extrapolations from data
                                 ext_data <- {{ x }} %>%
                                     dplyr::filter(dplyr::between(sample_size, {{ n_maxplot }}, max(sample_size)))

                                 # Make a combined dataframe with id included to colour-code ribbon
                                 both_data <- dplyr::bind_rows(exp_data, ext_data, .id = "id")

                                 # Plot the width of the 95% CI
                                 width_plot <- ggplot2::ggplot(data = {{ x }}, aes(x = sample_size,
                                                                                   y = width_ci)) +
                                     geom_line(data = both_data, size = input$line_width, aes(x = sample_size,
                                                                     y = width_ci,
                                                                     colour = id),
                                               alpha = 1) +
                                     scale_colour_manual(values = c(colour_exp, colour_extrap),
                                                         labels = c("Experimental", "Extrapolation")) +
                                     geom_ribbon(data = both_data, aes(ymin = sd_width_lower,
                                                                       ymax = sd_width_upper,
                                                                       fill = id),
                                                 linetype = 3,
                                                 alpha = input$alpha) +
                                     scale_fill_manual(values = c(colour_exp, colour_extrap),
                                                       labels = c("Experimental", "Extrapolation")) +
                                     ggthemes[[input$ggtheme]] +
                                     geom_hline(yintercept = 0,
                                                linetype = "dashed") +
                                     labs(x = "Sample size (n)",
                                          y = "Width of confidence interval (95% CI)",
                                          fill = "Data",
                                          subtitle = paste("(a) ", input$col.inc)) +
                                     theme(panel.border = element_rect(colour = "black", fill = NA),
                                           axis.text = element_text(colour = "black"),
                                           axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                           axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                                           legend.position = input$legend) +
                                     guides(colour = FALSE)

                                 # Plot the width of the 95% CI
                                 contain_plot <- ggplot2::ggplot(data = {{ x }}, aes(x = sample_size,
                                                                                     y = prop_ci_contain)) +
                                     geom_line(data = both_data, size = input$line_width, aes(x = sample_size,
                                                                     y = prop_ci_contain,
                                                                     colour = id),
                                               alpha = 1) +
                                     scale_colour_manual(values = c(colour_exp, colour_extrap),
                                                         labels = c("Experimental", "Extrapolation")) +
                                     ggthemes[[input$ggtheme]] +
                                     geom_hline(yintercept = 0.90,
                                                linetype = "dashed") +
                                     labs(x = "Sample size (n)",
                                          y = "Proportion of 95%'s \ncontaining median CTL",
                                          colour = "Data",
                                          subtitle = paste("(b)", input$col.inc)) +
                                     theme(panel.border = element_rect(colour = "black", fill = NA),
                                           axis.text = element_text(colour = "black"),
                                           axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                           axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                                           legend.position = input$legend)

                                 # Return the plots
                                 output$plotone = renderPlot(cowplot::plot_grid(width_plot, contain_plot, ncol = 2))

                                 #################################################################
                                 # END  OF PLOTTING FOR ONE SAMPLE
                                 #################################################################

                                 #################################################################
                                 # START OF DOWNLOAD PLOT FOR ONE GROUP
                                 #################################################################


                                 output$downloadplot_onegroup <- downloadHandler(
                                     filename = function (){paste("1GroupPlot", "svg", sep = '.')},
                                     content = function(file){
                                         ggsave(file, cowplot::plot_grid(width_plot, contain_plot, ncol = 2))
                                     } # use ggsave, as this cowplot is part of ggplot2. Won't work with just the normal saving
                                 )

                                 ##################################################################
                                 # END OF DOWNLOAD PLOT FOR ONE GROUP
                                 #################################################################

                             }

                             else if (length(groups_which == 2)){


                                 #################################################################
                                 # START  OF PLOTTING FOR TWO SAMPLES
                                 #################################################################

                                 n_minplot = input$nminplot
                                 n_maxplot = input$nmaxplot
                                 x = comb_data_sum
                                 colour_exp = input$colour_exp
                                 colour_extrap = input$colour_extrap

                                 # Create dataframe for experimental data
                                 exp_data <- {{ x }} %>%
                                     dplyr::filter(dplyr::between(sample_size, {{ n_minplot }}, {{ n_maxplot }}))

                                 # Create dataframe for extrapolations from data
                                 ext_data <- {{ x }} %>%
                                     dplyr::filter(dplyr::between(sample_size, {{ n_maxplot }}, max(sample_size)))

                                 # Make a combined datafram with id included to colour-code ribbon
                                 both_data <- dplyr::bind_rows(exp_data, ext_data, .id = "id")

                                 # Plot the width of the 95% CI
                                 width_plot <- ggplot2::ggplot(data = {{ x }}, aes(x = sample_size,
                                                                                   y = width_ci)) +
                                     geom_line(data = both_data, size = input$line_width, aes(x = sample_size,
                                                                     y = width_ci,
                                                                     colour = id),
                                               alpha = 0.8) +
                                     scale_colour_manual(values = c(colour_exp, colour_extrap),
                                                         labels = c("Experimental", "Extrapolation")) +
                                     geom_ribbon(data = both_data, aes(ymin = sd_width_lower,
                                                                       ymax = sd_width_upper,
                                                                       fill = id),
                                                 linetype = 3,
                                                 alpha = input$alpha) +
                                     scale_fill_manual(values = c(colour_exp, colour_extrap),
                                                       labels = c("Experimental", "Extrapolation")) +
                                     theme_classic() +
                                     geom_hline(yintercept = 0,
                                                linetype = "dashed") +
                                     labs(x = "Sample size (n)",
                                          y = "Width of confidence interval (95% CI)",
                                          subtitle = "(a)") +
                                     theme(panel.border = element_rect(colour = "black", fill = NA),
                                           axis.text = element_text(colour = "black"),
                                           axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                           axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                                           legend.position = "none")

                                 # Plot the 95% CI of mean difference

                                 ci_plot <- ggplot2::ggplot(data = {{ x }}, aes(x = sample_size,
                                                                                y = mean_diff)) +
                                     geom_line(data = {{ x }}, aes(x = sample_size,
                                                                   y = mean_low_ci),
                                               linetype = "dashed") +
                                     geom_line(data = {{ x }},  aes(x = sample_size,
                                                                   y = mean_upp_ci),
                                               linetype = "dashed") +
                                     geom_ribbon(data = both_data, aes(ymin = mean_low_ci,
                                                                       ymax = mean_upp_ci,
                                                                       fill = id),
                                                 linetype = 3,
                                                 alpha = input$alpha) +
                                     scale_fill_manual(values = c(colour_exp, colour_extrap),
                                                       labels = c("Experimental", "Extrapolation")) +
                                     geom_point(data = both_data, shape = as.numeric(input$point_shape), size = input$point_size, aes(x = sample_size,
                                                                      y = mean_diff,
                                                                      colour = id),
                                                alpha = 0.8) +
                                     scale_colour_manual(values = c(colour_exp, colour_extrap),
                                                         labels = c("Experimental", "Extrapolation")) +
                                     theme_classic() +
                                     geom_hline(yintercept = 0, linetype = "dashed") +
                                     labs(x = "Sample size (n)",
                                          y = "Mean difference between groups (95% CI)",
                                          subtitle = "(b)",
                                          fill = "Data") +
                                     theme(panel.border = element_rect(colour = "black", fill = NA),
                                           axis.text = element_text(colour = "black"),
                                           axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                                           axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                                           legend.position = input$legend,
                                           legend.key = element_rect(linetype = "dashed")) +
                                     guides(colour = "none")

                                 # Combine the two plots

                                 output$plottwo = renderPlot(cowplot::plot_grid(width_plot, ci_plot, ncol = 2))


                                 #################################################################
                                 # END  OF PLOTTING FOR TWO SAMPLES
                                 #################################################################

                                 #################################################################
                                 # START OF DOWNLOAD PLOT FOR TWO GROUPS
                                 #################################################################

                                 output$downloadplot_twogroups <- downloadHandler(
                                     filename = function (){paste("2GroupsPlot", "svg", sep = '.')},
                                     content = function(file){
                                         ggsave(file, cowplot::plot_grid(width_plot, ci_plot, ncol = 1))
                                     }
                                 )

                             } # end of else if (length(groups_which == 2)){
                             ##################################################################
                             # END OF DOWNLOAD PLOT FOR TWO GROUPS
                             #################################################################

                         })

                     }) # end of observeEvent(input$simulate,


    }) # end of first observe

}) # end of start of shinyserver
