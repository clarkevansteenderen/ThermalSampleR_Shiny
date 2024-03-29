# Function to perform Test of Total Equivalence testing approach by Duffy et al. (2021)
# in their functional ecology paper
# - Original code was written by Grant Duffy (Otago University).
# - Code adapted and processed into a function by Guy Sutton and Clarke van Steenderen
#   (Rhodes University)

###########################################################################
# Setup -------------------------------------------------------------------
###########################################################################

# ***********
# Parameters
# ***********

# 'data'   = Name of dataframe containing CT estimates
# 'column' = Name of dataframe column containing group ID (e.g.
#            species or population name)
# 'group'  = Character string containing name of group ID
# 'skews'  = Numeric vector containing skewness parameter(s)
# 'equiv_margin' = Equivalence of subsets to full population CT estimate
#                  (unit = degree Celcius)
# 'pop_n'  = Size of population to sample (will test subsamples of size
#            pop_n - x against pop_n for equivalence)
#          = Defaults to population size = 30

# Define function
equiv_tost = function(data,
                      column,
                      group,
                      y,
                      skews = c(0, 1, 2, 10, 50),
                      tte_cols = c("blue", "red", "orange", "forestgreen", "lightgrey"),
                      alpha_val = 0.3,
                      line_wd = 1.5,
                      equiv_margin = 1,
                      pop_n = 30,
                      ggtheme = theme_classic()) {

  # -------------------------------------------------------------------------
  # Setup: -----------------------------------------------------------------
  # -------------------------------------------------------------------------


  # Custom functions are taken from Duffy et al. 2021 (Functional Ecology)

  # FUNCTION: TOSTer
  TOSTer <- function(x, equiv_margin, set = NULL) {
    if (is.null(set))
    {
      spacdat <- x
    }  #if used on empirical data (i.e. one vector of CT data)
    if (is.numeric(set)) {
      spacdat <- skew_dists[[set]][x][[1]]
    }
    # if used on simulated data where the 'set' is required
    # for subsetting

    ChenTOST <-
      function(sub,
               spacdat,
               full_mn,
               full_sd,
               equiv_margin) {
        # modified CHEN TOST procedure run all combinations
        # if < 10 000 possible combinations
        if (choose(length(spacdat), sub) < 10000) {
          nsub <- choose(length(spacdat), sub)
          comX <- combn(1:length(spacdat), sub)
          smpl_mn <-
            split(comX, rep(1:ncol(comX), each = nrow(comX)))
        } else {
          # if there are more than 10 000 possible
          # combinations, run a random subset of 10 000
          nsub <- 10000
          smpl_mn <- lapply(1:nsub, function(x) {
            sample(1:length(spacdat), sub)
          })
        }

        equiv <- sapply(smpl_mn, function(x) {
          tryCatch({
            # EnvStats::chenTTest #two one-sided
            # t-tests for mean equivalence try a Chen
            # t-test on one end of distribution
            mn_upr <-
              EnvStats::chenTTest(spacdat[x],
                                  alternative = "greater",
                                  mu = full_mn - equiv_margin)$p.value[3]
            # if data are not sufficiently skewed for
            # Chen t-test, run a standard t-test
            # instead
            if (is.na(mn_upr)) {
              mn_upr <- t.test(spacdat[x],
                               alternative = "greater",
                               mu = full_mn - equiv_margin)$p.value
            }
            mn_lwr <- t.test(spacdat[x],
                             alternative = "less",
                             mu = full_mn + equiv_margin)$p.value  #standard t-test for other end
            mn_rez <- c(mn_upr, mn_lwr)

            # EnvStats::varTest
            var_upr <-
              EnvStats::varTest(
                spacdat[x],
                alternative = "greater",
                sigma.squared = ifelse((full_sd - equiv_margin) >
                                         0,
                                       (full_sd - equiv_margin) ^ 2,
                                       1e-05
                ),
                conf.level = 0.95
              )$p.value
            var_lwr <-
              EnvStats::varTest(
                spacdat[x],
                alternative = "less",
                sigma.squared = (full_sd + equiv_margin) ^
                  2,
                conf.level = 0.95
              )$p.value
            var_rez <- c(var_upr, var_lwr)

            mnvar_equiv <- c(NA, NA)
            ifelse(sum(is.na(mn_rez)) > 0, {
              mnvar_equiv[1] <- NA
            }, {
              mnvar_equiv[1] <- max(mn_rez) < 0.05
            })
            ifelse(sum(is.na(var_rez)) > 0, {
              mnvar_equiv[2] <- NA
            }, {
              mnvar_equiv[2] <- max(var_rez) < 0.05
            })

            return(mnvar_equiv)
            # error handling if TOST procedure fails
            # (e.g. if data contain too many non-unique
            # values)\t
          }, error = function(z) {
            return(c(NA, NA))
          })
        })

        mn_eq <- equiv[1, ]
        var_eq <- equiv[2, ]
        # calculate proportion of succesful tests
        return(c(
          length(mn_eq[which(mn_eq == T)]) / sum(is.finite(mn_eq)),
          length(var_eq[which(var_eq == T)]) / sum(is.finite(var_eq))
        ))
      }

    res <- do.call(
      cbind,
      lapply(
        3:length(spacdat),
        ChenTOST,
        spacdat = spacdat,
        full_mn = mean(spacdat),
        full_sd = sd(spacdat),
        equiv_margin = equiv_margin
      )
    )  #only doing subsets where n > 3
    res2return <- cbind(1:length(spacdat), t(cbind(matrix(
      NA,
      nrow = 2, ncol = 2
    ), res)))
    colnames(res2return) <- c("n", "mn_eq", "var_eq")
    return(data.frame(res2return))
  }

  # Set vector of seeds for simulating replicated datasets (n = 50 datasets)
  pop_seeds <- c(
    809560L,
    854479L,
    16380L,
    856738L,
    42793L,
    949638L,
    349154L,
    680772L,
    904048L,
    32917L,
    846553L,
    23701L,
    673766L,
    538887L,
    373079L,
    888918L,
    244202L,
    598545L,
    671795L,
    530864L,
    944901L,
    489945L,
    21730L,
    380536L,
    854547L,
    482305L,
    628216L,
    892896L,
    198276L,
    988513L,
    296083L,
    262567L,
    209831L,
    76535L,
    257664L,
    198760L,
    523672L,
    174167L,
    129254L,
    916874L,
    288318L,
    408110L,
    212368L,
    18343L,
    963653L,
    102179L,
    652356L,
    779200L,
    109124L,
    253919L
  )

  # Create subset of data
  # - Need to extract a vector of CTmin/CTmax estimates
  # - {{ data }} - Dataframe or tibble
  # - {{ group }} - Group identifier of study population
  # - {{ y }} - Column name containing CTmin/CTmax estimates
  df <- {{ data }} %>%
    dplyr::filter( {{ column }} %in% {{ group }} ) %>%
    dplyr::pull( {{ y }} )

  # Set parameters for source population(s) and equivalence testing
  # - Change as required.
  mn <- mean(df)                      # Mean of population(s)
  stdev <- sd(df)                     # Standard deviation of population(s)
  skews <- {{ skews }}                # Skew of population(s)
  equiv_margin <-  {{ equiv_margin }} # Equivalence margin for equivalence testing (celcius)

  # -------------------------------------------------------------------------
  # Generate simulated populations: -----------------------------------------
  # -------------------------------------------------------------------------

  # Generate randomly drawn population(s) comprising {{ pop_n }} individuals
  # - E.g. if pop_n = 30, each population will comprise up to 30 individuals
  # A number of different populations with characteristic skewness parameters
  # are drawn to account for deficiencies with bootstrapping whereby we could
  # sample out of the tail of the simulated distribution.
  skew_dists <- lapply(skews, function(skw, stdev, seeds) {
    lapply(seeds, function(seed) {
      set.seed(seed)
      rsn({
        {
          pop_n
        }
      },
      xi = mn,
      omega = stdev,
      alpha = skw)
    })
  }, stdev = stdev, seeds = pop_seeds)

  # -------------------------------------------------------------------------
  # Run TOSTer simulations: -------------------------------------------------
  # -------------------------------------------------------------------------

  # Run simulations
  tost_res <- parallel::mclapply(1:length(skews), function(x) {
    do.call(rbind,
            parallel::mclapply(
              1:length(pop_seeds),
              TOSTer,
              equiv_margin = equiv_margin,
              set = x
            ))
  })

  # -------------------------------------------------------------------------
  # Process simulation output: ----------------------------------------------
  # -------------------------------------------------------------------------

  # Summarise and format data for plotting (originally = 100)
  plot_dat_mean <-
    do.call(rbind, lapply(1:length(skews), function(x) {
      data.frame(
        nsamp = 1:{
          {
            pop_n
          }
        },
        mn = sapply(1:{
          {
            pop_n
          }
        }, function(n) {
          mean(tost_res[[x]][which(tost_res[[x]][, "n"] == n),
                             "mn_eq"])
        }),
        stdev = sapply(1:{
          {
            pop_n
          }
        }, function(n) {
          sd(tost_res[[x]][which(tost_res[[x]][, "n"] == n), "mn_eq"])
        }),
        grp = skews[x]
      )
    }))

  # Calculate confidence intervals for mean estimates
  plot_dat_mean <- plot_dat_mean %>%
    dplyr::group_by(grp, nsamp) %>%
    dplyr::reframe(
      mn = mn,
      stdev = stdev,
      ymin = mn - stdev,
      ymax = mn + stdev
    ) %>%
    dplyr::mutate(ymin = dplyr::case_when(ymin < 0 ~ 0,
                                          ymin > 0 & ymin <= 1 ~ ymin,
                                          ymin > 1 ~ 1)) %>%
    dplyr::mutate(ymax = dplyr::case_when(ymax > 0 &
                                            ymax <= 1 ~ ymax,
                                          ymax > 1 ~ 1))


  # Make mean TOST plot
  plot_mean <- ggplot(
    data = plot_dat_mean,
    mapping = aes(
      x = nsamp,
      y = mn,
      ymin = ymin,
      ymax = ymax,
      fill = as.factor(grp),
      colour = as.factor(grp),
    )
  ) +
    scale_fill_manual(values = tte_cols) +
    scale_color_manual(values = tte_cols) +
    geom_ribbon(alpha = alpha_val, linetype = 0) +
    geom_line(size = line_wd) +
    labs(
      x = "Subsample size (n)",
      y = "Proportion equivalent",
      fill = "Skewness",
      subtitle = "(a) Equivalence of means"
    ) +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks=seq(round(max(plot_dat_mean$nsamp),0))) +
    theme(legend.position = "right") +
    guides(colour = "none") +
    ggtheme


  # Summarise and format data for plotting (originally = 100)
  plot_dat_var <-
    do.call(rbind, lapply(1:length(skews), function(x) {
      data.frame(
        nsamp = 1:{
          {
            pop_n
          }
        },
        var_est = sapply(1:{
          {
            pop_n
          }
        } , function(n) {
          mean(tost_res[[x]][which(tost_res[[x]][, "n"] == n),
                             "var_eq"])
        }),
        stdev = sapply(1:{
          {
            pop_n
          }
        } , function(n) {
          sd(tost_res[[x]][which(tost_res[[x]][, "n"] == n), "var_eq"])
        }),
        grp = skews[x]
      )
    }))

  # Calculate confidence intervals for mean estimates
  plot_var_dat <- plot_dat_var %>%
    dplyr::filter(!stdev  %in% NA) %>%
    dplyr::group_by(grp, nsamp) %>%
    dplyr::reframe(
      mn = var_est,
      stdev = stdev,
      ymin = var_est - stdev,
      ymax = var_est + stdev
    ) %>%
    dplyr::mutate(ymin = dplyr::case_when(ymin < 0 ~ 0,
                                          ymin > 0 & ymin <= 1 ~ ymin,
                                          ymin > 1 ~ 1)) %>%
    dplyr::mutate(ymax = dplyr::case_when(ymax > 0 &
                                            ymax <= 1 ~ ymax,
                                          ymax > 1 ~ 1))


  # Make var TOST plot
  plot_var <- ggplot(
    data = plot_var_dat,
    mapping = aes(
      x = nsamp,
      y = mn,
      ymin = ymin,
      ymax = ymax,
      fill = as.factor(grp),
      colour = as.factor(grp),
    )
  ) +
    scale_fill_manual(values = tte_cols) +
    scale_color_manual(values = tte_cols) +
    geom_ribbon(alpha = alpha_val, linetype = 0) +
    geom_line(size = line_wd) +
    labs(
      x = "Subsample size (n)",
      y = "Proportion equivalent",
      fill = "Skewness",
      subtitle = "(b) Equivalence of variances"
    ) +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks=seq(round(max(plot_var_dat$nsamp),0)), expand = c(0, 0), limits = c(0, NA)) +
    theme(legend.position = "right") +
    guides(colour = "none") +
    ggtheme

  # -------------------------------------------------------------------------
  # Make plot of results: ---------------------------------------------------
  # -------------------------------------------------------------------------

  # Combine plots
  plots <- cowplot::plot_grid(plot_mean, plot_var, nrow = 1)

  # Return combined plots
  return(plots)

} # End of function

#############################################################################
#############################################################################
#############################################################################
