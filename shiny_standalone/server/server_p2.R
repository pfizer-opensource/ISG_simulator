
output$p2_text1   <- renderText({  paste0("This section is to simulate a parallel (repeated measures) design trial. ")
})
output$p2_text2   <- renderText({  paste0("The period effects are generated randomly based on the mean and standard deviation you set 
                                           in the overall settings (in the left panel).")
})

##### p2_1 #####
#seed              <- reactive({input$p2_1_seed1})
p2_1_tab1         <- eventReactive(input$p2_1_run,  {  
  
                                  seed <- input$p2_1_seed1;
                                  set.seed(seed) 
                                  
                                  n_trt               <- input$n_trt                             
                                  beta_vec            <- as.numeric(beta_vec()) 
                                  between_sd          <- input$between_sd
                                  P_bl                <- input$P_bl
                                  eta                 <- input$eta
                                  beta_bl             <- 1
                                  within_sd           <- input$within_sd 
                                  tau_vec             <- as.numeric(tau_vec())
                                  tau_vec_0           <- as.numeric(tau_vec_0()) 
                                  n_per_grp_par_rm    <- input$p2_1_n_per_arm
                                  
                                  source("script/functions/scen_in/0918/take_inputs.R",local = TRUE)$value
                                  source("script/functions/scen_in/0918/dz_par_rm.R",local = TRUE)$value
                                  design_par_rm  
})
output$p2_1_tab1  <- renderTable({ p2_1_tab1()  })
output$p2_1_text1 <- renderText({   paste0("Table shows the baseline, period effects, random effects and baseline response (Y_BL) for each animal in the simulated parallel RM design trial.") })


##### p2_2 #####
 
# w/o bl
p2_2_tab1_i <- eventReactive(input$p2_2_run_i, {
  
                                  seed <- input$p2_1_seed1
                                  set.seed(seed)
                                  
                                  n_sim    <- input$p2_2_nsim
                                  seed_vec <- ceiling(rnorm(n_sim, 2500, 3000))
                                  seed_vec <- ifelse(seed_vec < 0, -seed_vec, seed_vec)
                                  
                                  n_trt               <- input$n_trt                             
                                  beta_vec            <- as.numeric(beta_vec()) 
                                  between_sd          <- input$between_sd
                                  P_bl                <- input$P_bl
                                  eta                 <- input$eta
                                  beta_bl             <- 1
                                  within_sd           <- input$within_sd 
                                  tau_vec             <- as.numeric(tau_vec())
                                  tau_vec_0           <- as.numeric(tau_vec_0()) 
                                  n_per_grp_par_rm    <- input$p2_2_n_per_arm
  
                                  source("script/functions/scen_in/0918/take_inputs.R",local = TRUE)$value
                                  source("script/functions/scen_in/0918/dz_par_rm.R",local = TRUE)$value
                                  source('script/functions/scen_in/0918/f_dopar_pwr_par_rm_in.R',local=TRUE)$value  ; list_par_rm  <-list
                                  source('script/functions/scen_in/0918/f_dopar_tp1_par_rm_in.R',local=TRUE)$value  ; tp1_par_rm   <-list
                                  f_list_to_tb(n_trt = n_trt, pwrlist = list_par_rm, tp1list = tp1_par_rm)
   
})
output$p2_2_tab1_i  <- render_gt({ p2_2_tab1_i()  })


# w/ bl
p2_2_tab1_ii <- eventReactive(input$p2_2_run_ii, {
  

                                  seed <- input$p2_1_seed1
                                  set.seed(seed)
                                  
                                  n_sim    <- input$p2_2_nsim
                                  seed_vec <- ceiling(rnorm(n_sim, 2500, 3000))
                                  seed_vec <- ifelse(seed_vec < 0, -seed_vec, seed_vec)
                                  
                                  n_trt               <- input$n_trt                             
                                  beta_vec            <- as.numeric(beta_vec()) 
                                  between_sd          <- input$between_sd
                                  P_bl                <- input$P_bl
                                  eta                 <- input$eta
                                  beta_bl             <- 1
                                  within_sd           <- input$within_sd 
                                  tau_vec             <- as.numeric(tau_vec())
                                  tau_vec_0           <- as.numeric(tau_vec_0()) 
                                  n_per_grp_par_rm    <- input$p2_2_n_per_arm
  
                                  source("script/functions/scen_in/0918/take_inputs.R",local = TRUE)$value
                                  source("script/functions/scen_in/0918/dz_par_rm.R",local = TRUE)$value
                                  source('script/functions/scen_in/0918/f_dopar_pwr_par_rm_bl_in.R',local=TRUE)$value  ; list_par_rm_bl  <-list
                                  source('script/functions/scen_in/0918/f_dopar_tp1_par_rm_bl_in.R',local=TRUE)$value  ; tp1_par_rm_bl   <-list
                                  f_list_to_tb(n_trt = n_trt, pwrlist = list_par_rm_bl, tp1list = tp1_par_rm_bl)
  
})
output$p2_2_tab1_ii  <- render_gt({ p2_2_tab1_ii()  })

##### p2_3 #####

app_source("functions/plot/f_pwr_par_rm_overlay_plot_1.R", local = TRUE)

output$p2_3_plot1 <- renderPlotly({ 
  
  req(input$between_sd, input$P_bl, input$within_sd, input$p2_3_n_per_arm_fix, input$n_trt)
  
  sd_bt        <- input$between_sd
  P_bl         <- input$P_bl
  sd_within    <- input$within_sd
  n_per_arm_fix <- input$p2_3_n_per_arm_fix
  p             <- length(beta_vec())
  sd_bl         <- sqrt((sd_bt^2) * (1 - P_bl) + sd_within^2/p)
  sd_no_bl      <- sqrt(sd_bt^2 + sd_within^2/p)

  f_pwr_par_rm_overlay_plot_1( delta_v       = seq(3,25,.1), 
                                    sd_bl         = sd_bl, 
                                    sd_no_bl      = sd_no_bl, 
                                    n_per_arm_fix = n_per_arm_fix,
                                    n_trt         = input$n_trt,
                                    p             = p
  )
  
})

output$p2_3_plot1_points_tb <- renderTable({

  req(input$between_sd, input$P_bl, input$within_sd, input$p2_3_n_per_arm_fix, input$n_trt)

  sd_bt         <- input$between_sd
  P_bl          <- input$P_bl
  sd_within     <- input$within_sd
  n_per_arm_fix <- input$p2_3_n_per_arm_fix
  p             <- length(beta_vec())
  sd_bl         <- sqrt((sd_bt^2) * (1 - P_bl) + sd_within^2/p)
  sd_no_bl      <- sqrt(sd_bt^2 + sd_within^2/p)

  f_pwr_par_rm_overlay_points_tb_1(
    sd_bl = sd_bl,
    sd_no_bl = sd_no_bl,
    n_per_arm_fix = n_per_arm_fix,
    n_trt = input$n_trt,
    p = p,
    x_points = c(5, 10, 15, 20)
  )

}, striped = TRUE, bordered = TRUE, spacing = "s", colnames = FALSE)

output$p2_3_power_tb_sim = render_gt({ p2_power_tb_sim })


app_source("functions/plot/f_pwr_par_rm_overlay_plot_2.R", local = TRUE)

output$p2_3_plot2 <- renderPlotly({ 
  
  req(input$between_sd, input$P_bl, input$within_sd, input$p2_3_power_fix, input$n_trt)
  
  sd_bt        <- input$between_sd
  P_bl         <- input$P_bl
  sd_within    <- input$within_sd
  power_fix    <- input$p2_3_power_fix
  p             <- length(beta_vec())
  sd_bl        <- sqrt((sd_bt^2) * (1 - P_bl) + sd_within^2/p)
  sd_no_bl     <- sqrt(sd_bt^2 + sd_within^2/p)

  f_pwr_par_rm_overlay_plot_2( n_v=2:10, 
                                    sd_bl = sd_bl, 
                                    sd_no_bl = sd_no_bl, 
                                    power_fix = power_fix,
                                    n_trt     = input$n_trt
   )
  
})

output$p2_3_plot2_points_tb <- renderTable({

  req(input$between_sd, input$P_bl, input$within_sd, input$p2_3_power_fix, input$n_trt)

  sd_bt     <- input$between_sd
  P_bl      <- input$P_bl
  sd_within <- input$within_sd
  power_fix <- input$p2_3_power_fix
  p         <- length(beta_vec())
  sd_bl     <- sqrt((sd_bt^2) * (1 - P_bl) + sd_within^2/p)
  sd_no_bl  <- sqrt(sd_bt^2 + sd_within^2/p)

  f_pwr_par_rm_overlay_points_tb_2(
    sd_bl = sd_bl, sd_no_bl = sd_no_bl,
    power_fix = power_fix, n_trt = input$n_trt, p = p, n_v = 2:10
  )

}, striped = TRUE, bordered = TRUE, spacing = "s", colnames = FALSE)