output$p3_text1   <- renderText({  paste0("This section is to simulate a crossover design trial. ")
})
output$p3_text2   <- renderText({  paste0("The period effects are generated randomly based on the mean and standard deviation you set 
                                           in the overall settings (in the left panel).")
})

##### p3_1 #####
#seed              <- reactive({input$p3_1_seed1})
p3_1_tab1         <- eventReactive(input$p3_1_run,  {   
                                    
                                    seed <- input$p1_1_seed1;
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
                                    n_per_grp_xov       <- input$p3_1_n_per_seq
  
                                    source("script/functions/scen_in/0918/take_inputs.R",local = TRUE)$value
                                    source("script/functions/scen_in/0918/dz_xov.R",local = TRUE)$value
                                    design_xov
})
output$p3_1_tab1  <- renderTable({ p3_1_tab1()  })
output$p3_1_text1 <- renderText({   paste0("Table shows the baseline, period effects, random effects and baseline response (Y_BL) for each animal in the simulated crossover design trial.") })


##### p3_2 #####
 
p3_2_tab1 <- eventReactive(input$p3_2_run, {
  
                                    seed <- input$p3_1_seed1
                                    set.seed(seed)
                                    
                                    n_sim    <- input$p3_2_nsim
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
                                    n_per_grp_xov       <- input$p3_2_n_per_seq
  
                                    source("script/functions/scen_in/0918/take_inputs.R",local = TRUE)$value
                                    source("script/functions/scen_in/0918/dz_xov.R",local = TRUE)$value
                                    source('script/functions/scen_in/0918/f_dopar_pwr_xov_in.R',local=TRUE)$value          ;list_xov          <-list
                                    source('script/functions/scen_in/0918/f_dopar_tp1_xov_in.R',local=TRUE)$value          ;tp1_xov          <-list
                                    f_list_to_tb(n_trt = n_trt, pwrlist = list_xov   , tp1list = tp1_xov)
                  
})
output$p3_2_tab1  <- render_gt({ p3_2_tab1()  })

##### p3_3 #####

app_source("functions/plot/f_pwr_xov_plot_1.R", local = TRUE)

output$p3_3_plot1 <- renderPlotly({ 
  
  req(input$between_sd, input$P_bl, input$within_sd, input$p3_3_n_per_seq_fix, input$n_trt)
  
  sd_bt        <- input$between_sd
  P_bl         <- input$P_bl
  sd_within    <- input$within_sd
  n_per_seq_fix <- input$p3_3_n_per_seq_fix
  sd           <- sd_within
  
  f_pwr_xov_plot_1(
    delta_v = seq(3, 25, 0.1),
    sd = sd,
    n_per_trt_fix = n_per_seq_fix,
    n_trt = input$n_trt
  )
  
})

output$p3_3_plot1_points_tb <- renderTable({

  req(input$between_sd, input$P_bl, input$within_sd, input$p3_3_n_per_seq_fix, input$n_trt)

  sd <- input$within_sd
  n_per_seq_fix <- input$p3_3_n_per_seq_fix

  f_pwr_xov_points_tb_1(
    sd = sd,
    n_per_trt_fix = n_per_seq_fix,
    n_trt = input$n_trt,
    x_points = c(5, 10, 15, 20)
  )

}, striped = TRUE, bordered = TRUE, spacing = "s", colnames = FALSE)

output$p3_3_power_tb_sim = render_gt({ p3_power_tb_sim })

app_source("functions/plot/f_pwr_xov_plot_2.R", local = TRUE)

output$p3_3_plot2 <- renderPlotly({ 
  
  req(input$between_sd, input$P_bl, input$within_sd, input$p3_3_power_fix, input$n_trt)
  
  sd_bt        <- input$between_sd
  P_bl         <- input$P_bl
  sd_within    <- input$within_sd
  power_fix    <- input$p3_3_power_fix
  sd           <- sd_within
  
  f_pwr_xov_plot_2(
    n_v = 2:10,
    sd = sd,
    power_fix = power_fix,
    n_trt = input$n_trt
  )
  
})

output$p3_3_plot2_points_tb <- renderTable({

  req(input$between_sd, input$P_bl, input$within_sd, input$p3_3_power_fix, input$n_trt)

  sd_within <- input$within_sd
  power_fix <- input$p3_3_power_fix

  f_pwr_xov_points_tb_2(
    sd = sd_within, power_fix = power_fix, n_trt = input$n_trt, n_v = 2:10
  )

}, striped = TRUE, bordered = TRUE, spacing = "s", colnames = FALSE)