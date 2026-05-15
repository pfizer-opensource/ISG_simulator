output$p4_text1   <- renderText({  paste0("This section is to simulate a sequential design trial. ")
})
output$p4_text2   <- renderText({  paste0("The period effects are generated randomly based on the mean and standard deviation you set 
                                           in the overall settings (in the left panel).")
})

##### p4_1 #####
#seed              <- reactive({input$p4_1_seed1})
p4_1_tab1         <- eventReactive(input$p4_1_run,  { 
                                      
                                      seed <- input$p4_1_seed1;
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
                                      n_per_grp_seq       <- input$p4_1_n_per_seq
                                      
                                      source("script/functions/scen_in/0918/take_inputs.R",local = TRUE)$value
                                      source("script/functions/scen_in/0918/dz_seq.R",local = TRUE)$value
                                      design_seq
})
output$p4_1_tab1  <- renderTable({ p4_1_tab1()  })
output$p4_1_text1 <- renderText({   paste0("Table shows the baseline, period effects, random effects and baseline response (Y_BL) for each animal in the simulated sequential design trial.") })


##### p4_2 #####
 
p4_2_tab1 <- eventReactive(input$p4_2_run, {
  
                                      seed <- input$p4_1_seed1
                                      set.seed(seed)
                                      
                                      n_sim    <- input$p4_2_nsim
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
                                      n_per_grp_seq       <- input$p4_2_n_per_seq
                                      
                                      source("script/functions/scen_in/0918/take_inputs.R",local = TRUE)$value
                                      source("script/functions/scen_in/0918/dz_seq.R",local = TRUE)$value
                                      source('script/functions/scen_in/0918/f_dopar_pwr_seq_in.R',local=TRUE)$value          ;list_seq          <-list
                                      source('script/functions/scen_in/0918/f_dopar_tp1_seq_in.R',local=TRUE)$value          ;tp1_seq          <-list
                                      f_list_to_tb(n_trt = n_trt, pwrlist = list_seq   , tp1list = tp1_seq)
              
})
output$p4_2_tab1  <- render_gt({ p4_2_tab1()  })

##### p4_3 #####

app_source("functions/plot/f_pwr_seq_plot_1.R", local = TRUE)

output$p4_3_plot1 <- renderPlotly({ 
  
  req(input$between_sd, input$P_bl, input$within_sd, input$p4_3_n_per_seq_fix, input$n_trt)
  
  sd_bt        <- input$between_sd
  P_bl         <- input$P_bl
  sd_within    <- input$within_sd
  n_per_seq_fix <- input$p4_3_n_per_seq_fix
  sd           <- sd_within
  
  f_pwr_seq_plot_1(
    delta_v = seq(3, 25, 0.1),
    sd = sd,
    n_per_seq_fix = n_per_seq_fix,
    n_trt = input$n_trt
  )
  
})

output$p4_3_plot1_points_tb <- renderTable({

  req(input$between_sd, input$P_bl, input$within_sd, input$p4_3_n_per_seq_fix, input$n_trt)

  sd <- input$within_sd
  n_per_seq_fix <- input$p4_3_n_per_seq_fix

  f_pwr_seq_points_tb_1(
    sd = sd,
    n_per_seq_fix = n_per_seq_fix,
    n_trt = input$n_trt,
    x_points = c(5, 10, 15, 20)
  )

}, striped = TRUE, bordered = TRUE, spacing = "s", colnames = FALSE)

output$p4_3_power_tb_sim = render_gt({ p4_power_tb_sim })

app_source("functions/plot/f_pwr_seq_plot_2.R", local = TRUE)

output$p4_3_plot2 <- renderPlotly({ 
  
  req(input$between_sd, input$P_bl, input$within_sd, input$p4_3_power_fix, input$n_trt)
  
  sd_bt        <- input$between_sd
  P_bl         <- input$P_bl
  sd_within    <- input$within_sd
  power_fix    <- input$p4_3_power_fix
  sd           <- sd_within
  
  f_pwr_seq_plot_2(
    n_v = 2:10,
    sd = sd,
    power_fix = power_fix,
    n_trt = input$n_trt
  )
  
})

output$p4_3_plot2_points_tb <- renderTable({

  req(input$between_sd, input$P_bl, input$within_sd, input$p4_3_power_fix, input$n_trt)

  sd_within <- input$within_sd
  power_fix <- input$p4_3_power_fix

  f_pwr_seq_points_tb_2(
    sd = sd_within, power_fix = power_fix, n_trt = input$n_trt, n_v = 2:10
  )

}, striped = TRUE, bordered = TRUE, spacing = "s", colnames = FALSE)