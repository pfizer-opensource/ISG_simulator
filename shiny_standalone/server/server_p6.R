output$p6_text1   <- renderText({  paste0("This section is to simulate a modified ascending design trial. ")
})
output$p6_text2   <- renderText({  paste0("The period effects are generated randomly based on the mean and standard deviation you set 
                                           in the overall settings (in the left panel).")
})

##### p6_1 #####
#seed              <- reactive({input$p6_1_seed1})
p6_1_tab1         <- eventReactive(input$p6_1_run,  {   
  
  seed <- input$p6_1_seed1;
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
  n_per_grp_mod_asc   <- input$p6_1_n_per_seq
  
  source("script/functions/scen_in/0918/take_inputs.R",local = TRUE)$value
  source("script/functions/scen_in/0918/dz_mod_asc.R",local = TRUE)$value
  design_mod_asc
})
output$p6_1_tab1  <- renderTable({ p6_1_tab1()  })
output$p6_1_text1 <- renderText({   paste0("Table shows the baseline, period effects, random effects and baseline response (Y_BL) for each animal in the simulated ascending design trial.") })


##### p6_2 #####

p6_2_tab1 <- eventReactive(input$p6_2_run, {
  
  seed <- input$p6_1_seed1
  set.seed(seed)
  
  n_sim    <- input$p6_2_nsim
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
  n_per_grp_mod_asc   <- input$p6_2_n_per_seq
  
  source("script/functions/scen_in/0918/take_inputs.R",local = TRUE)$value
  source("script/functions/scen_in/0918/dz_mod_asc.R",local = TRUE)$value
  source('script/functions/scen_in/0918/f_dopar_pwr_modified_asc_in.R',local=TRUE)$value          ;list_mod_asc         <-list
  source('script/functions/scen_in/0918/f_dopar_tp1_modified_asc_in.R',local=TRUE)$value          ;tp1_mod_asc          <-list
  f_list_to_tb(n_trt = n_trt, pwrlist = list_mod_asc   , tp1list = tp1_mod_asc)
  
})
output$p6_2_tab1  <- render_gt({ p6_2_tab1()  })
