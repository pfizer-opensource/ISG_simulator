n_trt                    <- reactive({  input$n_trt })
output$input_trt_eff     <- renderUI({
                                        default_vals <- c(0, 5, 10, 20)
                                        input_list <- lapply(1:n_trt(), function(i) {
                                          def <- if (i <= length(default_vals)) default_vals[i] else ""
                                          textInput(paste0("trt_", i), label = paste("Group", i, "Effect"), value = def)
                                        })
                                        do.call(tagList, input_list)
})
output$input_period_eff  <- renderUI({
                                        input_list <- lapply(1:n_trt(), function(i) {
                                          textInput(paste0("beta_", i), label = paste("Period/Day", i, "Effect"))
                                        })
                                        do.call(tagList, input_list)
})
tau_vec                  <- reactive({ sapply(1:n_trt(), function(i) input[[paste0("trt_", i)]])  })
tau_vec_0                <- reactive({ rep(0,n_trt()) })  # set the first treatment effect to 0
beta_vec                 <- reactive({    
                                        # fixed period effect
  
                                        if (input$period_eff_type == "fixed") {
                                          sapply(1:n_trt(), function(i) input[[paste0("beta_", i)]]) 
                                        } else {
                                          
                                        # random period effect
                                          n_day               <- n_trt() # suppose a square design
                                          beta_mean           <- input$beta_mean
                                          beta_sd             <- input$beta_sd
                                          set.seed(input$seed_beta)
                                          beta_vec_           <- c(0,rnorm(n_day-1,beta_mean,beta_sd) )
                                          
                                          round(beta_vec_,3)
                                        }
})

output$disp_tau_vec      <- renderText({  paste("Treatment Effect vector under H1:   ", paste(tau_vec()  , collapse = "," ) ) })
output$disp_tau_vec_0    <- renderText({  paste("Treatment Effect vector under H0:   ", paste(tau_vec_0(), collapse = "," ) ) })
output$disp_beta_vec     <- renderText({  paste("Now period effect vector has been generated as :   ", paste(beta_vec()  , collapse = "," ) ) })
