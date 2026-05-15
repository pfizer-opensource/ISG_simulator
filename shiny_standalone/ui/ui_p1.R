tabItem(tabName = "p1",
        tags$h2("1. Parallel Design",style = "color: blue;  font-style: "),   # font-size: 20px;
        textOutput("p1_text1"),  tags$head(tags$style("#p1_text1{color:black; font-size: 15px; font-style:   }")),
        textOutput("p1_text2"),  tags$head(tags$style("#p1_text2{color:black; font-size: 15px; font-style:   }")), 

        tabsetPanel( 

          ##### p1_0  #####
          tabPanel("Design Overview",
                   tags$img(src = "pics/des_par_bl.png", width = "50%")
          ),

          ##### p1_1  #####
          tabPanel("Data Structure",
                   
                   tags$h4("1.1 Simulate one parallel design",style = "color: blue;  font-style: "),   # font-size: 20px;
                   numericInput("p1_1_n_per_arm","Number of animals per arm", 4),
                   numericInput("p1_1_seed1","p1_1_seed1", 1234), 
                   actionButton("p1_1_run","Simulate 1 Trial"),
                   tableOutput("p1_1_tab1" ),
                   textOutput("p1_1_text1")
          ),
          
          ##### p1_2   #####
          tabPanel("Operating Characteristics",
                   
                   tags$h4("1.2 Simulate multiple trials",style = "color: blue;  font-style: "), # font-size: 17px; 
                   numericInput("p1_2_n_per_arm","Number of animals per arm", 4),
                   numericInput("p1_2_nsim","p1_2_nsim", 5), 
                   actionButton("p1_2_run_ii","View Simulation Result (w/ BL)"),
                   gt_output("p1_2_tab1_ii" ),
                   textOutput("p1_2_text1_ii"),  tags$head(tags$style("#p1_2_text1_ii{color:black; font-size: 15px; font-style:   }")) ,
                   
                   actionButton("p1_2_run_i","View Simulation Result (w/o BL)"),
                   gt_output("p1_2_tab1_i" ),
                   textOutput("p1_2_text1_i"),  tags$head(tags$style("#p1_2_text1_i{color:black; font-size: 15px; font-style:   }"))
                   
          ), 

          ##### p1_3   #####
          tabPanel("Power Analysis",
                   
                   tags$h4("1.3.1 Power Curve",style = "color: blue;  font-style: "), # font-size: 17px; 
                   numericInput("p1_3_n_per_arm_fix","Enter Number of Animals per Arm", 4), 
                   plotlyOutput("p1_3_plot1"),
                   tags$h5("Coordinates of labeled points (x = effect size, y = power)", style = "color: black;"),
                   tableOutput("p1_3_plot1_points_tb"),
                   
                #   tags$h4("Note: Below we provide simulation results under a particular scenario",style = "color: blue;  font-style: "), # font-size: 17px; 
                #    tags$h5("Number of animals per arm = 4",style = "color: black;  font-style: italic;"),
                #    tags$h5("Within-Animal SD = 4.037",style = "color: black;  font-style: italic;"),
                #    tags$h5("Between-Animal SD = 16.47",style = "color: black;  font-style: italic;"),
                #    tags$h5("Proportion of between-animal variability accounted for by the baseline value = 0.8",style = "color: black;  font-style: italic;"),
                #    tags$h5("Number of Simulations = 5000",style = "color: black;  font-style: italic;"),
                #   tableOutput("p1_3_power_tb_sim"),

                   tags$h4("1.3.2  Sample Size vs Effect Size ",style = "color: blue;  font-style: "), # font-size: 17px; 
                   numericInput("p1_3_power_fix","Enter Target Power", 0.8),
                    plotlyOutput("p1_3_plot2"),
                   tags$h5("Coordinates of labeled points (x = sample size n, y = effect size \u03b4)", style = "color: black;"),
                   tableOutput("p1_3_plot2_points_tb")

          ) 

          #####



        )
)