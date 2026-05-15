tabItem(tabName = "p2",
        tags$h2("2. Parallel RM Design",style = "color: blue;  font-style: "),   # font-size: 20px;
        textOutput("p2_text1"),  tags$head(tags$style("#p2_text1{color:black; font-size: 15px; font-style:   }")),
        textOutput("p2_text2"),  tags$head(tags$style("#p2_text2{color:black; font-size: 15px; font-style:   }")), 
        
        tabsetPanel( 
          
          ##### p2_0  #####
          tabPanel("Design Overview",
                   tags$img(src = "pics/des_par_rm_bl.png", width = "50%")
          ),
          ##### p2_1  #####
          tabPanel("Data Structure",
                   
                   tags$h4("2.1 Simulate one parallel RM design",style = "color: blue;  font-style: "),   # font-size: 20px;
                   numericInput("p2_1_n_per_arm","Number of animals per arm", 4),
                   numericInput("p2_1_seed1","p2_1_seed1", 1234), 
                   actionButton("p2_1_run","Simulate 1 Trial"),
                   tableOutput("p2_1_tab1" ),
                   textOutput("p2_1_text1")
          ),
          
          ##### p2_2   #####
          tabPanel("Operating Characteristics",
                   
                   tags$h4("2.2 Simulate multiple trials",style = "color: blue;  font-style: "), # font-size: 17px; 
                   numericInput("p2_2_n_per_arm","Number of animals per arm", 4),
                   numericInput("p2_2_nsim","p2_2_nsim", 5), 
                   actionButton("p2_2_run_ii","View Simulation Result (w/ BL)"),
                   gt_output("p2_2_tab1_ii" ),
                   textOutput("p2_2_text1_ii"),  tags$head(tags$style("#p2_2_text1_ii{color:black; font-size: 15px; font-style:   }")), 
                   
                   actionButton("p2_2_run_i","View Simulation Result (w/o BL)"),
                   gt_output("p2_2_tab1_i" ),
                   textOutput("p2_2_text1_i"),  tags$head(tags$style("#p2_2_text1_i{color:black; font-size: 15px; font-style:   }"))
                   
          ) , 
          
          ##### p2_3   #####
          tabPanel("Power Analysis",
                   
                   tags$h4("2.3.1 Power Curve",style = "color: blue;  font-style: "), # font-size: 17px; 
                   numericInput("p2_3_n_per_arm_fix","Enter Number of Animals per Arm", 4), 
                   plotlyOutput("p2_3_plot1"),
                   tags$h5("Coordinates of labeled points (x = effect size, y = power)", style = "color: black;"),
                   tableOutput("p2_3_plot1_points_tb"),
                   
              #     tags$h4("Note: Below we provide simulation results under a particular scenario",style = "color: blue;  font-style: "), # font-size: 17px; 
              #      tags$h5("Number of animals per arm = 4",style = "color: black;  font-style: italic;"),
              #      tags$h5("Within-Animal SD = 4.037",style = "color: black;  font-style: italic;"),
              #      tags$h5("Between-Animal SD = 16.47",style = "color: black;  font-style: italic;"),
              #      tags$h5("Proportion of between-animal variability accounted for by the baseline value = 0.8",style = "color: black;  font-style: italic;"),
              #      tags$h5("Number of Simulations = 5000",style = "color: black;  font-style: italic;"),
              #    tableOutput("p2_3_power_tb_sim"),
                   
                   tags$h4("2.3.2  Sample Size vs Effect Size ",style = "color: blue;  font-style: "), # font-size: 17px; 
                   numericInput("p2_3_power_fix","Enter Target Power", 0.8),
                   plotlyOutput("p2_3_plot2"),
                   tags$h5("Coordinates of labeled points (x = sample size n, y = effect size \u03b4)", style = "color: black;"),
                   tableOutput("p2_3_plot2_points_tb")
                   
          ) 
          #####
        )
)