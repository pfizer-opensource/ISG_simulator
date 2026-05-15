tabItem(tabName = "p3",
        tags$h2("3. Crossover Design",style = "color: blue;  font-style: "),   # font-size: 20px;
        textOutput("p3_text1"),  tags$head(tags$style("#p3_text1{color:black; font-size: 15px; font-style:   }")),
        textOutput("p3_text2"),  tags$head(tags$style("#p3_text2{color:black; font-size: 15px; font-style:   }")), 
        
        tabsetPanel(
          
          ##### p3_0  #####
          tabPanel("Design Overview",
                   tags$img(src = "pics/des_xov.png", width = "50%")
          ),

          ##### p3_1  #####
          tabPanel("Data Structure",
                   
                   tags$h4("3.1 Simulate one crossover design",style = "color: blue;  font-style: "),   # font-size: 20px;
                   numericInput("p3_1_n_per_seq","Number of animals per seq", 1),
                   numericInput("p3_1_seed1","p3_1_seed1",  1234), 
                   actionButton("p3_1_run","Simulate 1 Trial"),
                   tableOutput("p3_1_tab1" ),
                   textOutput("p3_1_text1"),  tags$head(tags$style("#p3_text2{color:black; font-size: 15px; font-style:   }")) 
          ),
          
          ##### p3_2   #####
          tabPanel("Operating Characteristics",
                   
                   tags$h4("3.2 Simulate crossover trials",style = "color: blue;  font-style: "), # font-size: 17px; 
                   numericInput("p3_2_n_per_seq","Number of animals per seq", 1),
                   numericInput("p3_2_nsim","p3_2_nsim", 5), 
                   actionButton("p3_2_run","Simulate Multi Trials"),
                   gt_output("p3_2_tab1" ),
                   textOutput("p3_2_text1"),  tags$head(tags$style("#P3_text2{color:black; font-size: 15px; font-style:   }")) 
                   
          ), 
          
          ##### p3_3   #####
          tabPanel("Power Analysis",
                   
                   tags$h4("3.3.1 Power Curve",style = "color: blue;  font-style: "), # font-size: 17px; 
                   numericInput("p3_3_n_per_seq_fix","Enter Number of Animals per Seq", 1), 
                   plotlyOutput("p3_3_plot1"),
                   tags$h5("Coordinates of labeled points (x = effect size, y = power)", style = "color: black;"),
                   tableOutput("p3_3_plot1_points_tb"),
                   
              #    tags$h4("Note: Below we provide simulation results under a particular scenario",style = "color: blue;  font-style: "), # font-size: 17px; 
              #      tags$h5("Number of animals per sequence = 1",style = "color: black;  font-style: italic;"),
              #      tags$h5("Within-Animal SD = 4.037",style = "color: black;  font-style: italic;"),
              #      tags$h5("Between-Animal SD = 16.47",style = "color: black;  font-style: italic;"),
              #      tags$h5("Proportion of between-animal variability accounted for by the baseline value = 0.8",style = "color: black;  font-style: italic;"),
              #      tags$h5("Number of Simulations = 5000",style = "color: black;  font-style: italic;"),
              #     tableOutput("p3_3_power_tb_sim"),
                   
                   
                   tags$h4("3.3.2  Sample Size vs Effect Size ",style = "color: blue;  font-style: "), # font-size: 17px; 
                   numericInput("p3_3_power_fix","Enter Target Power", 0.8),
                   plotlyOutput("p3_3_plot2"),
                   tags$h5("Coordinates of labeled points (x = sample size n, y = effect size \u03b4)", style = "color: black;"),
                   tableOutput("p3_3_plot2_points_tb")
                   
          ) 
          #####   
        )
)