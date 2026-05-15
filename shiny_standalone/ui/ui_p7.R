tabItem(tabName = "p7",
        tags$h2("7. Modified Ascending Design 1",style = "color: blue;  font-style: "),   # font-size: 20px;
        textOutput("p7_text1"),  tags$head(tags$style("#p7_text1{color:black; font-size: 15px; font-style:   }")),
        textOutput("p7_text2"),  tags$head(tags$style("#p7_text2{color:black; font-size: 15px; font-style:   }")), 
        
        tabsetPanel(
          
          ##### p7_0  #####
          tabPanel("Design Overview",
                   tags$img(src = "pics/des_mod_asc1.png", width = "50%")
          ),
          
          ##### p7_1  #####
          tabPanel("Data Structure",
                   
                   tags$h4("7.1 Simulate one modified ascending design 1",style = "color: blue;  font-style: "),   # font-size: 20px;
                   numericInput("p7_1_n_per_seq","Number of animals per seq", 4),
                   numericInput("p7_1_seed1","p7_1_seed1",  1234), 
                   actionButton("p7_1_run","Simulate 1 Trial"),
                   tableOutput("p7_1_tab1" ),
                   textOutput("p7_1_text1"),  tags$head(tags$style("#p7_text2{color:black; font-size: 15px; font-style:   }")) 
          ),
          
          ##### p7_2   #####
          tabPanel("Operating Characteristics",
                   
                   tags$h4("7.2 Simulate ascending trials",style = "color: blue;  font-style: "), # font-size: 17px; 
                   numericInput("p7_2_n_per_seq","Number of animals per seq", 4), 
                   numericInput("p7_2_nsim","p7_2_nsim", 5), 
                   actionButton("p7_2_run","Simulate Multi Trials"),
                   gt_output("p7_2_tab1" ),
                   textOutput("p7_2_text1"),  tags$head(tags$style("#p7_text2{color:black; font-size: 15px; font-style:   }")) 
                   
          )
          
          
          #####   
        )
)
