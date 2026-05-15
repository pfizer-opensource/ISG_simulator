tabItem(tabName = "p6",
        tags$h2("6. Modified Ascending Design",style = "color: blue;  font-style: "),   # font-size: 20px;
        textOutput("p6_text1"),  tags$head(tags$style("#p6_text1{color:black; font-size: 15px; font-style:   }")),
        textOutput("p6_text2"),  tags$head(tags$style("#p6_text2{color:black; font-size: 15px; font-style:   }")), 
        
        tabsetPanel(
          
          ##### p6_0  #####
          tabPanel("Design Overview",
                   tags$img(src = "pics/des_mod_asc.png", width = "50%")
          ),
          
          ##### p6_1  #####
          tabPanel("Data Structure",
                   
                   tags$h4("6.1 Simulate one modified ascending design",style = "color: blue;  font-style: "),   # font-size: 20px;
                   numericInput("p6_1_n_per_seq","Number of animals per seq", 4),
                   numericInput("p6_1_seed1","p6_1_seed1",  1234), 
                   actionButton("p6_1_run","Simulate 1 Trial"),
                   tableOutput("p6_1_tab1" ),
                   textOutput("p6_1_text1"),  tags$head(tags$style("#p6_text2{color:black; font-size: 15px; font-style:   }")) 
          ),
          
          ##### p6_2   #####
          tabPanel("Operating Characteristics",
                   
                   tags$h4("6.2 Simulate ascending trials",style = "color: blue;  font-style: "), # font-size: 17px; 
                   numericInput("p6_2_n_per_seq","Number of animals per seq", 4), 
                   numericInput("p6_2_nsim","p6_2_nsim", 5), 
                   actionButton("p6_2_run","Simulate Multi Trials"),
                   gt_output("p6_2_tab1" ),
                   textOutput("p6_2_text1"),  tags$head(tags$style("#p6_text2{color:black; font-size: 15px; font-style:   }")) 
                   
          )
          
          
          #####   
        )
)