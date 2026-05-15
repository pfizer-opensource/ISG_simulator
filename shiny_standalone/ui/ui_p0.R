tabItem(tabName = "p0",
        # app intro
        # overall settings
        fluidRow(
          
             box(title = "Define the Parameter of Interest",
                 width = 12,
                 column(3, selectInput("parameter","Parameter",multiple = F, choices=c( "QT" ), selected = "QT")),
                 column(3, numericInput("eta","An overal estimate of the parameter's baseline value" ,200) )
                 ),
             box(title = "Define Animal-related Variance",
                 width = 12,
                 column(3, numericInput("within_sd", "Within-Animal SD", 4.037 )),
                 column(3, numericInput("between_sd", "Between-Animal SD", 16.470)),
                 column(3, numericInput("P_bl", "Proportion of between-animal variability accounted for by the baseline value", 0.8))
                 ), 
          column(12, 
                 box(title = "Define Treatment Effects", 
                     width = 6, 
                     numericInput("n_trt","Number of treatment groups", 4, min = 2, max = 6, step = 1),   
                     uiOutput("input_trt_eff"),
                     textOutput("disp_tau_vec"),
                     textOutput("disp_tau_vec_0") 
                 ),
                 box(title = "Define Period/Day Effects",  
                     width = 6, 
                     selectInput("period_eff_type","Period Effect Type",multiple = F, choices=c( "fixed","random" ), selected = "random"),
                     # only show this panel if the period effect type is fixed
                     conditionalPanel(
                       condition = "input.period_eff_type == 'fixed'",
                       helpText("You have selected fixed period effects. Please input the effect for each period/day."),
                       uiOutput("input_period_eff") 
                     ),
                     # only show this panel if the period effect type is random
                     conditionalPanel(
                       condition = "input.period_eff_type == 'random'",
                       helpText("You have selected random period effects. Please input the mean and standard deviation for generating random effects."),
                       numericInput("beta_mean","beta_mean",  0.238),
                       numericInput("beta_sd","beta_sd", 1.526),
                       numericInput("seed_beta","seed",123 )
                     ),
                     textOutput("disp_beta_vec")         
                 ) 
               )#column 
          
        )#fluidRow
        
)#tabItem
