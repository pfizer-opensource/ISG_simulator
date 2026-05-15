f_list_to_tb = function(n_trt,pwrlist ,tp1list ){
  
  if(n_trt==4){
    
                                  d41 = round(pwrlist$d41,3)
                                  d31 = round(pwrlist$d31,3)
                                  d21 = round(pwrlist$d21,3)
                                  d41_l = round(pwrlist$d41_l,3)
                                  d31_l = round(pwrlist$d31_l,3)
                                  d21_l = round(pwrlist$d21_l,3)
                                  d41_u = round(pwrlist$d41_u,3)
                                  d31_u = round(pwrlist$d31_u,3)
                                  d21_u = round(pwrlist$d21_u,3)
                                  bias_d41 = round(pwrlist$bias41,3)
                                  bias_d31 = round(pwrlist$bias31,3)
                                  bias_d21 = round(pwrlist$bias21,3)
                                  power41  = round(pwrlist$power41,3)
                                  power31  = round(pwrlist$power31,3)
                                  power21  = round(pwrlist$power21,3)
                                  p.cover_41 = round(pwrlist$p.cover41,3)
                                  p.cover_31 = round(pwrlist$p.cover31,3)
                                  p.cover_21 = round(pwrlist$p.cover21,3)
                                  tp1err4   = round(tp1list$tp1err4,3)
                                  tp1err3   = round(tp1list$tp1err3,3)
                                  tp1err2   = round(tp1list$tp1err2,3)
                                  
                                  tb_multi_sim=cbind.data.frame(  Effect   = c("High Dose","Mid Dose","Low Dose"),
                                                                  Estimate = c(d41,d31,d21),
                                                                  CI       = c(paste("(", d41_l, "," , d41_u, ")"), 
                                                                               paste("(", d31_l, "," , d31_u, ")"),
                                                                               paste("(", d21_l, "," , d21_u, ")")
                                                                  ),
                                                                  Bias     = c(bias_d41,bias_d31,bias_d21),
                                                                  Power    = c(power41,power31, power21),
                                                                  Cover.P  = c(p.cover_41,p.cover_31,p.cover_21),
                                                                  tp1err   = c(tp1err4, tp1err3,tp1err2)
                                  )%>% gt()
                                  
    
  }else if (n_trt==3){
    
    d41 = round(pwrlist$d41,3)
    d31 = round(pwrlist$d31,3)
    d41_l = round(pwrlist$d41_l,3)
    d31_l = round(pwrlist$d31_l,3)
    d41_u = round(pwrlist$d41_u,3)
    d31_u = round(pwrlist$d31_u,3)
    bias_d41 = round(pwrlist$bias41,3)
    bias_d31 = round(pwrlist$bias31,3)
    power41  = round(pwrlist$power41,3)
    power31  = round(pwrlist$power31,3)
    p.cover_41 = round(pwrlist$p.cover41,3)
    p.cover_31 = round(pwrlist$p.cover31,3)
    tp1err4   = round(tp1list$tp1err4,3)
    tp1err3   = round(tp1list$tp1err3,3)

    tb_multi_sim=cbind.data.frame(  Effect   = c("Higher Dose","Lower Dose"),
                                    Estimate = c(d41,d31),
                                    CI       = c(paste("(", d41_l, "," , d41_u, ")"), 
                                                 paste("(", d31_l, "," , d31_u, ")")
                                    ),
                                    Bias     = c(bias_d41,bias_d31),
                                    Power    = c(power41,power31),
                                    Cover.P  = c(p.cover_41,p.cover_31),
                                    tp1err   = c(tp1err4, tp1err3)
    )%>% gt()
                          
  }else if (n_trt==2){
     
    d41 = round(pwrlist$d41,3)
    d41_l = round(pwrlist$d41_l,3)
    d41_u = round(pwrlist$d41_u,3)
    bias_d41 = round(pwrlist$bias41,3)
    power41  = round(pwrlist$power41,3)
    p.cover_41 = round(pwrlist$p.cover41,3)
    tp1err4   = round(tp1list$tp1err4,3)
    
    tb_multi_sim=cbind.data.frame(  Effect   = c("Dosing Group"),
                                    Estimate = c(d41),
                                    CI       = c(paste("(", d41_l, "," , d41_u, ")")
                                    ),
                                    Bias     = c(bias_d41),
                                    Power    = c(power41),
                                    Cover.P  = c(p.cover_41),
                                    tp1err   = c(tp1err4)
    )%>% gt()
    
  }
  
  return(tb_multi_sim)
}
