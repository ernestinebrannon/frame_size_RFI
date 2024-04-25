---
title: "Psychometric Parameter Search"
author: "Ernestine Brannon"
output: html_document 
---







```r
set.seed(2023)
#Define iterations and draw numbers 
n <- 100
parList <- list()

for (i in 1:n){
  pse_par <- runif(1, min = -15, max= 15)
  slope_par <- runif(1, min = 0 , max = 2)
  parList[[i]] <- c(pse_par, slope_par)
}

parDF <- do.call(rbind, parList)
colnames(parDF) <- c("pse", "slope")
```





```r
#for(i in names(list_of_dfs)){
#  write.csv(list_of_dfs[[i]], paste0(i,".csv"))
#}
```



```r
# optimal_parameters <- function(df){
#   return(as.data.frame(df[which.min(df$sse),]))
# }
# 
# lapply_pars <- lapply(list_of_dfs, optimal_parameters)
# best_pars <- bind_rows(lapply_pars, .id = "DataFrame_ID")
# rownames(best_pars) <- NULL
# 
# best_pars <- best_pars %>% 
#   separate_wider_delim(DataFrame_ID," ", names = c("frame_orientation", "frame_size", "sid"))
# 
# write.csv(best_pars, "optimized_parameters")
# ```
# ```{r}
# p_data_clean<-p_data_clean %>% 
#   mutate()
```

# ```{r PsyFun fit individual, include=FALSE,echo=FALSE}
# 
# individual_fits<- data.frame()
# 
# for( subject in unique(p_data_clean$sid)){ #outermost for loop to iterate through subjects
#   for( f_size in unique(p_data_clean$frame_size)){ # for each frame size 
#     for(frame_orientation in unique(p_data_clean$frame_orient)){ #and for each frame orientation
#         ps <- best_pars %>%
#           filter(sid==subject, frame_size==f_size,frame_orientation ==frame_orientation)
#         p1 <- ps$par1 
#         p2 <- ps$par2
#         
#         filter_df<-p_data_clean%>% 
#           filter(sid== subject ,frame_orient == frame_orientation, frame_size == f_size) #%>%
#           
#         
#         # Plot range - rod tilts
#         x_plotlims2 <- -20:20 
#       
#         pse_fits <- filter_df %>% 
#           group_by(sid, frame_orient, frame_size) %>%
#           nest() %>% 
#           
#           mutate(psy_fit = map(data, ~optim(par   = c(p1, p2), 
#                                             fn    = psy_fun, 
#                                             x_val = .$rod_tilt,
#                                             rsp   = .$response)),
#                
#                pse       = map_dbl(psy_fit, ~.$par[1]),
#                slope     = map_dbl(psy_fit, ~.$par[2]),
#                residual  = map_dbl(psy_fit, "value"),
#                curve_fit = map(psy_fit, ~psy_plot(x_val = x_plotlims2, c(.$par[1], .$par[2]))),
#                indv_pts  = map(data, ~get_indv_pts(x = .$rod_tilt, y = .$response))
#         ) %>% 
#         
#         arrange(sid, frame_orient, frame_size)
#       
#         individual_fits[[paste0(frame_orientation," ",f_size," ", subject)]]<- pse_fits
#     }
#    }    
# }
```


