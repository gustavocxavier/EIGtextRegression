## Generate Output Table

# library(xtable)

eval_CS  <- readRDS("2_pipeline/5a_eigText_evaluation_CS.rds")
eval_Ind <- readRDS("2_pipeline/5b_eigText_evaluation_Industry.rds")
eval_LC  <- readRDS("2_pipeline/5c_eigText_evaluation_LifeCycle.rds")

eval_ALL <- list(eval_CS, eval_Ind, eval_LC)

DF <- data.frame(row_names = c("CS", "", "Ind", "", "LC", ""),
                 "Year 1" = rep("",6),
                 "Year 2" = rep("",6),
                 "Year 3" = rep("",6))

for (i in 1:3) {
  for (j in 1:3) {
    RMSE <- round(eval_ALL[[i]]$RMSE[j],3)
    p_value <- round(eval_ALL[[i]]$`t-stat`[[j]]$p.value,3)
    t_stat  <- paste0("(",round(eval_ALL[[i]]$`t-stat`[[j]]$statistic,3),")")

    if (p_value <= 0.01)         { RMSE <- paste(RMSE, "***")
    } else if (p_value <= 0.05 ) { RMSE <- paste(RMSE, "**")
    } else if (p_value <= 0.10 ) { RMSE <- paste(RMSE, "*")
    } else { RMSE <- as.character(RMSE) }

    if (i == 1) { df_line <- 1:2
    } else if (i == 2) { df_line <- 3:4
    } else { df_line <- 5:6 }

    DF[df_line,j+1] <- c(RMSE, t_stat)
  }
}
DF

print(xtable::xtable(DF), include.rownames = FALSE)
xlsx::write.xlsx(DF, "3_output/results/RMSE.xlsx")
