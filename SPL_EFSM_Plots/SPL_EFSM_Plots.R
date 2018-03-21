###########Create custom function and name variables##################################################
rm(list = ls(all = TRUE))
graphics.off()
# Custom_graph
custom_graph = function(data, title, variable, color) {
    plot(data, type = "l", xlab = "Time", main = title, ylab = variable, col = color, xaxt = "n")
    axis(1, at = seq(1, 359, 12), labels = seq(1988, 2017))
}

title = c("Date", "Expected Inflation (EI)", "Inflation (I)", "High-Yield Bonds (HYB)", 
          "Longterm Goverment Bonds (LGB)", "Change in yearly Production (YP)", 
          "Change in monthly production (MP)", "Unexpected Inflation (UI)", 
          "Change in Expected Inflation (DEI)", "Unanticipated changes in risk premia (URP)", 
          "Unanticipated changes in term structure (UTS)", "Equally weighted equities (EWNY)", 
          "Oil prices (OG)", "Consumption (CG)", "Return (R)")

name = c("Date", "EI", "I", "HYB", "LGB", "YP", "MP", "UI", "DEI", 
         "URP", "UTS", "EWNY", "OG", "CG", "R")

file_name = c("Date", "EI", "I", "HYB", "LGB", "YP", "SPL_EFSM_MP", "SPL_EFSM_UI", "SPL_EFSM_DEI", 
         "SPL_EFSM_URP", "SPL_EFSM_UTS", "SPL_EFSM_EWNY", "SPL_EFSM_OG", "SPL_EFSM_CG", "SPL_EFSM_R")

###########Creating plots############################################################################
#The code assumes that columns are in the exact specific order

big_table <- readRDS("~/EFSM/SPL_EFSM_Plots/big_table.RDS")

colr = c(1:6, 3, 4, 9:14, 3) # The colors for plots

for (i in 7:15){
    path = paste("~/EFSM/SPL_EFSM_Plots/", file_name[i] , ".pdf", sep = "")
    pdf( file = path, width = 6, height = 6)
    custom_graph(big_table[,i], title[i], name[i], colr[i])
    dev.off()
}
