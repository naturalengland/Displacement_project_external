assign_size <- function(LE_GEAR, VE_LEN) {
  size <- ifelse(LE_GEAR == "DRB" & VE_LEN < 10, "SMA",
          ifelse(LE_GEAR == "DRB" & VE_LEN >= 10 & VE_LEN <= 18, "MED",
          ifelse(LE_GEAR == "DRB" & VE_LEN > 18, "LAR",
          ifelse(LE_GEAR == "OTB" & VE_LEN < 12, "SMA",
          ifelse(LE_GEAR == "OTB" & VE_LEN >= 12 & VE_LEN <= 18, "MED",
          ifelse(LE_GEAR == "OTB" & VE_LEN > 18, "LAR",  
          ifelse(LE_GEAR == "OTT" & VE_LEN < 12, "SMA",
          ifelse(LE_GEAR == "OTT" & VE_LEN >= 12 & VE_LEN <= 18, "MED",
          ifelse(LE_GEAR == "OTT" & VE_LEN > 18, "LAR",
          ifelse(LE_GEAR == "PTB" & VE_LEN < 12, "SMA",
          ifelse(LE_GEAR == "PTB" & VE_LEN >= 12 & VE_LEN <= 15, "MED",
          ifelse(LE_GEAR == "PTB" & VE_LEN > 15, "LAR",
          ifelse(LE_GEAR == "TBB" & VE_LEN < 12, "SMA",
          ifelse(LE_GEAR == "TBB" & VE_LEN >= 12 & VE_LEN <= 18, "MED",
          ifelse(LE_GEAR == "TBB" & VE_LEN > 18, "LAR",
"NON")))))))))))))))
  return(size)
}



eflalo$size <- assign_size(eflalo$LE_GEAR, eflalo$VE_LEN)




eflalo2$LE_WIDTH <- assign_WIDTH(eflalo2$LE_MET)
eflalo2$LE_WIDTH <-0
eflalo2$LE_WIDTH  <- ifelse(grepl("DRB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET), 6.096, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("DRB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET), 9.144, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("DRB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET), 9.144, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 76.2, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 45.72, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 106.68, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 45.72, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET),121.92, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 45.72, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 76.2, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 45.72, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 106.68, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 45.72, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET),121.92, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 45.72, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 30.48, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 76.2, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 38.1, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET),76.2, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 38.1, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("SPF", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("SPF", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 30.48, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("SPF", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 76.2, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("SPF", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 38.1, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("SPF", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET),76.2, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("SPF", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 38.1, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 30.48, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 76.2, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 38.1, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET),76.2, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 38.1, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("MOL", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 12.192, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("MOL", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 12.192, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("MOL", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 12.192, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("MOL", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 12.192, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("MOL", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET),18.288, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("MOL", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 18.288, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 121.92, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 182.88, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET),228.6, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("CEP", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 121.92, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 182.88, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET),228.6, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("LEM", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 91.44, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 137.16, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET),137.16, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("DEF", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 91.44, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET), 137.16, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("South", eflalo2$LE_MET),137.16, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("OTT", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET)& grepl("MIX", eflalo2$LE_MET)& grepl("Other", eflalo2$LE_MET), 60.96, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("PTB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET), 92.5, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("PTB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET), 137.5, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("TBB", eflalo2$LE_MET) & grepl("SMA", eflalo2$LE_MET), 8, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("TBB", eflalo2$LE_MET) & grepl("MED", eflalo2$LE_MET), 14, eflalo2$LE_WIDTH)
eflalo2$LE_WIDTH  <- ifelse(grepl("TBB", eflalo2$LE_MET) & grepl("LAR", eflalo2$LE_MET), 24, eflalo2$LE_WIDTH)







