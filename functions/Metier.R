library(vmstools)
library(dplyr)
library(tidyr)
library(icesVocab)

data("eflalo")
eflalo<-as.data.frame(eflalo)


eflalo$ANA<-0
eflalo$CEP<-0
eflalo$CRU<-0
eflalo$DEF<-0
eflalo$DWS<-0
eflalo$MOL<-0
eflalo$SPF<-0


perform_calculation <- function(data, category_mapping) {
  # Calculate the sums for each category and assign to existing columns
  for (category in names(category_mapping)) {
    if (any(category_mapping[[category]] %in% colnames(data))) {
      data[[category]] <- rowSums(data[, grepl(paste0(category_mapping[[category]], collapse = "|"), names(data))])
    }
  }
  
  # Calculate the total
  data$total <- rowSums(data[, grepl("LE_EURO_", names(data))])
  
  # Calculate the percentages for each category and assign to existing columns
  for (category in names(category_mapping)) {
    if (any(category_mapping[[category]] %in% colnames(data))) {
      data[[category]] <- data[[category]] / data$total
    }
  }
  
  return(data)
}

metier <- list(
  ANA = c("LE_EURO_SHD", "LE_EURO_TRS"),
  CEP = c("LE_EURO_CTL", "LE_EURO_OCC", "LE_EURO_OCT", "LE_EURO_SQC", "LE_EURO_SQE", "LE_EURO_SQU"),
  CRU = c("LE_EURO_CPR", "LE_EURO_CRE", "LE_EURO_CRW", "LE_EURO_CSH", "LE_EURO_LBE", "LE_EURO_LIO", "LE_EURO_NEP", "LE_EURO_PRA", "LE_EURO_SCR"),
  DEF = c("LE_EURO_ANF", "LE_EURO_BER", "LE_EURO_BIB", "LE_EURO_BLL", "LE_EURO_BSH", "LE_EURO_BSS", "LE_EURO_COD", "LE_EURO_COE", "LE_EURO_DAB", "LE_EURO_DGH", "LE_EURO_DGS", "LE_EURO_DGX", "LE_EURO_FLE", "LE_EURO_GAG", "LE_EURO_GUG", "LE_EURO_GUR", "LE_EURO_GUU", "LE_EURO_GUX", "LE_EURO_HAD", "LE_EURO_HAL", "LE_EURO_HKE", "LE_EURO_JOD", "LE_EURO_LEM", "LE_EURO_LEZ", "LE_EURO_LIN", "LE_EURO_LUM", "LE_EURO_MAK", "LE_EURO_MUL", "LE_EURO_MUR", "LE_EURO_PLA", "LE_EURO_PLE", "LE_EURO_POK", "LE_EURO_POL", "LE_EURO_RED", "LE_EURO_RJC", "LE_EURO_RJE", "LE_EURO_RJH", "LE_EURO_RJI", "LE_EURO_RJM", "LE_EURO_RJN", "LE_EURO_RJO", "LE_EURO_RJU", "LE_EURO_ROE", "LE_EURO_ROL", "LE_EURO_SBA", "LE_EURO_SBR", "LE_EURO_SBX", "LE_EURO_SKA", "LE_EURO_SKH", "LE_EURO_SMD", "LE_EURO_SOL", "LE_EURO_SOS", "LE_EURO_SYC", "LE_EURO_SYX", "LE_EURO_THR", "LE_EURO_TOP", "LE_EURO_TRI", "LE_EURO_TUR", "LE_EURO_WEG", "LE_EURO_WHG", "LE_EURO_WIT", "LE_EURO_WRA"),
  DWS = c("LE_EURO_ALC", "LE_EURO_ALF", "LE_EURO_ANT", "LE_EURO_TJX"),
  MOL = c("LE_EURO_CLX", "LE_EURO_CMM", "LE_EURO_PER", "LE_EURO_QSC", "LE_EURO_SCE", "LE_EURO_WHE"),
  SPF = c("LE_EURO_ANE", "LE_EURO_ATP", "LE_EURO_GAR", "LE_EURO_HER", "LE_EURO_JAX", "LE_EURO_MAC", "LE_EURO_PIL", "LE_EURO_SPR")
)

eflalo <- perform_calculation(eflalo, metier)

eflalo <- eflalo %>%
  mutate(LE_MET = case_when(
    LE_GEAR == "DRB" & DEF >= 0.7 ~ "DRB_DES",
    LE_GEAR == "DRB" & MOL >= 0.7 ~ "DRB_MOL",
    LE_GEAR == "DRB" & CRU >= 0.7 ~ "DRB_CRU",
    LE_GEAR == "DRB" & CRU < 0.7 & MOL < 0.7 & DEF < 0.7 ~ "DRB_SWD",
    LE_GEAR == "FPO" & ANA >= 0.7 ~ "FPO_ANA",
    LE_GEAR == "FPO" & CEP >= 0.7 ~ "FPO_CEP",
    LE_GEAR == "FPO" & CRU >= 0.7 ~ "FPO_CRU",
    LE_GEAR == "FPO" & DEF >= 0.7 ~ "FPO_DEF",
    LE_GEAR == "FPO" & MOL >= 0.7 ~ "FPO_MOL",
    LE_GEAR == "FPO" & ANA < 0.7 & CEP < 0.7 & CRU < 0.7 & DEF < 0.7 & MOL < 0.7 ~ "FPO_SPF",
    LE_GEAR == "GND" & ANA >= 0.7 ~ "GND_ANA",
    LE_GEAR == "GND" & DEF >= 0.7 ~ "GND_DEF",
    LE_GEAR == "GND" & ANA < 0.7 & DEF < 0.7 ~ "GND_SPF",
    LE_GEAR == "GNS" & ANA >= 0.7 ~ "GNS_ANA",
    LE_GEAR == "GNS" & CRU >= 0.7 ~ "GNS_CRU",
    LE_GEAR == "GNS" & DEF >= 0.7 ~ "GNS_DEF",
    LE_GEAR == "GNS" & DWS >= 0.7 ~ "GNS_DWS",
    LE_GEAR == "GNS" & DWS < 0.7 & ANA < 0.7 & CRU < 0.7 & DEF < 0.7 ~ "GNS_SPF",
    LE_GEAR == "GN" & ANA >= 0.7 ~ "GNS_ANA",
    LE_GEAR == "GN" & CRU >= 0.7 ~ "GNS_CRU",
    LE_GEAR == "GN" & DEF >= 0.7 ~ "GNS_DEF",
    LE_GEAR == "GN" & DWS >= 0.7 ~ "GNS_DWS",
    LE_GEAR == "GN" & DWS < 0.7 & ANA < 0.7 & CRU < 0.7 & DEF < 0.7 ~ "GNS_SPF",
    LE_GEAR == "LHP" & CEP >= 0.7 ~ "LHP_CEP",
    LE_GEAR == "LHP" & DEF >= 0.7 ~ "LHP_DEF",
    LE_GEAR == "LHP" & DWS >= 0.7 ~ "LHP_DWS",
    LE_GEAR == "LHP" & MOL >= 0.7 ~ "LHP_MOL",
    LE_GEAR == "LHP" & DWS < 0.7 & CEP < 0.7 & DEF < 0.7 & MOL < 0.7 ~ "LHP_SPF",
    LE_GEAR == "LX" & CEP >= 0.7 ~ "LHP_CEP",
    LE_GEAR == "LX" & DEF >= 0.7 ~ "LHP_DEF",
    LE_GEAR == "LX" & DWS >= 0.7 ~ "LHP_DWS",
    LE_GEAR == "LX" & MOL >= 0.7 ~ "LHP_MOL",
    LE_GEAR == "LX" & DWS < 0.7 & CEP < 0.7 & DEF < 0.7 & MOL < 0.7 ~ "LHP_SPF",
    LE_GEAR == "LLS" & ANA >= 0.7 ~ "LLS_ANA",
    LE_GEAR == "LLS" & DEF >= 0.7 ~ "LLS_DEF",
    LE_GEAR == "LLS" & DWS >= 0.7 ~ "LLS_DWS",
    LE_GEAR == "LLS" & MOL >= 0.7 ~ "LLS_MOL",
    LE_GEAR == "LLS" & ANA < 0.7 & DEF < 0.7 & DWS < 0.7 & MOL < 0.7 ~ "LLS_SPF",
    LE_GEAR == "MIS" & ANA >= 0.7 ~ "MIS_ANA",
    LE_GEAR == "MIS" & CEP >= 0.7 ~ "MIS_CEP",
    LE_GEAR == "MIS" & CRU >= 0.7 ~ "MIS_CRU",
    LE_GEAR == "MIS" & DEF >= 0.7 ~ "MIS_DEF",
    LE_GEAR == "MIS" & DWS >= 0.7 ~ "MIS_DWS",
    LE_GEAR == "MIS" & MOL >= 0.7 ~ "MIS_MOL",
    LE_GEAR == "MIS" & SPF >= 0.7 ~ "MIS_SPF",
    LE_GEAR == "MIS" & ANA < 0.7 & CEP< 0.7 & CRU <0.7 & DEF < 0.7 & DWS < 0.7 & MOL < 0.7 & SPF < 0.7 ~ "MIS_SWD",
    LE_GEAR == "OTB" & CEP >= 0.7 ~ "OTB_CEP",
    LE_GEAR == "OTB" & CRU >= 0.7 ~ "OTB_CRU",
    LE_GEAR == "OTB" & DEF >= 0.7 ~ "OTB_DEF",
    LE_GEAR == "OTB" & DWS >= 0.7 ~ "OTB_DWS",
    LE_GEAR == "OTB" & MOL >= 0.7 ~ "OTB_MOL",
    LE_GEAR == "OTB" & SPF >= 0.7 ~ "OTB_SPF",
    LE_GEAR == "OTB" & (CRU + DEF) >= 0.7 & CRU < 0.7 & DEF < 0.7 ~ "OTB_MCD",
    LE_GEAR == "OTB" & (CEP + DEF) >= 0.7 & CEP < 0.7 & DEF < 0.7 ~ "OTB_MCF",
    LE_GEAR == "OTB" & (DWS + DEF)>= 0.7 & DWS < 0.7 & DEF < 0.7 ~ "OTB_MDD",
    LE_GEAR == "OTB" & CEP< 0.7 & CRU <0.7 & DEF < 0.7 & DWS < 0.7 & MOL < 0.7 & SPF < 0.7 & (CRU + DEF) < 0.7 & (CEP + DEF)< 0.7 & (DWS + DEF)< 0.7 ~ "OTB_MPD",
    LE_GEAR == "OTM" & CEP >= 0.7 ~ "OTM_CEP",
    LE_GEAR == "OTM" & DEF >= 0.7 ~ "OTM_DEF",
    LE_GEAR == "OTM" & DWS >= 0.7 ~ "OTM_DWS",
    LE_GEAR == "OTM" & SPF >= 0.7 ~ "OTM_SPF",
    LE_GEAR == "OTM" & (CEP + DEF)>= 0.7 & CEP < 0.7 & DEF < 0.7 ~ "OTM_MCF",
    LE_GEAR == "OTM" & CEP< 0.7 & DEF < 0.7 & DWS < 0.7 & SPF < 0.7 & (CEP + DEF) < 0.7 ~ "OTM_MPD",
    LE_GEAR == "OTT" & CEP >= 0.7 ~ "OTT_CEP",
    LE_GEAR == "OTT" & CRU >= 0.7 ~ "OTT_CRU",
    LE_GEAR == "OTT" & DEF >= 0.7 ~ "OTT_DEF",
    LE_GEAR == "OTT" & DWS >= 0.7 ~ "OTT_DWS",
    LE_GEAR == "OTT" & MOL >= 0.7 ~ "OTT_MOL",
    LE_GEAR == "OTT" & SPF >= 0.7 ~ "OTT_SPF",
    LE_GEAR == "OTT" & CEP< 0.7 & CRU < 0.7 & DEF < 0.7 & DWS < 0.7 & MOL < 0.7 & SPF < 0.7 ~ "OTT_MCD",
    LE_GEAR == "PS" & DEF >= 0.7 ~ "PS_DEF",
    LE_GEAR == "PS" & (DEF+SPF) >= 0.7 & DEF < 0.7 ~ "PS_MPD",
    LE_GEAR == "PS" & DEF< 0.7 & (DEF+SPF)< 0.7 ~ "PS_SPF",
    LE_GEAR == "PTB" & CEP >= 0.7 ~ "PTB_CEP",
    LE_GEAR == "PTB" & CRU >= 0.7 ~ "PTB_CRU",
    LE_GEAR == "PTB" & DEF >= 0.7 ~ "PTB_DEF",
    LE_GEAR == "PTB" & (DEF+SPF) >= 0.7 & CEP < 0.7 & CRU < 0.7 & DEF < 0.7  ~ "PTB_MPD",
    LE_GEAR == "PTB" & (DEF+SPF) < 0.7 & CEP < 0.7 & CRU < 0.7 & DEF < 0.7  ~ "PTB_SPF",
    LE_GEAR == "SV" ~ "SV_DEF",
    LE_GEAR == "TBB" & CEP >= 0.7 ~ "TBB_CEP",
    LE_GEAR == "TBB" & CRU >= 0.7 ~ "TBB_CRU",
    LE_GEAR == "TBB" & DEF >= 0.7 ~ "TBB_DEF",
    LE_GEAR == "TBB" & MOL >= 0.7 ~ "TBB_MOL",
    LE_GEAR == "TBB"  & CEP < 0.7 & CRU < 0.7 & DEF < 0.7 & MOL < 0.7   ~ "TBB_MCD",
    TRUE ~ "NA"  # Default case when none of the above conditions are met
  ))
