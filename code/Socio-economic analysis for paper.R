#Socioeconomic analysis
rm(list=ls()) 
load("./processed_data/tacsatEflalo_ID.RData")
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ggpubr)
options(digits=10)
options(scipen = n)
tacsatEflalo$SI_DATE <- as.Date(tacsatEflalo$SI_DATE, format = "%d/%m/%Y")
tacsatEflalo$Month <- format(tacsatEflalo$SI_DATE, "%m")# Make month column
Tot_val <- round(sum(tacsatEflalo$LE_TOT_VAL), 2)
Tot_val#the total value for all vessels
Tot_kg <- round(sum(tacsatEflalo$LE_TOT_KG), 2)
Tot_kg#the total kg for all vessels
Tot_time <- round(sum(tacsatEflalo$TIME), 2)
Tot_time#the total time for all vessels
Tot_kwh <- round(sum(tacsatEflalo$kwh), 2)
Tot_kwh#the total kWh for all vessels

tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")#filter for HypHPMA
tacsatEflalo_ten <- subset(tacsatEflalo, Location == "Ten")#filter for 100km2 surrounding area
tacsatEflalo_twenty <- subset(tacsatEflalo, Location == "Twenty")#filter for 400km2 surrounding area
tacsatEflalo_boundary <- subset(tacsatEflalo, Location == "Boundary")#filter for 400km2 surrounding area
tacsatEflalo_outside <- subset(tacsatEflalo, Location == "Outside")#filter for 400km2 surrounding area

Tot_val_five <- round(sum(tacsatEflalo_five$LE_TOT_VAL), 2)
Tot_val_five#the total value for HypHPMA
Tot_kg_five <- round(sum(tacsatEflalo_five$LE_TOT_KG), 2)
Tot_kg_five#the total kg for all HypHPMA
Tot_time_five <- round(sum(tacsatEflalo_five$TIME), 2)
Tot_time_five#the total time for HypHPMA
Tot_kwh_five <- round(sum(tacsatEflalo_five$kwh), 2)
Tot_kwh_five#the total time for HypHPMA

#percentage of total fleet
(Tot_val_five/Tot_val)*100
(Tot_kg_five/Tot_kg)*100
(Tot_time_five/Tot_time)*100
(Tot_kwh_five/Tot_kwh)*100

unique_vessels<- unique(tacsatEflalo_five$VE_REF)
length(unique_vessels)#see how many vessels using HypHPMA

unique_combinations <- tacsatEflalo_five%>%
  distinct(VE_REF, LE_GEAR)#and how many gear types

tacsatEflalo_five %>%
  group_by(LE_GEAR) %>%
  summarize(num_vessels = n_distinct(VE_REF))#Some use both

#percentage of 27 vessels found within the HypHPMA

vessels_27 <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels

Tot_val_27 <- round(sum(vessels_27$LE_TOT_VAL), 2)
Tot_val_27#the total value for all 27 vessels using the HypHPMA
Tot_kg_27 <- round(sum(vessels_27$LE_TOT_KG), 2)
Tot_kg_27#the total kg for 27 vessels
Tot_time_27 <- round(sum(vessels_27$TIME), 2)
Tot_time_27#the total time for 27 vessels, inside and out of HypHPMA
Tot_kwh_27 <- round(sum(vessels_27$kwh), 2)
Tot_kwh_27#the total time for 27 vessels, inside and out of HypHPMA

#percentage for 27 vessels
(Tot_val_five/Tot_val_27)*100
(Tot_kg_five/Tot_kg_27)*100
(Tot_time_five/Tot_time_27)*100
(Tot_kwh_five/Tot_kwh_27)*100

#Now lets find the total value for each vessel in HypHPMA

summary <- tacsatEflalo_five %>%
  group_by(VE_REF) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh)
  )
summary
#Find average catch per vessel and SD
mean(summary$total_value)
sd(summary$total_value)

mean(summary$total_kg)
sd(summary$total_kg)

mean(summary$total_time)
sd(summary$total_time)

mean(summary$total_kwh)
sd(summary$total_kwh)

#find average percentage of total catch in HypHPMA
summary_27 <- vessels_27 %>%
  group_by(VE_REF) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh)
  )

percentage_27 <- merge(summary, summary_27, by = "VE_REF", suffixes = c("_1", "_2"))
percentage_27<-percentage_27 %>%
  mutate(total_kg = (total_kg_1 / total_kg_2)*100,
         total_value = (total_value_1 / total_value_2)*100,
         total_time = (total_time_1 / total_time_2)*100,
         total_kwh = (total_kwh_1 / total_kwh_2)*100) 

percentage_27<- percentage_27[, c("VE_REF", "total_value", "total_kg", "total_time","total_kwh")]
arrange(percentage_27, desc(total_value))#arrange to see the pecentages >10%
arrange(percentage_27, desc(total_kg))#same for weight

mean(percentage_27$total_value)
sd(percentage_27$total_value)

mean(percentage_27$total_kg)
sd(percentage_27$total_kg)

mean(percentage_27$total_time)
sd(percentage_27$total_time)

mean(percentage_27$total_kwh)
sd(percentage_27$total_kwh)

#Also group for fishing trip to do a Kruskal Wallis test

summary <- tacsatEflalo_five %>%
  group_by(VE_REF, FT_REF) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh)
  )
kruskal.test(total_value ~ VE_REF, data = summary)#tests for significant differences between vessels
kruskal.test(total_kg ~ VE_REF, data = summary)
kruskal.test(total_time ~ VE_REF, data = summary)
kruskal.test(total_kwh ~ VE_REF, data = summary)

library(reshape2)
#Now look at top fish species within HypHPMA
species_value <- tacsatEflalo_five %>% dplyr::select(dplyr::contains("LE_EURO"))
species_value <- colSums(species_value , na.rm = TRUE)
species_value<- reshape2::melt(species_value)

#have summarized by species, now lets look at the highest values
species_value<- arrange(species_value, desc(value))
head(species_value, 5)

#Now look do the same for weights
species_kg <- tacsatEflalo_five %>% dplyr::select(dplyr::contains("LE_KG"))
species_kg <- colSums(species_kg , na.rm = TRUE)
species_kg<- reshape2::melt(species_kg)

#have summarized by species, now lets look at the highest values
species_kg<- arrange(species_kg, desc(value))
head(species_kg, 5)

#Also look at percentages in surrounding areas
summary_ten<- subset(tacsatEflalo_ten, VE_REF %in% unique_vessels)#filter down to 27 vessels
summary_twenty<- subset(tacsatEflalo_twenty, VE_REF %in% unique_vessels)#filter down to 27 vessels
summary_boundary<- subset(tacsatEflalo_boundary, VE_REF %in% unique_vessels)#filter down to 27 vessels
summary_outside<- subset(tacsatEflalo_outside, VE_REF %in% unique_vessels)#filter down to 27 vessels

(sum(summary_ten$LE_TOT_VAL)/sum(vessels_27$LE_TOT_VAL)*100)
(sum(summary_twenty$LE_TOT_VAL)/sum(vessels_27$LE_TOT_VAL)*100)
(sum(summary_boundary$LE_TOT_VAL)/sum(vessels_27$LE_TOT_VAL)*100)
(sum(summary_outside$LE_TOT_VAL)/sum(vessels_27$LE_TOT_VAL)*100)

(sum(summary_ten$LE_TOT_KG)/sum(vessels_27$LE_TOT_KG)*100)#same for weight
(sum(summary_twenty$LE_TOT_KG)/sum(vessels_27$LE_TOT_KG)*100)
(sum(summary_boundary$LE_TOT_KG)/sum(vessels_27$LE_TOT_KG)*100)
(sum(summary_outside$LE_TOT_KG)/sum(vessels_27$LE_TOT_KG)*100)

#Monthly analysis
summary <- tacsatEflalo_five %>%
  group_by(Month) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh)
  )#summarize based on month
#Now for kruskal wallis depending on month

summary <- tacsatEflalo_five %>%
  group_by(FT_REF, Month) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh)
  )#summarize based on month

kruskal.test(total_value ~ Month, data = summary)#tests for significant differences between vessels
kruskal.test(total_kg ~ Month, data = summary)
kruskal.test(total_time ~ Month, data = summary)
kruskal.test(total_kwh ~ Month, data = summary)

pairwise.wilcox.test(summary$total_value, summary$Month,
                     p.adjust.method = "BH")#pairwise to see which months differ
October <- subset(summary, Month == "10")
(sum(October$total_value)/sum(summary$total_value))*100#find percentage of catch value in October
(sum(October$total_kg)/sum(summary$total_kg))*100#same for weight
#Now look at July (lowest catch)
July <- subset(summary, Month == "07")
(sum(July$total_value)/sum(summary$total_value))*100#find percentage of catch value in October
(sum(July$total_kg)/sum(summary$total_kg))*100#same for weight

species_month<- tacsatEflalo_five %>%
  select(contains("LE_EURO") | contains("Month"))

species_month <- species_month%>%
  group_by(Month) %>%
  summarise_all(sum)

species_month<- species_month %>%
  pivot_longer(cols = -c(Month), names_to = "Species", values_to = "Value")

max_values <- species_month %>%
  group_by(Month) %>%
  filter(Value == max(Value))

print(max_values)#This gives you the species with the highest value for each month

#Same for weight
species_month<- tacsatEflalo_five %>%
  select(contains("LE_KG") | contains("Month"))

species_month <- species_month%>%
  group_by(Month) %>%
  summarise_all(sum)

species_month<- species_month %>%
  pivot_longer(cols = -c(Month), names_to = "Species", values_to = "Value")

max_values <- species_month %>%
  group_by(Month) %>%
  filter(Value == max(Value))

print(max_values)

#Now to make bar charts of average value per individual vessels
#This is based only on the vessels using the HypHPMA for that month
summary <- tacsatEflalo_five %>%
  group_by(VE_REF, Month) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh))

summary <- summary %>%
  group_by(Month) %>%
  summarise(
    mean_value = mean(total_value),
    se_value = sd(total_value) / sqrt(n()),
    mean_weight = mean(total_kg),
    se_weight = sd(total_kg) / sqrt(n()),
    mean_time = mean(total_time),
    se_time = sd(total_time) / sqrt(n()),
    mean_kwh = mean(total_kwh),
    se_kwh = sd(total_kwh) / sqrt(n()),
  )
summary$Month <- month.abb[as.numeric(summary$Month)]
summary$Month <- factor(summary$Month, levels = month.abb) 

Value<-ggplot(summary, aes(x = Month, y = mean_value)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2) +
  labs(
       x = "Month",
       y = "Average Catch Value (£)") +
  theme_classic()

Weight<-ggplot(summary, aes(x = Month, y = mean_weight)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.2) +
  labs(
    x = "Month",
    y = "Average Catch Weight (kg)") +
  theme_classic()

Time<-ggplot(summary, aes(x = Month, y = mean_time)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_time - se_time, ymax = mean_time + se_time), width = 0.2) +
  labs(
    x = "Month",
    y = "Average time spent fishing (hrs)") +
  theme_classic()

Kwh<-ggplot(summary, aes(x = Month, y = mean_kwh)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), width = 0.2) +
  labs(
    x = "Month",
    y = "Average Effort whilst fishing (kWh)") +
  theme_classic()

All_plot <- ggarrange(Value, Weight, Time, Kwh,
                 labels = c("A.", "B.", "C.", "D."),    # Plot labels
                 ncol = 2, nrow = 2,                    # Arrange in a 2x2 grid
                 label.x = 0.01,                        # Adjust the x position of the labels
                 label.y = 0.98) 
All_plot

#Now for stacked plots depending on gear type
summary <- tacsatEflalo_five %>%
  group_by(VE_REF, Month, LE_GEAR) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh))

summary <- summary %>%
  group_by(Month, LE_GEAR) %>%
  summarise(
    mean_value = mean(total_value),
    se_value = sd(total_value) / sqrt(n()),
    mean_weight = mean(total_kg),
    se_weight = sd(total_kg) / sqrt(n()),
    mean_time = mean(total_time),
    se_time = sd(total_time) / sqrt(n()),
    mean_kwh = mean(total_kwh),
    se_kwh = sd(total_kwh) / sqrt(n()),
  )
summary$Month <- month.abb[as.numeric(summary$Month)]
summary$Month <- factor(summary$Month, levels = month.abb) 


Value<-ggplot(summary, aes(x = Month, y = mean_value, fill= LE_GEAR)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Catch Value (£)",
    fill = "Gear") +
  theme_classic()

Weight<-ggplot(summary, aes(x = Month, y = mean_weight, fill=LE_GEAR)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Catch Weight (KG)",
    fill = "Gear") +
  theme_classic()

Time<-ggplot(summary, aes(x = Month, y = mean_time, fill= LE_GEAR)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_time - se_time, ymax = mean_time + se_time), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average time spent fishing (hrs)",
    fill = "Gear") +
  theme_classic()

Kwh<-ggplot(summary, aes(x = Month, y = mean_kwh, fill = LE_GEAR)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Effort whilst fishing (kWh)",
    fill = "Gear") +
  theme_classic()

All_plot <- ggarrange(Value, Weight, Time, Kwh,
                      labels = c("A.", "B.", "C.", "D."),    # Plot labels
                      ncol = 2, nrow = 2,                    # Arrange in a 2x2 grid
                      label.x = -0.01,                        # Adjust the x position of the labels
                      label.y = 0.99) 
All_plot

tacsatEflalo_five$VE_LEN<-as.numeric (tacsatEflalo_five$VE_LEN)
#Now we are going to do the same for vessel size
tacsatEflalo_five <- tacsatEflalo_five %>%
  mutate(Size = ifelse(VE_LEN >= 12, "≥12m", "<12m"))

summary <- tacsatEflalo_five %>%
  group_by(VE_REF, Month, Size) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh))

summary <- summary %>%
  group_by(Month, Size) %>%
  summarise(
    mean_value = mean(total_value),
    se_value = sd(total_value) / sqrt(n()),
    mean_weight = mean(total_kg),
    se_weight = sd(total_kg) / sqrt(n()),
    mean_time = mean(total_time),
    se_time = sd(total_time) / sqrt(n()),
    mean_kwh = mean(total_kwh),
    se_kwh = sd(total_kwh) / sqrt(n()),
  )
summary$Month <- month.abb[as.numeric(summary$Month)]
summary$Month <- factor(summary$Month, levels = month.abb) 


Value<-ggplot(summary, aes(x = Month, y = mean_value, fill= Size)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Catch Value (£)",
    fill = "Vessel size") +
  theme_classic()

Weight<-ggplot(summary, aes(x = Month, y = mean_weight, fill= Size)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Catch Weight (KG)",
    fill = "Vessel size") +
  theme_classic()

Time<-ggplot(summary, aes(x = Month, y = mean_time, fill= Size)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_time - se_time, ymax = mean_time + se_time), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average time spent fishing (hrs)",
    fill = "Vessel size") +
  theme_classic()

Kwh<-ggplot(summary, aes(x = Month, y = mean_kwh, fill = Size)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Effort whilst fishing (kWh)",
    fill = "Vessel Size") +
  theme_classic()


All_plot <- ggarrange(Value, Weight, Time, Kwh,
                      labels = c("A.", "B.", "C.", "D."),    # Plot labels
                      ncol = 2, nrow = 2,                    # Arrange in a 2x2 grid
                      label.x = -0.01,                        # Adjust the x position of the labels
                      label.y = 0.99) 
All_plot


#Now lets base this on an average of all 27 vessels
# Perform the summarization
template <- expand.grid(VE_REF = unique(tacsatEflalo_five$VE_REF), 
                        Month = unique(tacsatEflalo_five$Month))

# If vessel isn't present for that month then a 0 will be assigned
summary <- tacsatEflalo_five %>%
  group_by(VE_REF, Month) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh)) %>%
  # Right join with the template to fill missing combinations
  right_join(template, by = c("VE_REF", "Month")) %>%
  # Replace NA values with 0
  mutate(across(starts_with("total_"), replace_na, replace = 0))

summary <- summary %>%
  group_by(Month) %>%
  summarise(
    mean_value = mean(total_value),
    se_value = sd(total_value) / sqrt(n()),
    mean_weight = mean(total_kg),
    se_weight = sd(total_kg) / sqrt(n()),
    mean_time = mean(total_time),
    se_time = sd(total_time) / sqrt(n()),
    mean_kwh = mean(total_kwh),
    se_kwh = sd(total_kwh) / sqrt(n()),
  )
summary$Month <- month.abb[as.numeric(summary$Month)]
summary$Month <- factor(summary$Month, levels = month.abb) 

Value<-ggplot(summary, aes(x = Month, y = mean_value)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2) +
  labs(
    x = "Month",
    y = "Average Catch Value (£)") +
  theme_classic()

Weight<-ggplot(summary, aes(x = Month, y = mean_weight)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.2) +
  labs(
    x = "Month",
    y = "Average Catch Weight (KG)") +
  theme_classic()

Time<-ggplot(summary, aes(x = Month, y = mean_time)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_time - se_time, ymax = mean_time + se_time), width = 0.2) +
  labs(
    x = "Month",
    y = "Average time spent fishing (hrs)") +
  theme_classic()

Kwh<-ggplot(summary, aes(x = Month, y = mean_kwh)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), width = 0.2) +
  labs(
    x = "Month",
    y = "Average Effort whilst fishing (kWh)") +
  theme_classic()

All_plot <- ggarrange(Value, Weight, Time, Kwh,
                      labels = c("A.", "B.", "C.", "D."),    # Plot labels
                      ncol = 2, nrow = 2,                    # Arrange in a 2x2 grid
                      label.x = -0.01,                        # Adjust the x position of the labels
                      label.y = 0.99) 
All_plot

#Now do the same for gear type
tacsatEflalo_five$Combined<- paste(tacsatEflalo_five$VE_REF, tacsatEflalo_five$LE_GEAR, sep = "_")
template <- expand.grid(Combined = unique(tacsatEflalo_five$Combined), 
                        Month = unique(tacsatEflalo_five$Month))

# If vessel isn't present for that month then a 0 will be assigned
summary <- tacsatEflalo_five %>%
  group_by(Combined, Month) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh)) %>%
  # Right join with the template to fill missing combinations
  right_join(template, by = c("Combined", "Month")) %>%
  # Replace NA values with 0
  mutate(across(starts_with("total_"), replace_na, replace = 0))

split_values <- strsplit(as.character(summary$Combined), "_")#now split again
split_values <- matrix(unlist(split_values), ncol = 2, byrow = TRUE)
summary$VE_REF <- split_values[,1]
summary$LE_GEAR <- split_values[,2]

summary <- summary %>%
  group_by(Month, LE_GEAR) %>%
  summarise(
    mean_value = mean(total_value),
    se_value = sd(total_value) / sqrt(n()),
    mean_weight = mean(total_kg),
    se_weight = sd(total_kg) / sqrt(n()),
    mean_time = mean(total_time),
    se_time = sd(total_time) / sqrt(n()),
    mean_kwh = mean(total_kwh),
    se_kwh = sd(total_kwh) / sqrt(n()),
  )
summary$Month <- month.abb[as.numeric(summary$Month)]
summary$Month <- factor(summary$Month, levels = month.abb) 


Value<-ggplot(summary, aes(x = Month, y = mean_value, fill= LE_GEAR)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Catch Value (£)",
    fill = "Gear") +
  theme_classic()

Weight<-ggplot(summary, aes(x = Month, y = mean_weight, fill=LE_GEAR)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Catch Weight (KG)",
    fill = "Gear") +
  theme_classic()

Time<-ggplot(summary, aes(x = Month, y = mean_time, fill= LE_GEAR)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_time - se_time, ymax = mean_time + se_time), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average time spent fishing (hrs)",
    fill = "Gear") +
  theme_classic()

Kwh<-ggplot(summary, aes(x = Month, y = mean_kwh, fill = LE_GEAR)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Effort whilst fishing (kWh)",
    fill = "Gear") +
  theme_classic()

All_plot <- ggarrange(Value, Weight, Time, Kwh,
                      labels = c("A.", "B.", "C.", "D."),    # Plot labels
                      ncol = 2, nrow = 2,                    # Arrange in a 2x2 grid
                      label.x = -0.01,                        # Adjust the x position of the labels
                      label.y = 0.99) 
All_plot

#Now for Vessel size
tacsatEflalo_five$Combined<- paste(tacsatEflalo_five$VE_REF, tacsatEflalo_five$Size, sep = "_")
template <- expand.grid(Combined = unique(tacsatEflalo_five$Combined), 
                        Month = unique(tacsatEflalo_five$Month))

# If vessel isn't present for that month then a 0 will be assigned
summary <- tacsatEflalo_five %>%
  group_by(Combined, Month) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh)) %>%
  # Right join with the template to fill missing combinations
  right_join(template, by = c("Combined", "Month")) %>%
  # Replace NA values with 0
  mutate(across(starts_with("total_"), replace_na, replace = 0))

split_values <- strsplit(as.character(summary$Combined), "_")#now split again
split_values <- matrix(unlist(split_values), ncol = 2, byrow = TRUE)
summary$VE_REF <- split_values[,1]
summary$Size <- split_values[,2]

summary <- summary %>%
  group_by(Month, Size) %>%
  summarise(
    mean_value = mean(total_value),
    se_value = sd(total_value) / sqrt(n()),
    mean_weight = mean(total_kg),
    se_weight = sd(total_kg) / sqrt(n()),
    mean_time = mean(total_time),
    se_time = sd(total_time) / sqrt(n()),
    mean_kwh = mean(total_kwh),
    se_kwh = sd(total_kwh) / sqrt(n()),
  )
summary$Month <- month.abb[as.numeric(summary$Month)]
summary$Month <- factor(summary$Month, levels = month.abb) 


Value<-ggplot(summary, aes(x = Month, y = mean_value, fill= Size)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Catch Value (£)",
    fill = "Vessel size") +
  theme_classic()

Weight<-ggplot(summary, aes(x = Month, y = mean_weight, fill= Size)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Catch Weight (KG)",
    fill = "Vessel size") +
  theme_classic()

Time<-ggplot(summary, aes(x = Month, y = mean_time, fill= Size)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_time - se_time, ymax = mean_time + se_time), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average time spent fishing (hrs)",
    fill = "Vessel size") +
  theme_classic()

Kwh<-ggplot(summary, aes(x = Month, y = mean_kwh, fill = Size)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Month",
    y = "Average Effort whilst fishing (kWh)",
    fill = "Vessel Size") +
  theme_classic()

All_plot <- ggarrange(Value, Weight, Time, Kwh,
                      labels = c("A.", "B.", "C.", "D."),    # Plot labels
                      ncol = 2, nrow = 2,                    # Arrange in a 2x2 grid
                      label.x = -0.01,                        # Adjust the x position of the labels
                      label.y = 0.99) 
All_plot

#Now lets summarise by total catch depending on gear type
summary <- tacsatEflalo_five %>%
  group_by(LE_GEAR) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh))
#And by vessel size 

summary <- tacsatEflalo_five %>%
  group_by(Size) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh))

tacsatEflalo_five %>%
   group_by(Size) %>%
  summarize(num_vessels = n_distinct(VE_REF))#number of each vessel size

#Now lets summarize depending on location


summary <- tacsatEflalo %>%
  group_by(Location) %>%
  summarise(
    total_value = sum(LE_TOT_VAL),
    total_kg = sum(LE_TOT_KG),
    total_time = sum(TIME),
    total_kwh = sum(kwh))

summary <- summary %>%
  mutate_at(vars(-Location), ~ case_when(
    Location == "Five" ~ ./24.93,
    Location == "Ten" ~ ./74.78,
    Location == "Twenty" ~ ./299.12,
    Location == "Boundary" ~ ./4129.95,
    TRUE ~ . # standardize per km2 diving by area of location
  ))
summary

#quick potting eflalo analysis
load("./processed_data/eflaloClean.RData")
eflalo<- subset(eflalo, LE_GEAR == "FPO")
total_value <- eflalo %>%
  select(starts_with("LE_EURO")) %>%  # Select columns starting with 'LE_EURO'
  rowSums() %>%                        # Compute the sum of each row
  sum()    
total_value
total_weight <- eflalo %>%
  select(starts_with("LE_KG")) %>%  # Select columns starting with 'LE_EURO'
  rowSums() %>%                        # Compute the sum of each row
  sum()    
total_weight

column_sums <- eflalo %>%
  select(starts_with("LE_EURO")) %>%  # Select columns starting with 'LE_EURO'
  summarise_all(sum, na.rm = TRUE)%>%  # Sum each column, ignoring NA values
  pivot_longer(everything(),            # Pivot to long format: everything() to include all columns
               names_to = "Column",     # Name of the new column containing original column names
               values_to = "Sum")       # Name of the new column containing sums

column_sums <- eflalo %>%
  select(starts_with("LE_KG")) %>%  # Select columns starting with 'LE_EURO'
  summarise_all(sum, na.rm = TRUE)%>%  # Sum each column, ignoring NA values
  pivot_longer(everything(),            # Pivot to long format: everything() to include all columns
               names_to = "Column",     # Name of the new column containing original column names
               values_to = "Sum")       # Name of the new column containing sums

le_euro_columns <- grep("LE_EURO", names(eflalo), value = TRUE)
eflalo$TOT_LE_EURO <- rowSums(eflalo[, le_euro_columns], na.rm = TRUE)
le_kg_columns <- grep("LE_KG", names(eflalo), value = TRUE)
eflalo$TOT_LE_kg <- rowSums(eflalo[, le_kg_columns], na.rm = TRUE)
sums_by_harbour <- eflalo %>%
  group_by(FT_LHAR) %>%                   # Group by the 'Harbour' column
  summarise(total_value = sum(TOT_LE_EURO, na.rm = TRUE))
load("./processed_data/eflaloClean.RData")
eflalo<- eflalo %>%
  filter(LE_GEAR %in% c("GN", "GND", "GNS"))
total_value <- eflalo %>%
  select(starts_with("LE_EURO")) %>%  # Select columns starting with 'LE_EURO'
  rowSums() %>%                        # Compute the sum of each row
  sum()    
total_value
total_weight <- eflalo %>%
  select(starts_with("LE_KG")) %>%  # Select columns starting with 'LE_EURO'
  rowSums() %>%                        # Compute the sum of each row
  sum()    
total_weight

column_sums <- eflalo %>%
  select(starts_with("LE_EURO")) %>%  # Select columns starting with 'LE_EURO'
  summarise_all(sum, na.rm = TRUE)%>%  # Sum each column, ignoring NA values
  pivot_longer(everything(),            # Pivot to long format: everything() to include all columns
               names_to = "Column",     # Name of the new column containing original column names
               values_to = "Sum")       # Name of the new column containing sums

column_sums <- eflalo %>%
  select(starts_with("LE_KG")) %>%  # Select columns starting with 'LE_EURO'
  summarise_all(sum, na.rm = TRUE)%>%  # Sum each column, ignoring NA values
  pivot_longer(everything(),            # Pivot to long format: everything() to include all columns
               names_to = "Column",     # Name of the new column containing original column names
               values_to = "Sum")       # Name of the new column containing sums

le_euro_columns <- grep("LE_EURO", names(eflalo), value = TRUE)
eflalo$TOT_LE_EURO <- rowSums(eflalo[, le_euro_columns], na.rm = TRUE)
le_kg_columns <- grep("LE_KG", names(eflalo), value = TRUE)
eflalo$TOT_LE_kg <- rowSums(eflalo[, le_kg_columns], na.rm = TRUE)
sums_by_harbour <- eflalo %>%
  group_by(FT_LHAR) %>%                   # Group by the 'Harbour' column
  summarise(total_value = sum(TOT_LE_EURO, na.rm = TRUE))
#redo all plots
#load in required functions
library(vmstools)
source("functions//packages.R")

# Install and load required packages
for (package in required_packages) {
  install_and_load_package(package)
}

data(europa) #the following data is also required
class(europa)
data(ICESareas)
data("World")#map of the world
world <- ne_countries(scale = "large", returnclass = "sf")#boundaries using natural earth
source("functions//packages.R")

# Install and load required packages
for (package in required_packages) {
  install_and_load_package(package)
}

data(europa) #the following data is also required
class(europa)
data(ICESareas)
data("World")#map of the world
world <- ne_countries(scale = "large", returnclass = "sf")#boundaries using natural earth
source("functions//setcrs_makevalid.R")
source("functions//points_on_globe.R")
source("functions//Duplicate_tacsat.R")
source("functions//points_on_land.R")
source("functions//speed_removal.R")
source("functions//remove_harbours.R")
source("functions//remove_intervals.R")
source("functions//remove_duplicate_eflalo.R")
source("functions//remove_depart_arrive.R")
source("functions//Metier_species.R")
source("functions//Assign_width.R")
source("functions//threshold_value.R")
source("functions//sort_remove.R")
source("functions//Interpolate filter.R")
source("functions//VMShf.R")
source("functions//Interpolate_function.R")
source("functions//Gear_speed.R")
source("functions//Fixed_speed.R")
source("functions//speed_segmented.R")
source("functions//store_scheme.R")
source("functions//Normal_speed.R")
source("functions//split_pings.R")
source("functions//ping_site.R")
source("functions//Location_column.R")
source("functions//summary_output.R")
source("functions//summary_size.R")
source("functions//summary_month.R")
source("functions//plot_individual_VE.R")
source("functions//split_pings_id.R")
source("functions//percentage_plot.R")
source("functions//Heatmap_settings.R")
source("functions//Heatmap_plot.R")
source("functions//Heatmap_updated.R")
source("functions//Swept_area.R")
source("functions//Swept_function.R")
source("functions//Hab_map.R")
source("functions//Stack_hab.R")
source("functions//Stack_hab_over.R")
source("functions//Hab_ping_plot.R")
source("functions//plot_sensitivity_map.R")
source("functions//Stack_sens.R")
source("functions//plot_confidence.R")
source("functions//plot_MESH_confidence.R")
source("functions//Stack_sens_over.R")
source("functions//sens_ping_plot.R")
source("functions//FishArea.R")
source("functions//Stack_Fish.R")
source("functions//Stack_Fish_over.R")
source("functions//Carbon_map.R")
source("functions//Stack_Carbon.R")
source("functions//Stack_Carbon_over.R")
source("functions//Stack_bio.R")
source("functions//Stack_MESH.R")
source("functions//Swept_area_Sens.R")
source("functions//Stack_con_over.R")
source("functions//Swept_area_Con.R")
source("functions//Swept_area_Con_in.R")
source("functions//Con_stacked.R")
#and shapefiles
load("./processed_data/five.RData")#load our areas of interest
load("./processed_data/ten.RData")
load("./processed_data/twenty.RData")
load("./processed_data/boundary.RData")
load("./processed_data/ten_hole.RData")
load("./processed_data/twenty_hole.RData")
load("./processed_data/boundary_hole.RData")

#- Define the size of the grid cell
resx        <- 0.025
resy        <- 0.025

coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set crs
# Example usage:
# Example call with border patterns
result <- Heatmap(
  coords, 
  LE_TOT = "TIME", 
  scale_limits = c(0, 250), 
  legend_title = "Time (hrs)", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("darkgrey", "darkgrey", "darkgrey", "darkgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash")
)
Time<-result$map

# Example call with border patterns
result <- Heatmap(
  coords, 
  LE_TOT = "LE_TOT_KG", 
  scale_limits = c(0, 60000), 
  legend_title = "Weight (kg)", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("darkgrey", "darkgrey", "darkgrey", "darkgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash")
)
Weight<-result$map

# Example call with border patterns
result <- Heatmap(
  coords, 
  LE_TOT = "LE_TOT_VAL", 
  scale_limits = c(0, 60000), 
  legend_title = "Value (£)", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("darkgrey", "darkgrey", "darkgrey", "darkgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash")
)
Value<-result$map

# Example call with border patterns
result <- Heatmap(
  coords, 
  LE_TOT = "kwh", 
  scale_limits = c(0, 50000), 
  legend_title = "Effort (kWh)", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("darkgrey", "darkgrey", "darkgrey", "darkgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash")
)
kwh<-result$map
#add the variable you want to plot as a heatmap, additional shapefiles to be plotted, scale limits and x and y limits

ggp <- ggarrange(Value, Weight, Time, kwh,
                 labels = c("A.", "B.", "C.", "D."),    # Plot labels
                 ncol = 2, nrow = 2,                    # Arrange in a 2x2 grid
                 label.x = 0.02,                        # Adjust the x position of the labels
                 label.y = 0.9)  

                 ggp
#Now lets do the same for surface and subsurface abrasion
load("./processed_data/ten_hole.RData")
load("./processed_data/twenty_hole.RData")
load("./processed_data/boundary_hole.RData")
load("./processed_data/eflaloClean.RData")
resx        <- 0.025
resy        <- 0.025
tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates
coords$LE_WIDTH <- as.numeric(coords$LE_WIDTH)
result <- SweptArea(
  coords, 
  scale_limits = c(0, 12), 
  legend_title = "Surface SAR", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("darkgrey", "darkgrey", "darkgrey", "darkgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash")
)
SAR<-result$map

tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
swept <- calculate_swept_area(five_site, ten_hole, twenty_hole, boundary_hole, "Five", "Ten", "Twenty", "Boundary", result)
swept#this calculates the total swept area ratio of each site, excluding the other sites


#Same for subsurface abrasion

tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]#replace width with LE_SUB
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates
coords$LE_WIDTH <- as.numeric(coords$LE_WIDTH)
result <- SweptArea(
  coords, 
  scale_limits = c(0, 2), 
  legend_title = "Subsurface SAR", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("darkgrey", "darkgrey", "darkgrey", "darkgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash")
)
SUB<-result$map
ggp <- ggarrange(SAR,SUB,
                 labels = c("A.", "B."),    # Plot labels
                 ncol = 2, nrow = 1,                    # Arrange in a 2x2 grid
                 label.x = 0.05,                        # Adjust the x position of the labels
                 label.y = 0.8)  

ggp#combining both plots

tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
swept <- calculate_swept_area(five_site, ten_hole, twenty_hole, boundary_hole, "Five", "Ten", "Twenty", "Boundary", result)
swept
#We can also filter based on gear type or vessel length
#Use tacsatEflalo <- subset(tacsatEflalo, LE_GEAR == "DRB") and rerun code
#tacsatEflalo$VE_LEN<-as.numeric (tacsatEflalo$VE_LEN)
#tacsatEflalo <- tacsatEflalo %>%
#mutate(Size = ifelse(VE_LEN >= 12, "≥12m", "<12m"))
#tacsatEflalo <- subset(tacsatEflalo, Size == "≥12m")#for vessel size

#Plot your area of interest
# Specify the path to your Bathymetry file
file_path <- "./raw_data/Bath.tif"
# Read the file
img <- raster(file_path)#bathymetry data

#now we can plot our areas of interest
crop_extent <- extent(-5, -1,49, 51) 
cropped_raster <- crop(img, crop_extent)
raster_df <- as.data.frame(cropped_raster, xy = TRUE)
raster_df <- raster_df[raster_df$Bath <= 40, ]

# Define your data layers with their levels ordered
boundary$layer <- factor("D&S IFCA", levels = c( "D&S IFCA", "400km²" ,"100km²","HypHPMA"))
twenty_site$layer <- factor("400km²", levels = c( "D&S IFCA", "400km²" ,"100km²","HypHPMA"))
ten_site$layer <- factor("100km²", levels = c( "D&S IFCA", "400km²" ,"100km²","HypHPMA"))
five_site$layer <- factor("HypHPMA", levels = c( "D&S IFCA", "400km²" ,"100km²","HypHPMA"))
europa$layer <- "Europa" # If europa layer should not be in the legend, we keep it as a string

# Adjust the palette and plotting code
blue_palette <- brewer.pal(9, "Blues") 
blue_palette <- rev(blue_palette)
zoomout_map <- ggplot() +   
  geom_sf(data = europa, fill = "grey", col = NA) +
  geom_sf(data = five_site, fill = "red", col = "red", linewidth=1)+
  coord_sf(xlim = c(-10, 2), ylim = c(49.5, 59)) +
  theme(legend.position = "none") +
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = "none") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 6), 
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6))

# Plotting code
portland <- data.frame(
  lon = -2.4447,
  lat = 50.5675,
  name = "Portland"
)

brixham <- data.frame(
  lon = -3.516667,
  lat = 50.4,
  name = "Brixham"
)
zoom_map <- ggplot(raster_df) +   
  geom_raster(aes(x = x, y = y, fill = Bath)) +
  geom_sf(data = boundary, aes(color = layer), fill = NA, linewidth=0.8) +
  geom_sf(data = twenty_site, aes(color = layer), fill = NA, size = 3, linewidth=0.8) +
  geom_sf(data = ten_site, aes(color = layer), fill = NA, size = 3, linewidth=0.8) +
  geom_sf(data = five_site, aes(color = layer), fill = NA, size = 3, linewidth=0.8) +
  geom_sf(data = europa, fill = "grey", color = NA) +
  geom_point(data = brixham, aes(x = lon, y = lat), color = "black", size = 3) +
  geom_text(data = brixham, aes(x = lon, y = lat, label = name), vjust = -1, color = "black", size = 3)+
  geom_point(data = portland, aes(x = lon, y = lat), color = "black", size = 3) +
  geom_text(data = portland, aes(x = lon, y = lat, label = name), vjust = -1, color = "black", size = 3)+
  scale_color_manual(values = c("HypHPMA" = "red",
                                "400km²" = "yellow", 
                                "100km²" = "darkgreen", 
                                "D&S IFCA" = "darkblue")) +
  scale_fill_gradientn(colors = blue_palette, name="Depth (m)") + 
  coord_sf(xlim = c(-4, -2), ylim = c(50, 51)) +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  guides(color = guide_legend(title = "Layers", order = 1)) +
  theme_minimal()
ggp<- zoom_map+
  inset(ggplotGrob(zoomout_map), xmin = -4.3, xmax = -3.3, ymin = 50.60, ymax = 51.05)+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1.5)) 
ggp

#Now to summarise by habitat type
Hab_Type<-st_read("./raw_data/mapsquare.gpkg", layer = "mapsquare")
Hab_Type<- transform_and_make_valid(Hab_Type)

# Example usage of the function
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
result <- Stack_hab(five_site, ten_hole, twenty_hole, boundary_hole, order_locations)

View(result$Hab_final)

Hab_plot <- result$plot+labs(title = NULL)+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))

result <- Stack_hab_over(tacsatEflalo, order_locations)#plot of fishing activity depending on hab type
Hab_activity <- result$plot+labs(title = NULL)+
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))

ggp <- ggarrange(Hab_plot,Hab_activity,
                 labels = c("A.", "B."),    # Plot labels
                 ncol = 2, nrow = 1,                    # Arrange in a 2x2 grid
                 label.x = -0.01,                        # Adjust the x position of the labels
                 label.y = 0.99)  

ggp#combining both plots

#Now for MESH confidence
Title <-"MESH confidence estimate"# confidence
Heading <-"MESH confidence"
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
result <- Stack_MESH(Hab_Type,SUM_CONF,Title,five_site, ten_hole, twenty_hole, boundary_hole, order_locations,Heading)
MESH<-result$plot+labs(title = NULL)+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))+
  theme_minimal(base_size = 7.5) 
MESH
#####NESSST

Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
Hab_Type<- transform_and_make_valid(Hab_Type)

names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D2"] <- "Pressure"#rename your pressure column

Title<-"Sensitivity: penetration from demersal trawling"
Heading<-"Habitat sensitivity (SAB)"

order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
result <- Stack_sens(Hab_Type,Pressure,Title,five_site, ten_hole, twenty_hole, boundary_hole, order_locations,Heading)

pen<-result$plot+labs(title = NULL)+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))+
  theme_minimal(base_size = 7.5) 


names(Hab_Type)[names(Hab_Type) == "Pressure"] <- "sens_Z10_6_D2"#name back afterwards


#now for abrasion sens_Z10_6_D6
names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D6"] <- "Pressure"#rename your pressure column

Title<-"Sensitivity: abrasion from demersal trawling"
Heading<-"Habitat sensitivity (AB)"
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
result <- Stack_sens(Hab_Type,Pressure,Title,five_site, ten_hole, twenty_hole, boundary_hole, order_locations,Heading)

abr<-result$plot+labs(title = NULL)+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))+
  theme_minimal(base_size =7.5) 


names(Hab_Type)[names(Hab_Type) == "Pressure"] <- "sens_Z10_6_D6"#name back afterwards

#Now for confidences
Title <-"Biotope confidence estimate"#biotope confidence
Heading <-"Confidence in simulation"
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
result <- Stack_bio(Hab_Type,uncertainty_sim,Title,five_site, ten_hole, twenty_hole, boundary_hole, order_locations,Heading)
bio<-result$plot+labs(title = NULL)+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))+
  theme_minimal(base_size =7.5) 
bio
#confidence for each pressure
Title <-"Confidence level: penetration from demersal trawling"
Heading <-"Pressure confidence (SAB)"
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
names(Hab_Type)[names(Hab_Type) == "conf_Z10_6_D2"] <- "Pressure"#rename your confidence column
result <- Stack_con(Hab_Type,Pressure,Title,five_site, ten_hole, twenty_hole, boundary_hole, order_locations,Heading)
pen_con<-result$plot+labs(title = NULL)+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))+
  theme_minimal(base_size = 7.5) 
pen_con
names(Hab_Type)[names(Hab_Type) == "Pressure"] <- "conf_Z10_6_D2"#rename your confidence column
#now for abrasion
Title <-"Confidence level: abrasion from demersal trawling"
Heading <-"Pressure confidence (AB)"
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
names(Hab_Type)[names(Hab_Type) == "conf_Z10_6_D6"] <- "Pressure"#rename your confidence column
result <- Stack_con(Hab_Type,Pressure,Title,five_site, ten_hole, twenty_hole, boundary_hole, order_locations,Heading)
abr_con<-result$plot+labs(title = NULL)+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))+
  theme_minimal(base_size = 7.5) 
abr_con
names(Hab_Type)[names(Hab_Type) == "Pressure"] <- "conf_Z10_6_D6"#rename your confidence column

###########
abr<-abr+
  theme_minimal(base_size = 7.5) 
pen<-pen+
  theme_minimal(base_size = 7.5) 
abr_con<-abr_con+
  theme_minimal(base_size = 7.5)
pen_con<-pen_con+
  theme_minimal(base_size = 7.5) 
bio<-bio+
  theme_minimal(base_size = 7.5) 
MESH<-MESH+
  theme_minimal(base_size = 7.5)
ggp <- ggarrange(abr,pen,abr_con, pen_con,bio, MESH,
                 labels = c("A.", "B.", "C.","D.","E.","F."),    # Plot labels
                 ncol = 2, nrow = 3,                    # Arrange in a 2x2 grid
                 label.x = -0.015,                        # Adjust the x position of the labels
                 label.y = 1,font.label = list(size = 12))

ggp#combining both plots

#We also need to link sensitivity to fishing activity via swept area ratio
load("./processed_data/tacsatEflalo_ID.RData")#reload our fishing data
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
Title<-"Sensitivity: penetration from demersal trawling"
Heading<-"Sensitivity (SAB)"
names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D2"] <- "Pressure"#rename your pressure column
result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title, Heading,order_locations)
tacsatEflalo<-result$tacsatEflalo#associates each ping with a sensitivity
#Now we have to re-score the linked pressures to give a high to low sensitivity score
tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                        ifelse(tacsatEflalo$Pressure == 2, 2,
                        ifelse(tacsatEflalo$Pressure == 3, 1, 
                        ifelse(tacsatEflalo$Pressure == 4, 3, 
                        ifelse(tacsatEflalo$Pressure == 5, 3, 
                        ifelse(tacsatEflalo$Pressure == 6, 0,
                        ifelse(tacsatEflalo$Pressure == 7, 0, 
                        ifelse(tacsatEflalo$Pressure == 8, 0, 
                        ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))

#Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
#for penetration we use LE_SUB (subsurface abrasion width)
#- Define the size of the grid cell
resx        <- 0.025
resy        <- 0.025

load("./processed_data/eflaloClean.RData")
tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]#replace width with LE_SUB
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates
coords$LE_WIDTH <- as.numeric(coords$LE_WIDTH)
result <- SweptArea_Sens(
  coords, 
  scale_limits = c(0, 6), 
  legend_title = "Sensitivity SAR (SAB)", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash")
)
SUB<-result$map
swept_SUB <- calculate_swept_area(five_site, ten_hole, twenty_hole, boundary_hole, "Five", "Ten", "Twenty", "Boundary", result)
swept_SUB#calculate swept area sensitivity for different areas
names(Hab_Type)[names(Hab_Type) == "Pressure"] <- "sens_Z10_6_D2"#name back afterwards
#Now we do the same for abrasion pressure using sens_Z10_6_D6 and LE_WIDTH
load("./processed_data/tacsatEflalo_ID.RData")#reload our fishing data
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
Title<-"Sensitivity: abrasion from demersal trawling"
Heading<-"Sensitivity (AB)"
names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D6"] <- "Pressure"#rename your pressure column
result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title,Heading, order_locations)
tacsatEflalo<-result$tacsatEflalo#associates each ping with a sensitivity
#Now we have to re-score the linked pressures to give a high to low sensitivity score
tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                      ifelse(tacsatEflalo$Pressure == 2, 2,
                      ifelse(tacsatEflalo$Pressure == 3, 1, 
                      ifelse(tacsatEflalo$Pressure == 4, 3, 
                      ifelse(tacsatEflalo$Pressure == 5, 3, 
                      ifelse(tacsatEflalo$Pressure == 6, 0,
                      ifelse(tacsatEflalo$Pressure == 7, 0, 
                      ifelse(tacsatEflalo$Pressure == 8, 0, 
                      ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))

#Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
#for abrasion we use LE_WIDTH (surface abrasion width)
load("./processed_data/eflaloClean.RData")
tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates
coords$LE_WIDTH <- as.numeric(coords$LE_WIDTH)
result <- SweptArea_Sens(
  coords, 
  scale_limits = c(0, 16), 
  legend_title = "Sensitivity SAR (AB)", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash")
)
SAR<-result$map
swept_SAR <- calculate_swept_area(five_site, ten_hole, twenty_hole, boundary_hole, "Five", "Ten", "Twenty", "Boundary", result)
swept_SAR#calculate swept area sensitivity for different areas
names(Hab_Type)[names(Hab_Type) == "Pressure"] <- "sens_Z10_6_D6"#name back afterwards

#now need to do the same for the confidence intervals
#first lets look at confidence in penetration conf_Z10_6_D2
load("./processed_data/tacsatEflalo_ID.RData")#reload our fishing data
names(Hab_Type)[names(Hab_Type) == "conf_Z10_6_D2"] <- "Confidence"#rename your confidence column
result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
tacsatEflalo$Score <- ifelse(tacsatEflalo$Confidence == 1, 3,
                      ifelse(tacsatEflalo$Confidence == 2, 2,
                      ifelse(tacsatEflalo$Confidence == 3, 1, 
                      ifelse(tacsatEflalo$Confidence == 4, 0, 
                      ifelse(tacsatEflalo$Confidence == 0, 0, 0)))))
  
  
#add all confidences together within the square and divide by total number of pings
#Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
#for abrasion we use LE_WIDTH (surface abrasion width)
tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates

result <- SweptArea_Con(
  coords, 
  scale_limits = c(0, 25), 
  legend_title = "Pressure confidence (SAB)", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
  breaks = c(0,1,2,3), # Specify the breaks
  labels = c("Not relevant", "Low", "Medium", "High")
)
pen_con<-result$map
pen_con
names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "conf_Z10_6_D2"#return to normal

#next lets look at confidence in abrasion conf_Z10_6_D6
load("./processed_data/tacsatEflalo_ID.RData")#reload our fishing data
names(Hab_Type)[names(Hab_Type) == "conf_Z10_6_D6"] <- "Confidence"#rename your confidence column
result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
tacsatEflalo$Score <- ifelse(tacsatEflalo$Confidence == 1, 3,
                      ifelse(tacsatEflalo$Confidence == 2, 2,
                      ifelse(tacsatEflalo$Confidence == 3, 1, 
                      ifelse(tacsatEflalo$Confidence == 4, 0, 
                      ifelse(tacsatEflalo$Confidence == 0, 0, 0)))))


#add all confidences together within the square and divide by total number of pings
#Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
#for abrasion we use LE_WIDTH (surface abrasion width)
#- Define the size of the grid cell
tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates

result <- SweptArea_Con(
  coords, 
  scale_limits = c(0, 3), 
  legend_title = "Pressure confidence (AB)", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
  breaks = c(0,1,2,3), # Specify the breaks
  labels = c("Not relevant", "Low", "Medium", "High"))
abr_con<-result$map
names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "conf_Z10_6_D6"#return to normal

#next lets look at confidence in biotope proxy allocation
load("./processed_data/tacsatEflalo_ID.RData")#reload our fishing data
names(Hab_Type)[names(Hab_Type) == "uncertainty_sim"] <- "Confidence"#rename your confidence column
result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
tacsatEflalo$Score <- tacsatEflalo$Confidence
#add all confidences together within the square and divide by total number of pings
#Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
#for abrasion we use LE_WIDTH (surface abrasion width)
tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates

result <- SweptArea_Con(
  coords, 
  scale_limits = c(0, 1), 
  legend_title = "Confidence in simulation", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
  breaks = c(0,0.25,0.50,0.75,1.0), # Specify the breaks
  labels = c("Very low", "Low", "Moderate", "High","Very high")
)
bio<-result$map
names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "uncertainty_sim"#return to normal

#Finally lets look at MESH
Hab_Type<-st_read("./raw_data/mapsquare.gpkg", layer = "mapsquare")
Hab_Type<- transform_and_make_valid(Hab_Type)#need confidence from hab map

load("./processed_data/tacsatEflalo_ID.RData")#reload our fishing data
names(Hab_Type)[names(Hab_Type) == "SUM_CONF"] <- "Confidence"#rename your confidence column
result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
tacsatEflalo$Score <- tacsatEflalo$Confidence

#add all confidences together within the square and divide by total number of pings
#Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
#for abrasion we use LE_WIDTH (surface abrasion width)
tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates

result <- SweptArea_Con(
  coords, 
  scale_limits = c(0, 100), 
  legend_title = "MESH confidence", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
  breaks = c(0,20,37,58,79, 100), # Specify the breaks
  labels = c("0","Very low", "Low", "Moderate", "High","Very high")
)
MESH<-result$map
names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "SUM_CONF"#return to normal

SAR<-SAR+
  theme_classic(base_size = 7.5)+
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
SUB<-SUB+
  theme_classic(base_size = 7.5)+
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
abr_con<-abr_con+
  theme_classic(base_size = 7.5)+
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
pen_con<-pen_con+
  theme_classic(base_size = 7.5)+          
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
bio<-bio+
  theme_classic(base_size = 7.5)+          
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
MESH<-MESH+
  theme_classic(base_size = 7.5)+          
theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

ggp <- ggarrange(SAR,SUB,abr_con,pen_con,bio,MESH,
                 labels = c("A.", "B.","C.","D.","E.","F."),    # Plot labels
                 ncol = 2, nrow = 3,        # Arrange in a 2x2 grid
                 label.x = -0.01,            # Adjust the x position of the labels
                 label.y = 0.9)  


ggp#combining both plots


#Next we look at the Essential fish habitat map, looking at hab type and amount of fishing activity over each
Fish_hotspot <- st_read("./raw_data/data.gdb", layer = "Essential_Fish_Habitat_Hotspots_Adults")
Fish_hotspot<- transform_and_make_valid(Fish_hotspot)
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
spawning<-"spawning"
Heading<-"Spawning species richness"
Title<-"Spawning species richness"
result1 <- Stack_Fish(spawning,five_site, ten_hole, twenty_hole, boundary_hole, order_locations, Title, Heading)
result1$plot
Spawn<-result1$plot 
Spawn<-Spawn+theme_minimal(base_size =7.5)+
  theme(plot.title = element_blank())+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))
  

#make a stacked barchart of activity
result <- Stack_Fish_over(tacsatEflalo,Fish_hotspot,spawning,Title,Heading, order_locations)

Spawn_fish<-result$plot
Spawn_fish<-Spawn_fish+theme_minimal(base_size =7.5)+
  theme(plot.title = element_blank())+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))
  

names(Fish_hotspot)[names(Fish_hotspot) == "spawning"] <- "spawning1"#rename

#now for juveniles
Heading<-"Juveniles: species richness"
Title<-"Fish hotspots: Juveniles"
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
names(Fish_hotspot)[names(Fish_hotspot) == "juvenile"] <- "spawning"#rename
result1 <- Stack_Fish(spawning,five_site, ten_hole, twenty_hole, boundary_hole, order_locations, Title, Heading)
Juvenile<-result1$plot 
Juvenile<-Juvenile+theme_minimal(base_size =7.5)+
  theme(plot.title = element_blank())+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))
  

#make a stacked barchart of activity
result <- Stack_Fish_over(tacsatEflalo,Fish_hotspot,spawning,Title,Heading, order_locations)

Juvenile_fish<-result$plot
Juvenile_fish<-Juvenile_fish+theme_minimal(base_size =7.5)+
  theme(plot.title = element_blank())+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))
  

names(Fish_hotspot)[names(Fish_hotspot) == "spawning"] <- "juvenile"#rename

#now for adults
Heading<-"Adults: species richness"
Title<-"Fish hotspots: Adults"
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
names(Fish_hotspot)[names(Fish_hotspot) == "adults"] <- "spawning"#rename
result1 <- Stack_Fish(spawning,five_site, ten_hole, twenty_hole, boundary_hole, order_locations, Title, Heading)
Adults<-result1$plot
Adults<-Adults+theme_minimal(base_size =7.5)+
  theme(plot.title = element_blank())+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))
  
#make a stacked barchart of activity
result <- Stack_Fish_over(tacsatEflalo,Fish_hotspot,spawning,Title,Heading, order_locations)

Adults_fish<-result$plot
Adults_fish<-Adults_fish+ theme_minimal(base_size =7.5)+
  theme(plot.title = element_blank())+#plot of habitat area coverage
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))
  

names(Fish_hotspot)[names(Fish_hotspot) == "spawning"] <- "adults"#rename

ggp <- ggarrange(Spawn, Spawn_fish, Juvenile,Juvenile_fish,Adults,Adults_fish,
                 labels = c("A.", "B.","C.","D.","E.","F."),    # Plot labels
                 ncol = 2, nrow = 3,        # Arrange in a 2x2 grid
                 label.x = -0.018,            # Adjust the x position of the labels
                 label.y = 0.99)  

ggp#combining both plots


#Next we look at the proportion of blue carbon habitats
BC <- st_read("./raw_data/BC/final_inshoreBC.gdb", layer = "inshore_BC")
BC<- transform_and_make_valid(BC)
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
SF_CODE<-"SF_CODE"
result1 <- Stack_Carbon(SF_CODE,five_site, ten_hole, twenty_hole, boundary_hole, order_locations)
Hab_carbon<-result1$plot+
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))


result <- Stack_Carbon_over(tacsatEflalo, BC,SF_CODE,order_locations)
Fish_carbon<-result$plot+
  scale_x_discrete(labels = c("Five"="HypHPMA", "Ten"="100km²", "Twenty"="400km²", "Boundary"= "D&S IFCA", "Outside"))
Hab_carbon<-Hab_carbon+
  labs(title = NULL)
Fish_carbon<-Fish_carbon+
  labs(title = NULL)
 ggp <- ggarrange(Hab_carbon,Fish_carbon,
                 labels = c("A.", "B."),    # Plot labels
                 ncol = 2, nrow = 1,                    # Arrange in a 2x2 grid
                 label.x = -0.01,                        # Adjust the x position of the labels
                 label.y = 0.98)  
 ggp

#Next we will look at predicting increase in Time, SAR and SUB SAR
 load("./processed_data/tacsatEflalo_ID.RData")
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 subset_vessel<-tacsatEflalo
 vessel <- unique_vessels[1]
 subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
 subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
 time_sum <- sum(subset_df$TIME)
 subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
 total_time_sum <- sum(subset_total$TIME)
 
 #- Define the size of the grid cell
 resx <- 0.025
 resy <- 0.025
 
 coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
 st_crs(coords) <- 4326  # set crs
 
 areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
 areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
 
 bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
               matrix(st_bbox(areaRef), ncol = 2))
 rownames(bbox) <- c("x", "y")
 
 spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                   yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
 grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
 
 st_crs(grd) <- 4326
 
 grd$data <- 0
 
 subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
 
 idx <- st_over(coords, grd)
 subset_total$gridID <- idx
 
 grd$data[an(names(table(idx)))] <- aggregate(subset_total$TIME, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 grd$data <- grd$data  # Divide to get to hrs
 grd$data <- (grd$data) + ((grd$data * time_sum) / (total_time_sum))  # now for time increase
 
 
 # Initialize an empty grid
 grd_merged <- grd
 grd_merged$data <- 0
 
 # Assuming 'VE_REF' is a column in subset_total
 vessel_list <- unique(tacsatEflalo$VE_REF)
 
 
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   time_sum <- sum(subset_df$TIME)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_time_sum <- sum(subset_total$TIME)
   
   #- Define the size of the grid cell
   resx <- 0.025
   resy <- 0.025
   
   coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
   st_crs(coords) <- 4326  # set crs
   
   areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
   areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
   
   bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                 matrix(st_bbox(areaRef), ncol = 2))
   rownames(bbox) <- c("x", "y")
   
   spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                     yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
   grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
   
   st_crs(grd) <- 4326
   
   grd$data <- 0
   
   subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
   
   idx <- st_over(coords, grd)
   subset_total$gridID <- idx
   
   grd$data[an(names(table(idx)))] <- aggregate(subset_total$TIME, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   grd$data <- grd$data  # Divide to get to hrs
   grd$data <- ((grd$data * time_sum) / (total_time_sum))  # now for time increase
   
   grd_merged$data <- grd_merged$data + grd$data
 }
 
 load("./processed_data/five.RData")#load our areas of interest
 load("./processed_data/ten.RData")
 load("./processed_data/twenty.RData")
 load("./processed_data/boundary.RData")
 
 grd_merged$data[grd_merged$data <= 0] <- NA
 baseMap <- ggplot() +
   geom_sf(data = grd_merged, mapping = aes(fill = data), color = NA) +
   scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,15), name = "Time increase (hrs)") +
   xlim(x_limits = c(-3.6,-3.2)) +
   ylim(y_limits = c(50.4,50.7)) +
   labs(x = xl$label, y = yl$label) +
   geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth = 0.4) +
   geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth = 0.4) +
   geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth = 0.4 ) +
   geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth = 0.4) +
   geom_sf(data = europa, colour = 1, fill = "grey") +
   theme_classic() +
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
 theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
       axis.title = element_text(face = xl$font, size = rel(xl$cex)))
 Time<-baseMap
 save(grd_merged,file="./processed_data/Time_increase_data.RData")
 
 
 ############Now for swept area
 load("./processed_data/eflaloClean.RData")##calculate SAR
 tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
 tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
 subset_vessel<-tacsatEflalo
 subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
 time_sum <- sum(subset_df$TR_AREA)
 subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
 total_time_sum <- sum(subset_total$TR_AREA)
 
 #- Define the size of the grid cell
 resx <- 0.025
 resy <- 0.025
 
 coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
 st_crs(coords) <- 4326  # set crs
 
 areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
 areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
 
 bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
               matrix(st_bbox(areaRef), ncol = 2))
 rownames(bbox) <- c("x", "y")
 
 spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                   yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
 grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
 
 st_crs(grd) <- 4326
 
 grd$data <- 0
 
 subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
 
 idx <- st_over(coords, grd)
 subset_total$gridID <- idx
 
 grd$data[an(names(table(idx)))] <- aggregate(subset_total$TR_AREA, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 grd$data <- (grd$data) + ((grd$data * time_sum) / (total_time_sum))  # now for time increase
 
 
 # Initialize an empty grid
 grd_merged <- grd
 grd_merged$data <- 0
 
 vessel_list <- unique(tacsatEflalo$VE_REF)
 
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   time_sum <- sum(subset_df$TR_AREA)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_time_sum <- sum(subset_total$TR_AREA)
   #- Define the size of the grid cell
   resx <- 0.025
   resy <- 0.025
   
   coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
   st_crs(coords) <- 4326  # set crs
   
   areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
   areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
   
   bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                 matrix(st_bbox(areaRef), ncol = 2))
   rownames(bbox) <- c("x", "y")
   
   spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                     yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
   grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
   
   st_crs(grd) <- 4326
   
   grd$data <- 0
   
   subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
   
   idx <- st_over(coords, grd)
   subset_total$gridID <- idx
   
   grd$data[an(names(table(idx)))] <- aggregate(subset_total$TR_AREA, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   
   grd$data <- ((grd$data * time_sum) / (total_time_sum))  # now for time increase
   grd$data[is.na(grd$data)] <- 0
   grd$data[is.nan(grd$data)] <- 0
   grd_merged$data <- grd_merged$data + grd$data
 }
 save(grd_merged,file="./processed_data/grd_sar.RData")
 
 
 load("./processed_data/five.RData")#load our areas of interest
 load("./processed_data/ten.RData")
 load("./processed_data/twenty.RData")
 load("./processed_data/boundary.RData")
 
 #Now for SAR
 
 Hab_five_hole<- st_difference(grd_merged, five_site)#removing squares within HypHPMA
 
 Hab_five_hole <- st_make_valid(Hab_five_hole)
 st_crs(Hab_five_hole) <- 4326#setting crs
 Hab_five_hole$surface <- st_area(Hab_five_hole)/(1000*1000)#finding area of grid
 Hab_five_hole$SAR<- Hab_five_hole$data/Hab_five_hole$surface
 
 Hab_five_hole$SAR<-as.numeric(Hab_five_hole$SAR)
 Hab_five_hole$SAR[Hab_five_hole$SAR <= 0] <- NA
 baseMap <- ggplot() +
   geom_sf(data = Hab_five_hole, mapping = aes(fill = as.numeric(SAR)), color = NA) +
   scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,0.2), name = legend_title) +
   xlim(x_limits = c(-3.6,-3.2)) +
   ylim(y_limits = c(50.4,50.7)) +
   labs(x = xl$label, y = yl$label) +
   geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid", linewidth = 0.4 ) +
   geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth = 0.4) +
   geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth = 0.4 ) +
   geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth = 0.4) +
   geom_sf(data = europa, colour = 1, fill = "grey") +
   theme_classic() +
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
 theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
       axis.title = element_text(face = xl$font, size = rel(xl$cex)))
 baseMap

#Next we look at predicted increase after displacement
 load("./processed_data/tacsatEflalo_ID.RData")
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 subset_vessel<-tacsatEflalo
 vessel <- unique_vessels[1]
 subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
 subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
 time_sum <- sum(subset_df$TIME)
 subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
 total_time_sum <- sum(subset_total$TIME)
 
 #- Define the size of the grid cell
 resx <- 0.025
 resy <- 0.025
 
 coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
 st_crs(coords) <- 4326  # set crs
 
 areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
 areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
 
 bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
               matrix(st_bbox(areaRef), ncol = 2))
 rownames(bbox) <- c("x", "y")
 
 spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                   yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
 grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
 
 st_crs(grd) <- 4326
 
 grd$data <- 0
 
 subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
 
 idx <- st_over(coords, grd)
 subset_total$gridID <- idx
 
 grd$data[an(names(table(idx)))] <- aggregate(subset_total$TIME, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 grd$data <- grd$data  # Divide to get to hrs
 grd$data <- (grd$data) + ((grd$data * time_sum) / (total_time_sum))  # now for time increase
 
 
 # Initialize an empty grid
 grd_merged <- grd
 grd_merged$data <- 0
 
 # Assuming 'VE_REF' is a column in subset_total
 vessel_list <- unique(tacsatEflalo$VE_REF)
 
 
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   time_sum <- sum(subset_df$TIME)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_time_sum <- sum(subset_total$TIME)
   
   #- Define the size of the grid cell
   resx <- 0.025
   resy <- 0.025
   
   coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
   st_crs(coords) <- 4326  # set crs
   
   areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
   areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
   
   bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                 matrix(st_bbox(areaRef), ncol = 2))
   rownames(bbox) <- c("x", "y")
   
   spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                     yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
   grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
   
   st_crs(grd) <- 4326
   
   grd$data <- 0
   
   subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
   
   idx <- st_over(coords, grd)
   subset_total$gridID <- idx
   
   grd$data[an(names(table(idx)))] <- aggregate(subset_total$TIME, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   grd$data <- grd$data  # Divide to get to hrs
   grd$data <- ((grd$data * time_sum) / (total_time_sum))  # now for time increase
   
   grd_merged$data <- grd_merged$data + grd$data
 }
 
 load("./processed_data/five.RData")#load our areas of interest
 load("./processed_data/ten.RData")
 load("./processed_data/twenty.RData")
 load("./processed_data/boundary.RData")
 
 grd_merged$data[grd_merged$data <= 0] <- NA
 
 baseMap <- ggplot() +
   geom_sf(data = grd_merged, mapping = aes(fill = data), color = NA) +
   scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,15), name = "Time increase (hrs)") +
   xlim(x_limits = c(-3.6,-3.2)) +
   ylim(y_limits = c(50.4,50.7)) +
   labs(x = xl$label, y = yl$label) +
   geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid", linewidth= 0.4 ) +
   geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth= 0.4) +
   geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth= 0.4 ) +
   geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth= 0.4) +
   geom_sf(data = europa, colour = 1, fill = "grey") +
   theme_classic() +
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
 theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
       axis.title = element_text(face = xl$font, size = rel(xl$cex)))
 Time<-baseMap
 save(grd_merged,file="./processed_data/Time_increase_data.RData")
 load("./processed_data/Time_increase_data.RData")
 
 ############Now for swept area
 load("./processed_data/tacsatEflalo_ID.RData")
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 load("./processed_data/eflaloClean.RData")##calculate SAR
 tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
 tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
 subset_vessel<-tacsatEflalo
 subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
 time_sum <- sum(subset_df$TR_AREA)
 subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
 total_time_sum <- sum(subset_total$TR_AREA)
 
 #- Define the size of the grid cell
 resx <- 0.025
 resy <- 0.025
 
 coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
 st_crs(coords) <- 4326  # set crs
 
 areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
 areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
 
 bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
               matrix(st_bbox(areaRef), ncol = 2))
 rownames(bbox) <- c("x", "y")
 
 spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                   yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
 grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
 
 st_crs(grd) <- 4326
 
 grd$data <- 0
 
 subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
 
 idx <- st_over(coords, grd)
 subset_total$gridID <- idx
 
 grd$data[an(names(table(idx)))] <- aggregate(subset_total$TR_AREA, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 grd$data <- (grd$data) + ((grd$data * time_sum) / (total_time_sum))  # now for time increase
 
 
 # Initialize an empty grid
 grd_merged <- grd
 grd_merged$data <- 0
 
 vessel_list <- unique(tacsatEflalo$VE_REF)
 
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   time_sum <- sum(subset_df$TR_AREA)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_time_sum <- sum(subset_total$TR_AREA)
   #- Define the size of the grid cell
   resx <- 0.025
   resy <- 0.025
   
   coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
   st_crs(coords) <- 4326  # set crs
   
   areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
   areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
   
   bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                 matrix(st_bbox(areaRef), ncol = 2))
   rownames(bbox) <- c("x", "y")
   
   spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                     yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
   grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
   
   st_crs(grd) <- 4326
   
   grd$data <- 0
   
   subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
   
   idx <- st_over(coords, grd)
   subset_total$gridID <- idx
   
   grd$data[an(names(table(idx)))] <- aggregate(subset_total$TR_AREA, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   
   grd$data <- ((grd$data * time_sum) / (total_time_sum))  # now for time increase
   grd$data[is.na(grd$data)] <- 0
   grd$data[is.nan(grd$data)] <- 0
   grd_merged$data <- grd_merged$data + grd$data
 }
 save(grd_merged,file="./processed_data/grd_sar.RData")
 
 
 load("./processed_data/five.RData")#load our areas of interest
 load("./processed_data/ten.RData")
 load("./processed_data/twenty.RData")
 load("./processed_data/boundary.RData")
 
 #Now for SAR
 
 Hab_five_hole<- st_difference(grd_merged, five_site)#removing squares within HypHPMA
 
 Hab_five_hole <- st_make_valid(Hab_five_hole)
 st_crs(Hab_five_hole) <- 4326#setting crs
 Hab_five_hole$surface <- st_area(Hab_five_hole)/(1000*1000)#finding area of grid
 Hab_five_hole$SAR<- Hab_five_hole$data/Hab_five_hole$surface
 
 Hab_five_hole$SAR<-as.numeric(Hab_five_hole$SAR)
 Hab_five_hole$SAR[Hab_five_hole$SAR <= 0] <- NA
 baseMap <- ggplot() +
   geom_sf(data = Hab_five_hole, mapping = aes(fill = as.numeric(SAR)), color = NA) +
   scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,0.25), name = "Surface SAR increase") +
   xlim(x_limits = c(-3.6,-3.2)) +
   ylim(y_limits = c(50.4,50.7)) +
   labs(x = xl$label, y = yl$label) +
   geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4) +
   geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth= 0.4) +
   geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth= 0.4 ) +
   geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth= 0.4) +
   geom_sf(data = europa, colour = 1, fill = "grey") +
   theme_classic() +
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
 theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
       axis.title = element_text(face = xl$font, size = rel(xl$cex)))
 SAR<-baseMap
 save(Hab_five_hole,file="./processed_data/grd_sar_hole.RData")
 load("./processed_data/grd_sar_hole.RData")
 ##############################Now same but for subsurface
 load("./processed_data/tacsatEflalo_ID.RData")
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 load("./processed_data/eflaloClean.RData")##calculate SAR
 tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
 tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
 subset_vessel<-tacsatEflalo
 subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
 time_sum <- sum(subset_df$TR_AREA)
 subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
 total_time_sum <- sum(subset_total$TR_AREA)
 
 #- Define the size of the grid cell
 resx <- 0.025
 resy <- 0.025
 
 coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
 st_crs(coords) <- 4326  # set crs
 
 areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
 areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
 
 bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
               matrix(st_bbox(areaRef), ncol = 2))
 rownames(bbox) <- c("x", "y")
 
 spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                   yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
 grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
 
 st_crs(grd) <- 4326
 
 grd$data <- 0
 
 subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
 
 idx <- st_over(coords, grd)
 subset_total$gridID <- idx
 
 grd$data[an(names(table(idx)))] <- aggregate(subset_total$TR_AREA, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 grd$data <- (grd$data) + ((grd$data * time_sum) / (total_time_sum))  # now for time increase
 
 
 # Initialize an empty grid
 grd_merged <- grd
 grd_merged$data <- 0
 
 vessel_list <- unique(tacsatEflalo$VE_REF)
 
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   time_sum <- sum(subset_df$TR_AREA)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_time_sum <- sum(subset_total$TR_AREA)
   #- Define the size of the grid cell
   resx <- 0.025
   resy <- 0.025
   
   coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
   st_crs(coords) <- 4326  # set crs
   
   areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
   areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
   
   bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                 matrix(st_bbox(areaRef), ncol = 2))
   rownames(bbox) <- c("x", "y")
   
   spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                     yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
   grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
   
   st_crs(grd) <- 4326
   
   grd$data <- 0
   
   subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
   
   idx <- st_over(coords, grd)
   subset_total$gridID <- idx
   
   grd$data[an(names(table(idx)))] <- aggregate(subset_total$TR_AREA, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   
   grd$data <- ((grd$data * time_sum) / (total_time_sum))  # now for time increase
   grd$data[is.na(grd$data)] <- 0
   grd$data[is.nan(grd$data)] <- 0
   grd_merged$data <- grd_merged$data + grd$data
 }
 save(grd_merged,file="./processed_data/grd_sub.RData")
 
 
 load("./processed_data/five.RData")#load our areas of interest
 load("./processed_data/ten.RData")
 load("./processed_data/twenty.RData")
 load("./processed_data/boundary.RData")
 
 #Now for SUB SAR
 
 Hab_five_hole<- st_difference(grd_merged, five_site)#removing squares within HypHPMA
 
 Hab_five_hole <- st_make_valid(Hab_five_hole)
 st_crs(Hab_five_hole) <- 4326#setting crs
 Hab_five_hole$surface <- st_area(Hab_five_hole)/(1000*1000)#finding area of grid
 Hab_five_hole$SAR<- Hab_five_hole$data/Hab_five_hole$surface
 
 Hab_five_hole$SAR<-as.numeric(Hab_five_hole$SAR)
 Hab_five_hole$SAR[Hab_five_hole$SAR <= 0] <- NA
 save(Hab_five_hole,file="./processed_data/grd_sub_hole.RData")
 baseMap <- ggplot() +
   geom_sf(data = Hab_five_hole, mapping = aes(fill = as.numeric(SAR)), color = NA) +
   scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,0.1), name = "Subsurface SAR increase") +
   xlim(x_limits = c(-3.6,-3.2)) +
   ylim(y_limits = c(50.4,50.7)) +
   labs(x = xl$label, y = yl$label) +
   geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4) +
   geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth= 0.4) +
   geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth= 0.4 ) +
   geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth= 0.4) +
   geom_sf(data = europa, colour = 1, fill = "grey") +
   theme_classic() +
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
 theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
       axis.title = element_text(face = xl$font, size = rel(xl$cex)))
 SUB<-baseMap
 load("./processed_data/grd_sub_hole.RData")
 All_plot <- ggarrange(Time, SAR, SUB,
                       labels = c("A.", "B.", "C."),    # Plot labels
                       ncol = 2, nrow = 2,                    # Arrange in a 2x2 grid
                       label.x = -0.01,                        # Adjust the x position of the labels
                       label.y = 0.98) 
 All_plot
 ###########Next we look at Swept sensitivity increase
 load("./processed_data/tacsatEflalo_ID.RData")
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
 Hab_Type<- transform_and_make_valid(Hab_Type)
 #First we match sensitivity to ping
 load("./processed_data/eflaloClean.RData")
 names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D6"] <- "Pressure"#rename your pressure column
 order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
 Title<-"Sensitivity: abrasion from demersal trawling"
 result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title, order_locations)#abrasion
 tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
 tacsatEflalo$Pressure<-as.numeric(tacsatEflalo$Pressure)
 tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                              ifelse(tacsatEflalo$Pressure == 2, 2,
                                     ifelse(tacsatEflalo$Pressure == 3, 1, 
                                            ifelse(tacsatEflalo$Pressure == 4, 3, 
                                                   ifelse(tacsatEflalo$Pressure == 5, 3, 
                                                          ifelse(tacsatEflalo$Pressure == 6, 0,
                                                                 ifelse(tacsatEflalo$Pressure == 7, 0, 
                                                                        ifelse(tacsatEflalo$Pressure == 8, 0, 
                                                                               ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))#Next we find the swept area
 load("./processed_data/eflaloClean.RData")
 tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
 tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
 
 tacsatEflalo <- tacsatEflalo %>%
   mutate(
     High = if_else(Score == 3, TR_AREA, 0),
     Medium = if_else(Score == 2, TR_AREA, 0),
     Low = if_else(Score == 1, TR_AREA, 0),
     Not_relevant = if_else(Score == 0, TR_AREA, 0)
   )#separating based on scores
 
 subset_vessel<-tacsatEflalo
 subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
 time_sum <- sum(subset_df$TR_AREA)#find total swept area of HypHPMA
 
 subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
 total_time_sum <- sum(subset_total$TR_AREA)#find total swept area
 
 #- Define the size of the grid cell
 resx <- 0.025
 resy <- 0.025
 
 coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
 st_crs(coords) <- 4326  # set crs
 
 areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
 areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
 
 bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
               matrix(st_bbox(areaRef), ncol = 2))
 rownames(bbox) <- c("x", "y")
 
 spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                   yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
 grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
 
 st_crs(grd) <- 4326
 
 grd$Low <- 0
 grd$Medium<-0
 grd$High<-0
 grd$Not_relevant<-0
 subset_total<-as.data.frame(subset_total)
 subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
 
 idx <- st_over(coords, grd)
 subset_total$gridID <- idx
 
 grd$Low[an(names(table(idx)))] <- aggregate(subset_total$Low, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 grd$Medium[an(names(table(idx)))] <- aggregate(subset_total$Medium, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 grd$High[an(names(table(idx)))] <- aggregate(subset_total$High, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 grd$Not_relevant[an(names(table(idx)))] <- aggregate(subset_total$Not_relevant, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 
 grd$Low <- (grd$Low) + ((grd$Low * time_sum) / (total_time_sum))  # now for time increase
 grd$Medium <- (grd$Medium) + ((grd$Medium * time_sum) / (total_time_sum))  # now for time increase
 grd$High <- (grd$High) + ((grd$High * time_sum) / (total_time_sum))  # now for time increase
 grd$Not_relevant <- (grd$Not_relevant) + ((grd$Not_relevant * time_sum) / (total_time_sum))  # now for time increase
 
 grd$Low <-grd$Low*1
 grd$Medium <-grd$Medium*2
 grd$High <-grd$High*3
 grd$Not_relevant <-grd$Not_relevant*0
 
 grd$total<- grd$Low + grd$Medium + grd$High +grd$Not_relevant
 
 # Initialize an empty grid
 grd_merged <- grd
 grd_merged$Low <-0
 grd_merged$Medium <-0
 grd_merged$High <-0
 grd_merged$Not_relevant <-0
 
 vessel_list <- unique(tacsatEflalo$VE_REF)
 
 
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   time_sum <- sum(subset_df$TR_AREA)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_time_sum <- sum(subset_total$TR_AREA)
   #- Define the size of the grid cell
   resx <- 0.025
   resy <- 0.025
   
   coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
   st_crs(coords) <- 4326  # set crs
   
   areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
   areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
   
   bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                 matrix(st_bbox(areaRef), ncol = 2))
   rownames(bbox) <- c("x", "y")
   
   spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                     yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
   grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
   
   st_crs(grd) <- 4326
   
   grd$Low <- 0
   grd$Medium<-0
   grd$High<-0
   grd$Not_relevant<-0
   
   subset_total<-as.data.frame(subset_total)
   subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
   
   idx <- st_over(coords, grd)
   subset_total$gridID <- idx
   
   grd$Low[an(names(table(idx)))] <- aggregate(subset_total$Low, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   grd$Medium[an(names(table(idx)))] <- aggregate(subset_total$Medium, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   grd$High[an(names(table(idx)))] <- aggregate(subset_total$High, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   grd$Not_relevant[an(names(table(idx)))] <- aggregate(subset_total$Not_relevant, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   
   grd$Low <- ((grd$Low * time_sum) / (total_time_sum))  # now for time increase
   grd$Medium <- ((grd$Medium * time_sum) / (total_time_sum))  # now for time increase
   grd$High <- ((grd$High * time_sum) / (total_time_sum))  # now for time increase
   grd$Not_relevant <-((grd$Not_relevant * time_sum) / (total_time_sum))  # now for time increase
   
   grd$Low <-grd$Low*1
   grd$Medium <-grd$Medium*2
   grd$High <-grd$High*3
   grd$Not_relevant <-grd$Not_relevant*0
   
   grd$total<- grd$Low + grd$Medium + grd$High +grd$Not_relevant
   
   grd$Low[is.na( grd$Low)] <- 0
   grd$Low[is.nan( grd$Low)] <- 0
   grd$Medium[is.na(grd$Medium)] <- 0
   grd$Medium[is.nan(grd$Medium)] <- 0
   grd$High[is.na(grd$High)] <- 0
   grd$High[is.nan(grd$High)] <- 0
   grd$Not_relevant[is.na(grd$Not_relevant)] <- 0
   grd$Not_relevant[is.nan(grd$Not_relevant)] <- 0
   grd$total[is.na(grd$total)] <- 0
   grd$total[is.nan(grd$total)] <- 0
   grd_merged$Low <- grd_merged$Low + grd$Low
   grd_merged$Medium <- grd_merged$Medium + grd$Medium
   grd_merged$High <- grd_merged$High + grd$High
   grd_merged$Not_relevant <- grd_merged$Not_relevant + grd$Not_relevant
   grd_merged$total <- grd_merged$total + grd$total
 }
 
 save(grd_merged,file="./processed_data/grd_sens.RData")
 
 
 load("./processed_data/five.RData")#load our areas of interest
 load("./processed_data/ten.RData")
 load("./processed_data/twenty.RData")
 load("./processed_data/boundary.RData")
 
 
 Hab_five_hole<- st_difference(grd_merged, five_site)#removing squares within HypHPMA
 
 Hab_five_hole <- st_make_valid(Hab_five_hole)
 st_crs(Hab_five_hole) <- 4326#setting crs
 Hab_five_hole$surface <- st_area(Hab_five_hole)/(1000*1000)#finding area of grid
 Hab_five_hole$Total<-Hab_five_hole$Low + Hab_five_hole$Medium + Hab_five_hole$High
 Hab_five_hole$Total[Hab_five_hole$Total <= 0] <- NA
 Hab_five_hole$sur<-Hab_five_hole$Total/Hab_five_hole$surface
 
 
 baseMap <- ggplot() +
   geom_sf(data = Hab_five_hole, mapping = aes(fill = as.numeric(sur)), color = NA) +
   scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,0.5), name = "SSR (AB) increase") +
   xlim(x_limits = c(-3.6,-3.2)) +
   ylim(y_limits = c(50.4,50.7)) +
   labs(x = xl$label, y = yl$label) +
   geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4) +
   geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth= 0.4) +
   geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth= 0.4 ) +
   geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth= 0.4) +
   geom_sf(data = europa, colour = 1, fill = "grey") +
   theme_classic() +
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
 theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
       axis.title = element_text(face = xl$font, size = rel(xl$cex)))
 SAR<-baseMap
 save(Hab_five_hole,file="./processed_data/grd_sens_hole.RData")
 load("./processed_data/grd_sens_hole.RData")
 names(Hab_Type)[names(Hab_Type) == "Pressure"] <- "sens_Z10_6_D6"
 
 
 ####Next we do the same for Subsurface
 load("./processed_data/tacsatEflalo_ID.RData")
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
 Hab_Type<- transform_and_make_valid(Hab_Type)
 #First we match sensitivity to ping
 load("./processed_data/eflaloClean.RData")
 names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D2"] <- "Pressure"#rename your pressure column
 order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
 Title<-"Sensitivity: penetration from demersal trawling"
 result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title, order_locations)#penetration
 tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
 tacsatEflalo$Pressure<-as.numeric(tacsatEflalo$Pressure)
 tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                              ifelse(tacsatEflalo$Pressure == 2, 2,
                                     ifelse(tacsatEflalo$Pressure == 3, 1, 
                                            ifelse(tacsatEflalo$Pressure == 4, 3, 
                                                   ifelse(tacsatEflalo$Pressure == 5, 3, 
                                                          ifelse(tacsatEflalo$Pressure == 6, 0,
                                                                 ifelse(tacsatEflalo$Pressure == 7, 0, 
                                                                        ifelse(tacsatEflalo$Pressure == 8, 0, 
                                                                               ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))#Next we find the swept area
 load("./processed_data/eflaloClean.RData")
 tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
 tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
 
 tacsatEflalo <- tacsatEflalo %>%
   mutate(
     High = if_else(Score == 3, TR_AREA, 0),
     Medium = if_else(Score == 2, TR_AREA, 0),
     Low = if_else(Score == 1, TR_AREA, 0),
     Not_relevant = if_else(Score == 0, TR_AREA, 0)
   )#separating based on scores
 
 subset_vessel<-tacsatEflalo
 subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
 time_sum <- sum(subset_df$TR_AREA)#find total swept area of HypHPMA
 
 subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
 total_time_sum <- sum(subset_total$TR_AREA)#find total swept area
 
 #- Define the size of the grid cell
 resx <- 0.025
 resy <- 0.025
 
 coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
 st_crs(coords) <- 4326  # set crs
 
 areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
 areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
 
 bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
               matrix(st_bbox(areaRef), ncol = 2))
 rownames(bbox) <- c("x", "y")
 
 spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                   yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
 grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
 
 st_crs(grd) <- 4326
 
 grd$Low <- 0
 grd$Medium<-0
 grd$High<-0
 grd$Not_relevant<-0
 subset_total<-as.data.frame(subset_total)
 subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
 
 idx <- st_over(coords, grd)
 subset_total$gridID <- idx
 
 grd$Low[an(names(table(idx)))] <- aggregate(subset_total$Low, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 grd$Medium[an(names(table(idx)))] <- aggregate(subset_total$Medium, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 grd$High[an(names(table(idx)))] <- aggregate(subset_total$High, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 grd$Not_relevant[an(names(table(idx)))] <- aggregate(subset_total$Not_relevant, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 
 grd$Low <- (grd$Low) + ((grd$Low * time_sum) / (total_time_sum))  # now for time increase
 grd$Medium <- (grd$Medium) + ((grd$Medium * time_sum) / (total_time_sum))  # now for time increase
 grd$High <- (grd$High) + ((grd$High * time_sum) / (total_time_sum))  # now for time increase
 grd$Not_relevant <- (grd$Not_relevant) + ((grd$Not_relevant * time_sum) / (total_time_sum))  # now for time increase
 
 grd$Low <-grd$Low*1
 grd$Medium <-grd$Medium*2
 grd$High <-grd$High*3
 grd$Not_relevant <-grd$Not_relevant*0
 
 grd$total<- grd$Low + grd$Medium + grd$High +grd$Not_relevant
 
 # Initialize an empty grid
 grd_merged <- grd
 grd_merged$Low <-0
 grd_merged$Medium <-0
 grd_merged$High <-0
 grd_merged$Not_relevant <-0
 
 vessel_list <- unique(tacsatEflalo$VE_REF)
 
 
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   time_sum <- sum(subset_df$TR_AREA)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_time_sum <- sum(subset_total$TR_AREA)
   #- Define the size of the grid cell
   resx <- 0.025
   resy <- 0.025
   
   coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
   st_crs(coords) <- 4326  # set crs
   
   areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
   areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
   
   bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                 matrix(st_bbox(areaRef), ncol = 2))
   rownames(bbox) <- c("x", "y")
   
   spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                     yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
   grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
   
   st_crs(grd) <- 4326
   
   grd$Low <- 0
   grd$Medium<-0
   grd$High<-0
   grd$Not_relevant<-0
   
   subset_total<-as.data.frame(subset_total)
   subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
   
   idx <- st_over(coords, grd)
   subset_total$gridID <- idx
   
   grd$Low[an(names(table(idx)))] <- aggregate(subset_total$Low, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   grd$Medium[an(names(table(idx)))] <- aggregate(subset_total$Medium, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   grd$High[an(names(table(idx)))] <- aggregate(subset_total$High, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   grd$Not_relevant[an(names(table(idx)))] <- aggregate(subset_total$Not_relevant, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
   
   grd$Low <- ((grd$Low * time_sum) / (total_time_sum))  # now for time increase
   grd$Medium <- ((grd$Medium * time_sum) / (total_time_sum))  # now for time increase
   grd$High <- ((grd$High * time_sum) / (total_time_sum))  # now for time increase
   grd$Not_relevant <-((grd$Not_relevant * time_sum) / (total_time_sum))  # now for time increase
   
   grd$Low <-grd$Low*1
   grd$Medium <-grd$Medium*2
   grd$High <-grd$High*3
   grd$Not_relevant <-grd$Not_relevant*0
   
   grd$total<- grd$Low + grd$Medium + grd$High +grd$Not_relevant
   
   grd$Low[is.na( grd$Low)] <- 0
   grd$Low[is.nan( grd$Low)] <- 0
   grd$Medium[is.na(grd$Medium)] <- 0
   grd$Medium[is.nan(grd$Medium)] <- 0
   grd$High[is.na(grd$High)] <- 0
   grd$High[is.nan(grd$High)] <- 0
   grd$Not_relevant[is.na(grd$Not_relevant)] <- 0
   grd$Not_relevant[is.nan(grd$Not_relevant)] <- 0
   grd$total[is.na(grd$total)] <- 0
   grd$total[is.nan(grd$total)] <- 0
   grd_merged$Low <- grd_merged$Low + grd$Low
   grd_merged$Medium <- grd_merged$Medium + grd$Medium
   grd_merged$High <- grd_merged$High + grd$High
   grd_merged$Not_relevant <- grd_merged$Not_relevant + grd$Not_relevant
   grd_merged$total <- grd_merged$total + grd$total
 }
 
 save(grd_merged,file="./processed_data/grd_sub_sens.RData")
 
 
 load("./processed_data/five.RData")#load our areas of interest
 load("./processed_data/ten.RData")
 load("./processed_data/twenty.RData")
 load("./processed_data/boundary.RData")
 
 
 Hab_five_hole<- st_difference(grd_merged, five_site)#removing squares within HypHPMA
 
 Hab_five_hole <- st_make_valid(Hab_five_hole)
 st_crs(Hab_five_hole) <- 4326#setting crs
 Hab_five_hole$surface <- st_area(Hab_five_hole)/(1000*1000)#finding area of grid
 Hab_five_hole$Total<-Hab_five_hole$Low + Hab_five_hole$Medium + Hab_five_hole$High
 Hab_five_hole$Total[Hab_five_hole$Total <= 0] <- NA
 Hab_five_hole$sur<-Hab_five_hole$Total/Hab_five_hole$surface
 
 
 baseMap <- ggplot() +
   geom_sf(data = Hab_five_hole, mapping = aes(fill = as.numeric(sur)), color = NA) +
   scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,0.25), name = "SSR (SAB) increase") +
   xlim(x_limits = c(-3.6,-3.2)) +
   ylim(y_limits = c(50.4,50.7)) +
   labs(x = xl$label, y = yl$label) +
   geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4) +
   geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth= 0.4) +
   geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth= 0.4 ) +
   geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth= 0.4) +
   geom_sf(data = europa, colour = 1, fill = "grey") +
   theme_classic() +
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
 theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
       axis.title = element_text(face = xl$font, size = rel(xl$cex)))
 SUB<-baseMap
 save(Hab_five_hole,file="./processed_data/grd_sub_sens_hole.RData")
 load("./processed_data/grd_sub_sens_hole.RData")
 #Now for confidences, subsurface abrasion
 load("./processed_data/tacsatEflalo_ID.RData")
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 names(Hab_Type)[names(Hab_Type) == "conf_Z10_6_D2"] <- "Confidence"#rename your confidence column
 result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
 tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
 tacsatEflalo$Score <- ifelse(tacsatEflalo$Confidence == 1, 3,
                              ifelse(tacsatEflalo$Confidence == 2, 2,
                                     ifelse(tacsatEflalo$Confidence == 3, 1, 
                                            ifelse(tacsatEflalo$Confidence == 4, 0, 
                                                   ifelse(tacsatEflalo$Confidence == 0, 0, 0)))))
 
 
 #add all confidences together within the square and divide by total number of pings
 #Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
 #for abrasion we use LE_WIDTH (surface abrasion width)
 tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
 coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
 st_crs(coords) <- 4326#set coordinates
 
 result <- SweptArea_Con_in(
   coords, 
   scale_limits = c(0, 3), 
   legend_title = "Pressure confidence (SAB)", 
   x_limits = c(-3.6, -3.2), 
   y_limits = c(50.4, 50.7), 
   additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
   additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
   additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
   breaks = c(0,1,2,3), # Specify the breaks
   labels = c("Not relevant", "Low", "Medium", "High")
 )
 pen_con<-result$map
 pen_con
 names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "conf_Z10_6_D2"#return to normal
 
 
 #Now for confidence in abrasion
 load("./processed_data/tacsatEflalo_ID.RData")
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 
 names(Hab_Type)[names(Hab_Type) == "conf_Z10_6_D6"] <- "Confidence"#rename your confidence column
 result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
 tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
 tacsatEflalo$Score <- ifelse(tacsatEflalo$Confidence == 1, 3,
                              ifelse(tacsatEflalo$Confidence == 2, 2,
                                     ifelse(tacsatEflalo$Confidence == 3, 1, 
                                            ifelse(tacsatEflalo$Confidence == 4, 0, 
                                                   ifelse(tacsatEflalo$Confidence == 0, 0, 0)))))
 
 
 #add all confidences together within the square and divide by total number of pings
 #Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
 #for abrasion we use LE_WIDTH (surface abrasion width)
 #- Define the size of the grid cell
 tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
 coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
 st_crs(coords) <- 4326#set coordinates
 
 result <- SweptArea_Con_in(
   coords, 
   scale_limits = c(0, 3), 
   legend_title = "Pressure confidence (AB)", 
   x_limits = c(-3.6, -3.2), 
   y_limits = c(50.4, 50.7), 
   additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
   additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
   additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
   breaks = c(0,1,2,3), # Specify the breaks
   labels = c("Not relevant", "Low", "Medium", "High"))
 abr_con<-result$map
 names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "conf_Z10_6_D6"#return to normal
 
 #Next confidence in biotope
 load("./processed_data/tacsatEflalo_ID.RData")
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 
 names(Hab_Type)[names(Hab_Type) == "uncertainty_sim"] <- "Confidence"#rename your confidence column
 result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
 tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
 tacsatEflalo$Score <- tacsatEflalo$Confidence
 #add all confidences together within the square and divide by total number of pings
 #Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
 #for abrasion we use LE_WIDTH (surface abrasion width)
 tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
 coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
 st_crs(coords) <- 4326#set coordinates
 
 result <- SweptArea_Con_in(
   coords, 
   scale_limits = c(0, 1), 
   legend_title = "Confidence in simulation", 
   x_limits = c(-3.6, -3.2), 
   y_limits = c(50.4, 50.7), 
   additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
   additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
   additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
   breaks = c(0,0.25,0.50,0.75,1.0), # Specify the breaks
   labels = c("Very low", "Low", "Moderate", "High","Very high")
 )
 bio<-result$map
 names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "uncertainty_sim"#return to normal
 
 #Now for MESH confidence
 load("./processed_data/tacsatEflalo_ID.RData")
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 Hab_Type<-st_read("./raw_data/mapsquare.gpkg", layer = "mapsquare")
 Hab_Type<- transform_and_make_valid(Hab_Type)#need confidence from hab map
 
 names(Hab_Type)[names(Hab_Type) == "SUM_CONF"] <- "Confidence"#rename your confidence column
 result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
 tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
 tacsatEflalo$Score <- tacsatEflalo$Confidence
 
 #add all confidences together within the square and divide by total number of pings
 #Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
 #for abrasion we use LE_WIDTH (surface abrasion width)
 tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
 coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
 st_crs(coords) <- 4326#set coordinates
 
 result <- SweptArea_Con_in(
   coords, 
   scale_limits = c(0, 100), 
   legend_title = "MESH confidence", 
   x_limits = c(-3.6, -3.2), 
   y_limits = c(50.4, 50.7), 
   additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
   additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
   additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
   breaks = c(0,20,37,58,79, 100), # Specify the breaks
   labels = c("0","Very low", "Low", "Moderate", "High","Very high")
 )
 MESH<-result$map
 names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "SUM_CONF"#return to normal
 
 SAR<-SAR+
   theme_classic(base_size = 7.5)+
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
 SUB<-SUB+
   theme_classic(base_size = 7.5)+
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
 abr_con<-abr_con+
   geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4)+
   theme_classic(base_size = 7.5)+
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
 pen_con<-pen_con+
   geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4)+
   theme_classic(base_size = 7.5)+          
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
 bio<-bio+
   geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4)+
   theme_classic(base_size = 7.5)+          
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
 MESH<-MESH+
   geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4)+
   theme_classic(base_size = 7.5)+          
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
 
 ggp <- ggarrange(SAR,SUB,abr_con,pen_con,bio,MESH,
                  labels = c("A.", "B.","C.","D.","E.","F."),    # Plot labels
                  ncol = 2, nrow = 3,        # Arrange in a 2x2 grid
                  label.x = -0.01,            # Adjust the x position of the labels
                  label.y = 0.9)  
 
 
 ggp#combining both plots
 #Calculate increase after displacement
 load("./processed_data/tacsatEflalo_ID.RData")
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 vessel_list <- unique(tacsatEflalo$VE_REF)
 Final_summary <- data.frame(Location = "Ten", Total_Time = 0)
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   time_sum <- sum(subset_df$TIME)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_time_sum <- sum(subset_total$TIME)
   summary <- subset_vessel%>%
     filter(Location != "Five") %>%
     group_by(Location) %>%
     summarize(Total_Time = sum(TIME))
   summary$Total_Time <- ((summary$Total_Time * time_sum) / (total_time_sum))
   Final_summary<- rbind(Final_summary,summary)}
 
 summary <-  Final_summary%>%
   group_by(Location) %>%
   summarize(Total_Time = sum(Total_Time))
 #####SAR surface
 load("./processed_data/tacsatEflalo_ID.RData")
 load("./processed_data/eflaloClean.RData")##calculate SAR
 tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
 tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 vessel_list <- unique(tacsatEflalo$VE_REF)
 Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   swept_sum <- sum(subset_df$TR_AREA)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_swept_sum <- sum(subset_total$TR_AREA)
   summary <- subset_vessel%>%
     filter(Location != "Five") %>%
     group_by(Location) %>%
     summarize(TR_AREA = sum(TR_AREA))
   summary$TR_AREA <- ((summary$TR_AREA * swept_sum) / (total_swept_sum))
   Final_summary<- rbind(Final_summary,summary)}
 
 summary <-  Final_summary%>%
   group_by(Location) %>%
   summarize(TR_AREA = sum(TR_AREA))
 five <- st_transform( five_site, crs = 3035)
 ten <- st_transform( ten_hole, crs = 3035)
 twenty <- st_transform( twenty_hole, crs = 3035)
Boundary <- st_transform( boundary_hole, crs = 3035)
 five<-as.numeric((st_area(five))/1000000)
 ten<-as.numeric((st_area(ten))/1000000)
 twenty<-as.numeric((st_area(twenty))/1000000)
 Boundary<-as.numeric((st_area(Boundary))/1000000)
 Boundary<-sum(Boundary)
 summary <- summary%>%
   mutate(
     TR_AREA = case_when(
       Location == "Ten" ~ TR_AREA / ten,
       Location == "Twenty" ~ TR_AREA / twenty,
       Location == "Boundary" ~ TR_AREA / Boundary,
       TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
     )
   )
 summary
 ###SUB SAR
 load("./processed_data/tacsatEflalo_ID.RData")
 load("./processed_data/eflaloClean.RData")##calculate SAR
 tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
 tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 vessel_list <- unique(tacsatEflalo$VE_REF)
 Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   swept_sum <- sum(subset_df$TR_AREA)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_swept_sum <- sum(subset_total$TR_AREA)
   summary <- subset_vessel%>%
     filter(Location != "Five") %>%
     group_by(Location) %>%
     summarize(TR_AREA = sum(TR_AREA))
   summary$TR_AREA <- ((summary$TR_AREA * swept_sum) / (total_swept_sum))
   Final_summary<- rbind(Final_summary,summary)}
 
 summary <-  Final_summary%>%
   group_by(Location) %>%
   summarize(TR_AREA = sum(TR_AREA))
 five <- st_transform( five_site, crs = 3035)
 ten <- st_transform( ten_hole, crs = 3035)
 twenty <- st_transform( twenty_hole, crs = 3035)
 Boundary <- st_transform( boundary_hole, crs = 3035)
 five<-as.numeric((st_area(five))/1000000)
 ten<-as.numeric((st_area(ten))/1000000)
 twenty<-as.numeric((st_area(twenty))/1000000)
 Boundary<-as.numeric((st_area(Boundary))/1000000)
 Boundary<-sum(Boundary)
 summary <- summary%>%
   mutate(
     TR_AREA = case_when(
       Location == "Ten" ~ TR_AREA / ten,
       Location == "Twenty" ~ TR_AREA / twenty,
       Location == "Boundary" ~ TR_AREA / Boundary,
       TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
     )
   )
 
 summary
 #Now for SSR
 load("./processed_data/tacsatEflalo_ID.RData")
 load("./processed_data/eflaloClean.RData")##calculate SAR
 tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
 tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
 Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
 Hab_Type<- transform_and_make_valid(Hab_Type)
 names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D6"] <- "Pressure"#rename your pressure column
 order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
 Title<-"Sensitivity: abrasion from demersal trawling"
 Heading<-"Sensitivity (AB)"
 result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title,Heading, order_locations)#penetration
 tacsatEflalo<-result$tacsatEflalo#associates each ping with a pressure  
 tacsatEflalo$Pressure<-as.numeric(tacsatEflalo$Pressure)
 tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                              ifelse(tacsatEflalo$Pressure == 2, 2,
                                     ifelse(tacsatEflalo$Pressure == 3, 1, 
                                            ifelse(tacsatEflalo$Pressure == 4, 3, 
                                                   ifelse(tacsatEflalo$Pressure == 5, 3, 
                                                          ifelse(tacsatEflalo$Pressure == 6, 0,
                                                                 ifelse(tacsatEflalo$Pressure == 7, 0, 
                                                                        ifelse(tacsatEflalo$Pressure == 8, 0, 
                                                                               ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))#Next we find the swept area
 tacsatEflalo$Total<-tacsatEflalo$Score*tacsatEflalo$TR_AREA
 summary <- tacsatEflalo%>%
   group_by(Location) %>%
   summarize(Total = sum(Total))
 summary <- st_drop_geometry(summary)
 summary <- summary%>%
   mutate(
     Total = case_when(
       Location == "Five" ~ Total / five,
       Location == "Ten" ~ Total / ten,
       Location == "Twenty" ~ Total/ twenty,
       Location == "Boundary" ~ Total / Boundary,
       TRUE ~ Total # Default case, if location doesn't match any of the above
     )
   )
 
 tacsatEflalo <- tacsatEflalo %>%
   mutate(
     High = if_else(Score == 3, TR_AREA, 0),
     Medium = if_else(Score == 2, TR_AREA, 0),
     Low = if_else(Score == 1, TR_AREA, 0),
     Not_relevant = if_else(Score == 0, TR_AREA, 0)
   )#separating based on scores
 
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 vessel_list <- unique(tacsatEflalo$VE_REF)
 Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   swept_sum <- sum(subset_df$TR_AREA)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_swept_sum <- sum(subset_total$TR_AREA)
   subset_total$Low <-((subset_total$Low * swept_sum) / (total_swept_sum))  # now for time increase
   subset_total$Medium <-  ((subset_total$Medium * swept_sum) / (total_swept_sum))  # now for time increase
   subset_total$High <-((subset_total$High * swept_sum) / (total_swept_sum))  # now for time increase
   subset_total$Not_relevant <- ((subset_total$Not_relevant * swept_sum) / (total_swept_sum))  # now for time increase
   
   subset_total$Low<-subset_total$Low*1
   subset_total$Medium <-subset_total$Medium*2
   subset_total$High <-subset_total$High*3
   subset_total$Not_relevant <-subset_total$Not_relevant*0
   subset_total$total<- subset_total$Low + subset_total$Medium + subset_total$High
   
   summary <- subset_total%>%
     filter(Location != "Five") %>%
     group_by(Location) %>%
     summarize(TR_AREA = sum(total))
   summary <- st_drop_geometry(summary)
   Final_summary<-rbind(Final_summary,summary)}
 
 summary <-  Final_summary%>%
   group_by(Location) %>%
   summarize(TR_AREA = sum(TR_AREA))
 
 summary <- summary%>%
   mutate(
     TR_AREA = case_when(
       Location == "Five" ~ TR_AREA / five,
       Location == "Ten" ~ TR_AREA / ten,
       Location == "Twenty" ~ TR_AREA/ twenty,
       Location == "Boundary" ~ TR_AREA / Boundary,
       TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
     )
   )
 
 #Now for subsurface SSR
 load("./processed_data/tacsatEflalo_ID.RData")
 load("./processed_data/eflaloClean.RData")##calculate SAR
 tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
 tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
 Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
 Hab_Type<- transform_and_make_valid(Hab_Type)
 names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D2"] <- "Pressure"#rename your pressure column
 order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
 Title<-"Sensitivity: penetration from demersal trawling"
 Heading<-"SSR (SAB)"
 result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title,Heading, order_locations)#penetration
 tacsatEflalo<-result$tacsatEflalo#associates each ping with a pressure  
 tacsatEflalo$Pressure<-as.numeric(tacsatEflalo$Pressure)
 tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                              ifelse(tacsatEflalo$Pressure == 2, 2,
                                     ifelse(tacsatEflalo$Pressure == 3, 1, 
                                            ifelse(tacsatEflalo$Pressure == 4, 3, 
                                                   ifelse(tacsatEflalo$Pressure == 5, 3, 
                                                          ifelse(tacsatEflalo$Pressure == 6, 0,
                                                                 ifelse(tacsatEflalo$Pressure == 7, 0, 
                                                                        ifelse(tacsatEflalo$Pressure == 8, 0, 
                                                                               ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))#Next we find the swept area
 tacsatEflalo$Total<-tacsatEflalo$Score*tacsatEflalo$TR_AREA
 summary <- tacsatEflalo%>%
   group_by(Location) %>%
   summarize(Total = sum(Total))
 summary <- st_drop_geometry(summary)
 summary <- summary%>%
   mutate(
     Total = case_when(
       Location == "Five" ~ Total / five,
       Location == "Ten" ~ Total / ten,
       Location == "Twenty" ~ Total/ twenty,
       Location == "Boundary" ~ Total / Boundary,
       TRUE ~ Total # Default case, if location doesn't match any of the above
     )
   )
 
 
 tacsatEflalo <- tacsatEflalo %>%
   mutate(
     High = if_else(Score == 3, TR_AREA, 0),
     Medium = if_else(Score == 2, TR_AREA, 0),
     Low = if_else(Score == 1, TR_AREA, 0),
     Not_relevant = if_else(Score == 0, TR_AREA, 0)
   )#separating based on scores
 
 tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
 unique_vessels<- unique(tacsatEflalo_five$VE_REF)
 tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
 vessel_list <- unique(tacsatEflalo$VE_REF)
 Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
 for (vessel in vessel_list) {
   subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
   subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
   swept_sum <- sum(subset_df$TR_AREA)
   subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
   total_swept_sum <- sum(subset_total$TR_AREA)
   subset_total$Low <-((subset_total$Low * swept_sum) / (total_swept_sum))  # now for time increase
   subset_total$Medium <-  ((subset_total$Medium * swept_sum) / (total_swept_sum))  # now for time increase
   subset_total$High <-((subset_total$High * swept_sum) / (total_swept_sum))  # now for time increase
   subset_total$Not_relevant <- ((subset_total$Not_relevant * swept_sum) / (total_swept_sum))  # now for time increase
   
   subset_total$Low<-subset_total$Low*1
   subset_total$Medium <-subset_total$Medium*2
   subset_total$High <-subset_total$High*3
   subset_total$Not_relevant <-subset_total$Not_relevant*0
   subset_total$total<- subset_total$Low + subset_total$Medium + subset_total$High
   
   summary <- subset_total%>%
     filter(Location != "Five") %>%
     group_by(Location) %>%
     summarize(TR_AREA = sum(total))
   summary <- st_drop_geometry(summary)
   Final_summary<-rbind(Final_summary,summary)}
 
 summary <-  Final_summary%>%
   group_by(Location) %>%
   summarize(TR_AREA = sum(TR_AREA))
 
 summary <- summary%>%
   mutate(
     TR_AREA = case_when(
       Location == "Five" ~ TR_AREA / five,
       Location == "Ten" ~ TR_AREA / ten,
       Location == "Twenty" ~ TR_AREA/ twenty,
       Location == "Boundary" ~ TR_AREA / Boundary,
       TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
     )
   )

#Also need to plot potting and netting permits
 #Plot your area of interest
 # Specify the path to your Bathymetry file
 file_path <- "./raw_data/Bath.tif"
 # Read the file
 img <- raster(file_path)#bathymetry data
 
 #now we can plot our areas of interest
 crop_extent <- extent(-5, -1,49, 51) 
 cropped_raster <- crop(img, crop_extent)
 raster_df <- as.data.frame(cropped_raster, xy = TRUE)
 raster_df <- raster_df[raster_df$Bath <= 40, ]
 library("scatterpie")
 ports <- read.csv("./raw_data/ports.csv")
 n <- nrow(ports)
 baseMap<- ggplot() +  
   geom_sf(data = five_site, colour = "red", fill = NA, linewidth=0.5) +
   geom_sf(data = ten_site, colour = "darkgreen", fill = NA, linewidth=0.5) +
   geom_sf(data = twenty_site, colour = "yellow", fill = NA, linewidth=0.5) +
   geom_sf(data = boundary, colour = "darkblue", fill = NA, linewidth=0.5) +
   geom_sf(data = europa, colour = 1, fill = "grey") +
   geom_scatterpie(aes(x=Long, y=Lat, group=region,r=Rad/100),
                   data=ports, cols=c("Potting", "Netting"), color=NA, alpha=.8) +
   geom_scatterpie_legend(ports$Rad/100, x=-3.12, y=50.15, labeller=function(x) round(((x * 100)^2)*pi)) +
   coord_sf(xlim = c(-3.8, -3),
            ylim = c(50.1,50.7))+
   xlab("Longitude") + ylab("Latitude")+
   labs(fill="Gear Type")+
   theme_classic() +
   theme(panel.background = element_rect(fill = 'lightblue'))+
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1))+
   theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
         axis.title = element_text(face = xl$font, size = rel(xl$cex)))
 
 baseMap 


#virdis for other bar charts
#do environmental points moving- allocate high, medium, low and null to each point
#find total Swept area of HypHPMA
#find total Swept area
#find total Swept area of high sens areas increase proportionally, times by 3
#do same for medium, low and null
#add these together then divide by the total area of each grid square


#plan for habitat sens
#assign sensitivity to each ping
#calculate swept area ratio
#times swept area ratio by sensitivity
#create heatmap