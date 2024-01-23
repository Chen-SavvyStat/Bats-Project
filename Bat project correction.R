#Bats Project Analysis

library(dplyr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(tidyverse)

file_path <- "C:/Users/15160/OneDrive/Desktop/bat project/Grooming body parts.csv" 
sheet_data <- read.csv(file_path)

# take first 1800 rows # read columns: observer; socially groom; self-groom
data_subset_row1800 <- sheet_data[1:1800, ]
#View(data_subset_row1800)
#summary(data_subset_row1800)

selected_columns <- data_subset_row1800 %>% 
  select("observer", "which.body.part.most.SOCIALLY.groomed.", "which.body.part.is.groomed.in.the.next.SELF.grooming.event")
#View(selected_columns)

# delete any N/A rows
selected_columns_filtered <- selected_columns %>%
  filter(
    !str_detect(`which.body.part.most.SOCIALLY.groomed.`, "0") &
      !str_detect(`which.body.part.is.groomed.in.the.next.SELF.grooming.event`, "0")
  )
#View(selected_columns_filtered)

# Extract all inputs from "which body part most SOCIALLY groomed?" and "which body part is groomed in the next SELF-grooming event"
unique_body_parts <- unique(c(
  selected_columns_filtered$`which.body.part.most.SOCIALLY.groomed.`,
  selected_columns_filtered$`which.body.part.is.groomed.in.the.next.SELF.grooming.event`
))

# Recode into categories using case_when
mapped_categories <- data.frame(
  original_value = unique_body_parts,
  category = case_when(
    unique_body_parts %in% c("iw", "inside wing", "i", "lw","IW","Inside wing","Iw","I","i ") ~ "Inside Wing",
    unique_body_parts %in% c("head", "h","Head","H") ~ "Head",
    unique_body_parts %in% c("ow", "o","OW") ~ "Outside wing",
    unique_body_parts %in% c("ub", "upper back", "shoulder", "upperback","UB","Upper back","Shoulder","Upperback") ~ "Upper back",
    unique_body_parts %in% c("face", "f", "face (&back head)", "fc","Face","Face ") ~ "Face",
    unique_body_parts %in% c("lb", "lower back", "lowerback", "ib", "l","LB","Lowerback","Lower back","lowerback ") ~ "Lower back",
    unique_body_parts %in% c("chest", "c","Chest","Chest ") ~ "Chest",
    unique_body_parts %in% c("belly", "b", "belly/UB","Belly","B","belly ") ~ "Belly",
    unique_body_parts %in% c("arm", "a","Arm") ~ "Arm",
    unique_body_parts %in% c("thumb", "thumbs", "t","Thumb","Thumbs","Thumbs ","Thumb ") ~ "Thumb",
    unique_body_parts %in% c("foot", "ft","Foot") ~ "Foot",
    TRUE ~ "Other"
  )
)
print(mapped_categories)

# Use mapped_categories to recode body parts
data_subset_row1800_recoded <- data_subset_row1800 %>%
  mutate(
    social_grooming_category = coalesce(
      mapped_categories$category[match(`which.body.part.most.SOCIALLY.groomed.`, mapped_categories$original_value)],
      "Other"
    ),
    self_grooming_category = coalesce(
      mapped_categories$category[match(`which.body.part.is.groomed.in.the.next.SELF.grooming.event`, mapped_categories$original_value)],
      "Other"
    )
  ) %>%
  filter(
    social_grooming_category != "Other" & self_grooming_category != "Other"
  )


p1 <- ggplot(data_subset_row1800_recoded, aes(x = social_grooming_category)) +
  geom_bar(fill = "blue", color = "black", position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(title = "Social Grooming", x = "Body Part", y = "Frequency")


p2 <- ggplot(data_subset_row1800_recoded, aes(x = self_grooming_category)) +
  geom_bar(fill = "green", color = "black", position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(title = "Self Grooming", x = "Body Part", y = "Frequency")

grid.arrange(p1, p2, ncol = 2)

#Table 1
social_counts <- data_subset_row1800_recoded %>%
  group_by(social_grooming_category) %>%
  summarize(count = n())

self_counts <- data_subset_row1800_recoded %>%
  group_by(self_grooming_category) %>%
  summarize(count = n())

social_table <- ggplot(data_subset_row1800_recoded, aes(x = social_grooming_category)) +
  geom_bar(stat = "count", fill = "blue", color = "black", position = "dodge") +
  labs(title = "Social Grooming", x = "Body Part", y = "Frequency") +
  theme_minimal()  

social_data <- ggplot_build(social_table)$data[[1]] %>% 
  select(x, count) %>% 
  rename(category = x, social_count = count)

self_table <- ggplot(data_subset_row1800_recoded, aes(x = self_grooming_category)) +
  geom_bar(stat = "count", fill = "green", color = "black", position = "dodge") +
  labs(title = "Self Grooming", x = "Body Part", y = "Frequency") +
  theme_minimal() 

self_data <- ggplot_build(self_table)$data[[1]] %>% 
  select(x, count) %>% 
  rename(category = x, self_count = count)

combined_data <- full_join(social_data, self_data, by = "category") %>% 
  mutate(category = recode(category, 
                           `1` = "Arm",
                           `2` = "Belly",
                           `3` = "Chest",
                           `4` = "Face",
                           `5` = "Foot",
                           `6` = "Head",
                           `7` = "Inside Wing",
                           `8` = "Lower back",
                           `9` = "Outside Wing",
                           `10` = "Thumb",
                           `11` = "Upper Back")) %>% 
  replace(is.na(.), 0)

#print(combined_data)

#new variable; percentage of rows
combined_data$social_count <- social_data$social_count
combined_data$self_count <- self_data$self_count

combined_data <- combined_data %>%
  mutate(social_count = as.numeric(social_count),
         self_count = as.numeric(self_count))

combined_data <- combined_data %>%
  mutate(social_count = as.numeric(social_count),
         self_count = as.numeric(self_count),
         row_total = rowSums(select(., -category)))

combined_data <- combined_data %>%
  mutate(
    social_percentage = (social_count / row_total) * 100,
    self_percentage = (self_count / row_total) * 100
  )
#print(combined_data)

# Separate social grooming and self-grooming data
social_data <- combined_data %>%
  filter(social_count > 0) %>%
  select(category, social_count) %>%
  arrange(desc(social_count))

self_data <- combined_data %>%
  filter(self_count > 0) %>%
  select(category, self_count) %>%
  arrange(desc(self_count))

social_data_total <- bind_rows(social_data, tibble(category = "Total", social_count = sum(social_data$social_count)))
self_data_total <- bind_rows(self_data, tibble(category = "Total", self_count = sum(self_data$self_count)))

cat("Social Grooming Counts:\n")
print(social_data_total)

cat("\nSelf Grooming Counts:\n")
#print(self_data_total)

self_data_total <- self_data_total %>%
  mutate(percentage = (self_count / sum(self_data_total$self_count[self_data_total$category != "Total"])) * 100)

cat("\nSelf Grooming Counts with Percentage:\n")
print(self_data_total)

social_data_total <- social_data_total %>%
  mutate(percentage = (social_count / sum(social_data_total$social_count[social_data_total$category != "Total"])) * 100)

cat("\nSocial Grooming Counts with Percentage:\n")
print(social_data_total)

# horizontal bar plot
gg_self <- ggplot(filter(self_data_total, category != "Total"), aes(x = reorder(category, percentage), y = percentage, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(percentage / 100), hjust = -0.2), size = 3) + 
  coord_flip() +
  labs(title = "Self Grooming Percentage",
       x = "Body parts",
       y = "Percentage") +
  theme_minimal()

gg_social <- ggplot(filter(social_data_total, category != "Total"), aes(x = reorder(category, percentage), y = percentage, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(percentage / 100), hjust = -0.2), size = 3) +  
  coord_flip() +
  labs(title = "Social Grooming Percentage",
       x = "Body parts",
       y = "Percentage") +
  theme_minimal()

grid.arrange(gg_self, gg_social, ncol = 2)


#table for Bootstrap test(social)
selected_columns_filtered2 <- selected_columns %>%
  filter(
    !str_detect(`which.body.part.most.SOCIALLY.groomed.`, "^\\s*$") &
      !str_detect(`which.body.part.is.groomed.in.the.next.SELF.grooming.event`, "^\\s*$")
  )
#View(selected_columns_filtered)


# Filter only "which.body.part.most.SOCIALLY.groomed."
selected_columns_filtered2 <- selected_columns %>%
  filter(
    !str_detect(`which.body.part.most.SOCIALLY.groomed.`, "^\\s*$") &
      !str_detect(`which.body.part.is.groomed.in.the.next.SELF.grooming.event`, "^\\s*$")
  )

event_table3 <- selected_columns_filtered2 %>%
  mutate(event = row_number(), site = `which.body.part.most.SOCIALLY.groomed.`) %>%
  select(event, site) %>%
  mutate(type = "social")

#View(event_table3)

event_table3 <- event_table3 %>%
  mutate(site = case_when(
    site %in% mapped_categories$original_value ~ mapped_categories$category[match(site, mapped_categories$original_value)],
    TRUE ~ site
  ))

event_table3 <- event_table3 %>%
  filter(site != "Other") %>%
  arrange(event) %>%
  mutate(event = row_number())

#View(event_table3)

assign("data_social", event_table3)

#View(data_social)

social <-
  data_social %>%
  filter(type=="social")

means <-
  social %>%
  group_by(site) %>%
  summarize(observed.proportion = n()/nrow(.)) %>%
  arrange(site)
means

perms <- 1000

resamples <- matrix(NA, nrow= nrow(means), ncol= perms)
rownames(resamples) <- means$site

for (i in 1:perms) {
  
  means2 <- means
  means2$observed.proportion <- 0
  
  social2 <-
    social %>%
    sample_n(size= nrow(.), replace= T)
  
  t <-
    social2 %>%
    group_by(site) %>%
    summarize(prop = n()/nrow(.) ) %>%
    arrange(site)
  
  means2$observed.proportion <- t$prop[match(means2$site, t$site)]
  
  resamples[,i] <- means2$observed.proportion
  
  print(paste(i, "in", perms))
  
}

getlow <- function(x){quantile(x, probs= c(0.025), na.rm = TRUE)}
gethigh <- function(x){quantile(x, probs= c(0.975), na.rm = TRUE)}

means$lower95 <- apply(resamples, 1, getlow)
means$mean <- apply(resamples, 1, mean, na.rm = TRUE)
#means$mean <- apply(resamples, 1, mean)
means$upper95 <- apply(resamples, 1, gethigh)

means

plot <- ggplot(means, aes(x = mean, y = reorder(site, mean))) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_errorbar(aes(y = site, xmin = lower95, xmax = upper95), height = 0.25, color = "black") +
  geom_text(aes(label = scales::percent(mean)), hjust = -0.2, size = 3) +  # Add percentage labels
  labs(title = "Social-Grooming Proportions by Body Part",
       x = "Proportion",
       y = "Body Part") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1, margin = margin(r = 5)))

print(plot)


#data for bootstrap test, self
selected_columns_filtered <- selected_columns %>%
  filter(
    !str_detect(`which.body.part.most.SOCIALLY.groomed.`, "^\\s*$") &
      !str_detect(`which.body.part.is.groomed.in.the.next.SELF.grooming.event`, "^\\s*$")
  )
#View(selected_columns_filtered)


# Filter only "which.body.part.is.groomed.in.the.next.SELF.grooming.event"
selected_columns_filtered <- selected_columns %>%
  filter(
    !str_detect(`which.body.part.most.SOCIALLY.groomed.`, "^\\s*$") &
      !str_detect(`which.body.part.is.groomed.in.the.next.SELF.grooming.event`, "^\\s*$")
  )

event_table2 <- selected_columns_filtered %>%
  mutate(event = row_number(), site = `which.body.part.is.groomed.in.the.next.SELF.grooming.event`) %>%
  select(event, site) %>%
  mutate(type = "self")

#View(event_table2)

event_table2 <- event_table2 %>%
  mutate(site = case_when(
    site %in% mapped_categories$original_value ~ mapped_categories$category[match(site, mapped_categories$original_value)],
    TRUE ~ site
  ))

event_table2 <- event_table2 %>%
  filter(site != "Other") %>%
  arrange(event) %>%
  mutate(event = row_number())

#View(event_table2)

assign("data_self", event_table2)

#View(data_self)

# first get only self-grooming probabilities
self <-
  data_self %>%
  filter(type=="self")

# get probabilities (proportions) for self-grooming
means <-
  self %>%
  group_by(site) %>%
  summarize(observed.proportion = n()/nrow(.)) %>%
  arrange(site)
means

# choose number of permutations
perms <- 1000

resamples <- matrix(NA, nrow= nrow(means), ncol= perms)
rownames(resamples) <- means$site

for (i in 1:perms) {
  
  means2 <- means
  means2$observed.proportion <- 0
  
  self2 <-
    self %>%
    sample_n(size= nrow(.), replace= T)
  
  t <-
    self2 %>%
    group_by(site) %>%
    summarize(prop = n()/nrow(.) ) %>%
    arrange(site)
  
  means2$observed.proportion <- t$prop[match(means2$site, t$site)]
  
  resamples[,i] <- means2$observed.proportion
  
  print(paste(i, "in", perms))
  
}

getlow <- function(x){quantile(x, probs= c(0.025),na.rm = TRUE)}
gethigh <- function(x){quantile(x, probs= c(0.975),na.rm = TRUE)}

means$lower95 <- apply(resamples, 1, getlow)
means$mean <- apply(resamples, 1, mean, na.rm = TRUE)
#means$mean <- apply(resamples, 1, mean)
means$upper95 <- apply(resamples, 1, gethigh)

means

# Create a horizontal bar plot with error bars and percentage labels
plot <- ggplot(means, aes(x = mean, y = reorder(site, mean))) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_errorbar(aes(y = site, xmin = lower95, xmax = upper95), height = 0.25, color = "black") +
  geom_text(aes(label = scales::percent(mean)), hjust = -0.2, size = 3) +  # Add percentage labels
  labs(title = "Self-Grooming Proportions by Body Part",
       x = "Proportion",
       y = "Body Part") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1, margin = margin(r = 5)))

print(plot)

# Chi square test for each body parts
chisq_data <- data.frame(
  category = combined_data$category,
  social_count = combined_data$social_count,
  self_count = combined_data$self_count
)

chisq_results <- data.frame(category = character(), p_value = numeric(), stringsAsFactors = FALSE)

for (part in unique(chisq_data$category)) {
  part_data <- subset(chisq_data, category == part)
  chisq_result <- chisq.test(part_data[, c("social_count", "self_count")])
  chisq_results <- rbind(chisq_results, data.frame(category = part, p_value = chisq_result$p.value))
}

print(chisq_results) 
