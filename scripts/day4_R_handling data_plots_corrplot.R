library(tidyverse)
raw_cts <- read_csv("data_rnaseq/counts_raw.csv")
trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")
test_result <- read_csv("data_rnaseq/test_result.csv")

trans_cts_long <- trans_cts %>% 
  pivot_longer(names_to = "sample", 
               values_to = "cts", 
               cols = wt_0_r1:mut_180_r3 #select col "wt_0_1" to col "wt_0_3"
  )

trans_cts_long <- full_join(trans_cts_long, sample_info, by = "sample") 
#by defines which column tables are combined by
#multiple columns can be specified for joining (passed to "by" by vector)
#only two tables can be joined at once

trans_cts_long %>% 
  ggplot(aes(x = cts)) +
  geom_freqpoly()

trans_cts_long %>% 
  ggplot(aes(x = cts, color = replicate)) +
  geom_freqpoly() 

#define bin width (also works with default, as coded above)
trans_cts_long %>% 
  ggplot(aes(x = cts, color = replicate)) +
  geom_freqpoly(binwidth = 1) 

#create single plots, arranged by strain and time point
trans_cts_long %>% 
  ggplot(aes(x = cts, color = replicate)) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

#Exercise: make the same plot, based on the raw counts

raw_cts_long <- raw_cts %>% 
  pivot_longer(names_to = "sample", 
               values_to = "cts", 
               cols = wt_0_r1:mut_180_r3 
  ) %>% 
full_join(sample_info, by = "sample")

raw_cts_long %>% 
  ggplot(aes(x = cts, color = replicate)) +
  geom_freqpoly() +
  scale_x_log10() +
  facet_grid(rows = vars(strain), cols = vars(minute))

raw_cts_long %>% 
  ggplot(aes(x = log10(cts), color = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute))

#Warnmeldung:
#Removed 645 rows containing non-finite values (`stat_bin()`).
#due to 0 in the data set (log10(0) can't be calculated and returns -Inf)

raw_cts_long %>% 
  ggplot(aes(x = log10(cts+1), color = replicate)) + #+1 to include log of 0-values (for high values this doesn't change much)
  geom_freqpoly(binwidth = 1) + # does binwidth refer to original or log-transformed data?
  facet_grid(rows = vars(strain), cols = vars(minute))

#make a boxplot
raw_cts_long %>% 
  ggplot(aes(x = factor(minute), y = log10(cts+1), fill = strain)) + #+1 to include log of 0-values (for high values this doesn't change much)
  geom_boxplot(outlier.size = 0.5) + 
  facet_grid(cols = vars(replicate))

#compare replicates
raw_cts_long %>% 
  ggplot(aes(x = factor(minute), y = log10(cts+1), fill = replicate)) +
  geom_boxplot() +
  facet_grid(col = vars(strain))

#Correlation between wt sample at T0 and T30 - scatterplot based on wide_format
trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_30_r1)) +
  geom_point(size = 0.5) +
  geom_abline(color = "brown")

#Correlation between replicates 1+2 of wt sample at T0
trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point(size = 0.5) +
  geom_abline(color = "brown")

#Look at correlation of count data across all samples in our experiment
trans_cts_corr <- trans_cts %>% 
  select(-gene) %>% 
  cor(method = "spearman") # gives out matrix of pairwise correlations

library(corrr)

rplot(trans_cts_corr) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#hjust = 0 left; hjust = 0.5 middle; hjust = 1 right

##Compare trans_cts and raw_cts
summary(raw_cts_long$cts)
summary(trans_cts_long$cts) # one of the modifications made to raw data: log-transform

raw_cts %>% ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point(size = 0.5)

raw_cts %>% 
  ggplot(aes(x = wt_0_r1 + 1, y = wt_0_r2 + 1)) +
  geom_point(size = 0.5) +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

raw_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point() +
  geom_abline(color = "brown") +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")
#usually mean and variance from gene expression data are positively correlated
#raw data go through different processes(e.g. deseq2 package) for transformation

trans_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point()

#color points according to count value
trans_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  mutate(above_four = var_cts > 4) %>% 
  ggplot(aes(x = mean_cts, y = var_cts, color = above_four)) +
  geom_point()
