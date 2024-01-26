library(tidyverse)

trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")

pca_matrix <- trans_cts %>% 
  column_to_rownames("gene") %>% 
  as.matrix() %>%  #converts dataframe to matrix
  t() #transpose matrix

sample_pca <- prcomp(pca_matrix) #prcomp: basic R statitsics, no extra packages required
#find out what kind of object this is?
class(sample_pca)
summary(sample_pca)
head(sample_pca)
?prcomp
names(sample_pca)
sample_pca$center
str(sample_pca) #list of 5
#this is a list/ complex object
#proportion of Variance = anteil principal components

pca_matrix[1:10, 1:5]
as_tibble(pca_matrix)
as_tibble(pca_matrix, rownames = "sample")

pc_eigenvalues <- sample_pca$sdev^2

pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)),
                         variance = pc_eigenvalues) %>% 
  mutate(pct = variance/sum(variance)*100) %>% 
  mutate(pct_cum = cumsum(pct))

#pareto plot/chart: combination of cumulative line plot and bar plot
pc_eigenvalues %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) +
  geom_point(aes(y = pct_cum), size = 0.75) +
  geom_hline(yintercept = 90, color = "red") #e.g. you may want to consider the pcs that explain for example 90%
  labs(x = "Principal component", y = "Fraction variance explained")

pc_scores <- sample_pca$x %>% 
  as_tibble(rownames = "sample")

pc_scores %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point()

pca_plot <- pc_scores %>% 
  full_join(sample_info, by = "sample") %>% 
  ggplot(aes(x = PC1, y = PC2,
             color = factor(minute),
             shape = strain
             )) +
  geom_point()

pc_loadings <- sample_pca$rotation %>% 
  as_tibble(rownames = "gene")

top_genes <- pc_loadings %>%
  select(gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>%
  group_by(PC) %>% 
  arrange(desc(abs(loading))) %>% 
  slice(1:10) %>% 
  pull(gene) %>% 
  unique()

top_loadings <- pc_loadings %>% 
  filter(gene %in% top_genes)

loadings_plot <- ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
                arrow = arrow(length = unit(0.1, "in")),
                color = "brown") + #adds arrows
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) #adds labels
  scale_x_continuous(expand = c(0.02, 0.02))

library(patchwork)

(pca_plot | loadings_plot)
(pca_plot | pca_plot | pca_plot) / loadings_plot #/plot moved to next "row"

(pca_plot | pca_plot | pca_plot) / loadings_plot +
    plot_annotation(tag_levels = "A") #adds ABCD annotation to figure panels
  
library(ggfortify)
autoplot(sample_pca)    

pca_plot2 <- autoplot(sample_pca, 
         data = sample_info %>% mutate(minute = as.factor(minute)), 
         color = "minute", 
         shape = "strain" )

library(broom)
tidy(sample_pca, matrix = "eigenvalues")
tidy(sample_pca, matrix = "loadings")


#Differential expression results

test_results <- read.csv("data_rnaseq/test_result.csv")
head(test_results)
#gene column -> gene name
#baseMean column -> normalized expression level of a gene
#log2FoldChange column --> amonut of change between 2 timepoints
#lfcSE column -> standard error associated to log2FoldChange value
#stat column -> statistics value computed as log2FoldChange / lfcSE compared to standard normal distribution
#pvalue -> p-value associated with the change
#padj -> p-value corrected for multiple hypothesis testing
#comparison -> comparison group

#MA plot
#Exercise: generate an MA plot (baseMean vs log2FoldChange), organize panels by comparison
#Hint: consider log-transfrom baseMean

test_results %>% 
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(size = 0.75, alpha = 0.1) +
  facet_grid(cols = vars(comparison)) #alternativ: facet_wrap(facets = vars(comparison))

test_results %>% 
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA)) #add col with significant pvals
  #ifelse function: 3 arguments: 1. condition, 2.action if TRUE, 3.action if FALSE

ma_plot <- test_results %>% 
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA)) %>% 
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(size = 0.75, alpha = 0.1) +
  geom_point(aes(y = sig), color = "tomato", size = 0.75) +
  geom_hline(yintercept = 0, color = "dodgerblue") +
  facet_grid(cols = vars(comparison))

#vulcano plots maybe eaier to interprete, but foldchange hides the expression level --> mean gives more information

(ma_plot | pca_plot2) #use patchwork to print plots next to each other

#Visualizing expression trends
#1. Get candidate genes (aka padj < 0.01)
candidate_genes <- test_results %>% 
  filter(padj < 0.01) %>% 
  pull(gene) %>% #pull function is useful for piping/tidyverse; 
  #it does sth similar to test_results[,"gene"] aka test_result$gene
  #from here on we have a vector
  unique()

#1a. Het the trans_cts table in long format
trans_cts_long <- trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3, names_to = "sample",
               values_to = "cts") %>% 
  full_join(sample_info, by = "sample")

#2. Filter trans_cts_long for candidate genes and compute 
#mean expression value for each gene in each tp and ech genotype  

trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_genes) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts = mean(cts), nrep = n()) %>% 
  #nrep/n works only in tidyverse - goves you number of groups (?)
  ungroup() #drop grouping to have a table with the entire data set 
            #(grouping was just needed for some calculations)
  
#3. Plot trends
trans_cts_mean %>%
  ggplot(aes(x = minute, y = mean_cts)) + 
  geom_line(aes(group = gene), alpha = 0.3) +
  facet_grid(rows = vars(strain))

#Scaling data to improve visualization
#z-score transformation (centers to the mean) -> mean is 0 for all samples, std dev = ???

trans_cts_mean_scaled <- trans_cts_long %>% 
  filter(gene %in% candidate_genes) %>% 
  group_by(gene) %>% 
  mutate(cts_scaled = (cts - mean(cts)) / sd(cts)) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts_scaled = mean(cts_scaled),
            nrep = n()) %>% 
  ungroup()

trans_cts_mean_scaled %>%
  ggplot(aes(x = minute, y = mean_cts_scaled)) + 
  geom_line(aes(group = gene), alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.5) +
  facet_grid(rows = vars(strain)) +
  scale_x_continuous(breaks = unique(trans_cts_mean$minute)) #takes values from data set
  #scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 120, 180))  
#plot supports running a clustering analysis

#Clustering
trans_cts <- read_csv ("data_rnaseq/counts_transformed.csv")
#1. Create a matrix of counts
hclust_matrix <- trans_cts %>% 
  select(-gene) %>% 
  as.matrix() #function to compute distance needs matrix

rownames(hclust_matrix) <- trans_cts$gene
hclust_matrix_cand <- hclust_matrix[candidate_genes, ]
#dim(hclust_matrix_cand)
hclust_matrix_cand <- hclust_matrix_cand %>% 
  t() %>% 
  scale() %>%  #applies transformation to cols -_> to compute z-score for genes, we have to transpose
  t()

