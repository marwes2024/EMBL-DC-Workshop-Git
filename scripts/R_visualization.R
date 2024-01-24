library(ggplot2)

plt <- ggplot(
  data = surveys_complete,
  mapping = aes(x = weight, y = hindfoot_length)
)

plt + 
  geom_point() +
  ggtitle("My first plot!")

#1. define ggplot object
#plt <- ggplot(data = <data.frame>, mapping = <aestethics>)
#x aestetics
#y aestetics
#color aestetics
#shape aestetics
#...
#2. add geometry layers
#gemoetry functions have predictable names
#geom_{point, line, bar, histogram, violin, hex,...}
#
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

plt <- ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

plt +
  ggtitle("Weight vs hindfoot length")

install.packages("hexbin")
library(hexbin)
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_hex()

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = "blue") # alpha: transparency of the dots

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.25, aes(color = species_id)) #here the aes/color only applies to the geom_point

ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = weight, 
    y = hindfoot_length,
    color = species_id #here the color applies globally
    )
  ) +
  geom_point(alpha = 0.25)

#Challenge: scatterplot weight vs. species_id, colored by plot_type

ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight,
    color = plot_type
  ))+
  geom_point()

ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight,
    color = plot_type
   ))+
  geom_boxplot()

ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight
  ))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.3, color = "salmon") #adding a little value for each x coord

ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight
  ))+
  geom_jitter(alpha = 0.3, size = 0.3, color = "salmon") +
  geom_boxplot(outlier.shape = NA, fill = NA)

#Challenge: produce a violin plot of weight by species_id
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight
  ))+
  geom_violin(color = "blue", fill = "pink") +
  scale_y_log10() +
  ylab("Weight (log10)")
)
# strg + i automatically formats the code to increase readibility


#Challenge: make a boxplot + jittered scatterplot of hindfoot_length by species_id
#boxplot should be in front of the dots and filled with white

ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = hindfoot_length
  ))+
  geom_jitter(alpha = 0.3, size = 0.3, color = "firebrick") +
  geom_boxplot(outlier.shape = NA, fill = "white") #fill = "white" is also the default

#to specify colors rgb function can be used: color = rgb(.3,.3,.3, alpha = 0.5)

ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = hindfoot_length
  ))+
  geom_jitter(alpha = 0.3, size = 0.6, aes(color = plot_id)) + #plot_id read as numeric variable with continous scale
  geom_boxplot(outlier.shape = NA) 

ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = hindfoot_length
  ))+
  geom_jitter(alpha = 0.3, size = 0.6, aes(color = as.factor(plot_id))) + #plot_id transformed to factor - categoric data/ descrete values
  geom_boxplot(outlier.shape = NA) 

yearly_count <- surveys_complete %>% 
  count(year, genus)

ggplot(
  data = yearly_count, 
  mapping = aes(
    x = year, 
    y = n, 
    group = genus)) + #grouping by
  geom_line()

ggplot(
  data = yearly_count, 
  mapping = aes(
    x = year, 
    y = n, 
    color = genus)) + #coloring by
  geom_line(linewidth = 1.5) +
  theme_bw() +
  theme(panel.grid = element_blank())

yearly_count %>% 
  ggplot(mapping = aes(x = year, y = n, color = genus)) +
  geom_line()

yearly_count_graph <- surveys_complete %>% 
  count(year, genus) %>% 
  ggplot(mapping = aes(x = year, y = n, color = genus)) +
  geom_line()

#facet wrap
ggplot(data = yearly_count, 
       mapping = aes(
         x = year,
         y = n)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot( 
         mapping = aes(
           x = year,
           y = n,
           color = sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot( 
    mapping = aes(
      x = year,
      y = n,
      color = sex)) +
  geom_line() +
  facet_grid(
    rows = vars(sex),
    cols = vars(genus)
    )

surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot( 
    mapping = aes(
      x = year,
      y = n,
      color = sex)) +
  geom_line() +
  facet_grid(
    rows = vars(genus),
    )

surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot( 
    mapping = aes(
      x = year,
      y = n,
      color = sex)) +
  geom_line() +
  facet_grid(
    cols = vars(genus),
  )

plt <- surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot( 
    mapping = aes(
      x = year,
      y = n,
      color = sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus)) +
  theme_bw(base_size = 18) + #usually works well for presentations
  theme(panel.grid = element_blank())

plt <- surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot( 
    mapping = aes(
      x = year,
      y = n,
      color = sex)) +
  geom_line(linewidth = 1.5) +
  facet_wrap(facets = vars(genus)
             #, scales = "free" #free x and y axis
             ) + 
  scale_color_manual(
    values = c("goldenrod", "aquamarine"),
    labels = c("female", "male"),
    name = "Sex"
  ) +
  xlab("Year of observation") +
  ylab("Number of individuals") +
  ggtitle("Observed genera over time") +
  theme_bw(base_size = 18) + #usually works well for presentations
  theme(
    
    legend.position = "bottom", # = "none" removes legend
    aspect.ratio = 1, 
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1), #vjust for vertical position
    plot.title = element_text(hjust = 0.5), #move tittle to the middle
    panel.grid = element_blank(),
    strip.background = element_blank() #removes grey background from labels above plots
    )

ggsave(filename = "data/plot.pdf",
       plot = plt,
       width = 10,
       height = 10)

#to include text (labels or p-values)
#geomtext
#annotate("text", x = ..., y = ..., label = ...)

# code to add p-values and lines
# + stat_compare_means(comparisons = my comparisons) +
# stat_compare_means(label.y = 50)
