# Test script for h1 histogram plot
library(ggplot2)
library(dplyr)

# Create fake sdLogDensity_data
set.seed(123)  # For reproducible results

# Generate fake data with varying numbers of participants to test legend scaling
n_participants <- 18  # Change this to test different legend sizes (try 4, 8, 12, 16, 20)

sdLogDensity_data <- tibble(
  participant = paste0("P", sprintf("%02d", 1:n_participants)),
  sdLogDensity = runif(n_participants, min = 0.002, max = 0.03),  # Random values between 0.002 and 0.03
  pxPerCm = runif(n_participants, min = 35, max = 55)  # Random pixel density values
) %>%
  mutate(
    # Create bin centers (same logic as in original code)
    bin_width = 0.003,
    bin_center = ifelse(sdLogDensity >= 0,
                       floor(sdLogDensity / bin_width) * bin_width + bin_width/2,
                       ceiling(sdLogDensity / bin_width) * bin_width - bin_width/2),
    ratio = pxPerCm / sdLogDensity
  ) %>%
  arrange(bin_center, participant) %>%
  group_by(bin_center) %>%
  mutate(
    stack_position = row_number(),
    dot_y = stack_position
  ) %>%
  ungroup()

# Set parameters
calibrateTrackDistanceCheckLengthSDLogAllowed <- 0.01
bin_width <- 0.003

# Calculate legend rows and dynamic sizing (same as original code)
n_participants <- n_distinct(sdLogDensity_data$participant)
legend_rows <- ceiling(n_participants / 2)

# Dynamic sizing based on number of rows
legend_text_size <- max(0.5, 8 * (5/6)^(legend_rows - 1))
legend_key_size <- 0
legend_spacing_y <- min(-1, -0.5 / (5/6)^(legend_rows - 1))
legend_key_height <- min(0.0005, 0.01 * (5/6)^(legend_rows - 1))

# Calculate x_max
data_range <- range(sdLogDensity_data$sdLogDensity)
x_max <- max(sdLogDensity_data$bin_center, calibrateTrackDistanceCheckLengthSDLogAllowed) + bin_width

# Create color palette (you might need to adjust this based on your actual colorPalette)
colorPalette <- rainbow(n_participants)
names(colorPalette) <- unique(sdLogDensity_data$participant)

# Create the h1 plot
h1 <- ggplot(sdLogDensity_data, aes(x = sdLogDensity)) +
  # Add transparent light-green bar for allowed range
  annotate("rect", 
           xmin = -Inf, 
           xmax = calibrateTrackDistanceCheckLengthSDLogAllowed, 
           ymin = 0, 
           ymax = Inf, 
           fill = "lightgreen", 
           alpha = 0.3) +
  # Stacked colored points on top of bars
  geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 3, alpha = 0.8) +
  scale_color_manual(values = colorPalette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) + 
  scale_x_continuous(limits = c(0, x_max)) +
  guides(color = guide_legend(
    nrow = 3,  
    title = "",
    override.aes = list(size = 1.5),  
    keywidth = unit(0.8, "lines"),
    byrow = TRUE,
    spacing.y = unit(-1, "lines")  # This controls spacing between rows in guide_legend
  )) +
  labs(
    subtitle = "Histogram of SD of\nlog10 pixel density",
    x = "SD of log10 pixel density",
    y = "Count"
  ) +
  theme_bw() + 
  theme(legend.key.size = unit(0, "mm"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = legend_text_size, margin = margin(t = -10, b = -10)),
        legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
        legend.box.spacing = unit(0, "lines"),
        legend.spacing.y = unit(-10, "lines"),
        legend.spacing.x = unit(0, "lines"),
        legend.key.height = unit(0, "lines"),
        legend.key.width = unit(0, "mm"),
        legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = "top", 
        legend.box = "vertical", 
        legend.justification = 'left',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size  = 10, angle = 0, hjust=0, vjust=1),
        axis.text.y = element_text(size = 10),
              plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
         plot.title.position = "plot",
         plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0)),
        plot.caption = element_text(size = 10),
        plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
        strip.text = element_text(size = 14))

# Display the plot
print(h1)

# Print some info about the dynamic sizing
cat("Number of participants:", n_participants, "\n")
cat("Legend rows:", legend_rows, "\n")
cat("Legend text size:", legend_text_size, "\n")
cat("Legend spacing Y:", legend_spacing_y, "\n")

# To test different numbers of participants, change n_participants at the top and re-run
