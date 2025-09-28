data<-read.csv("Data/crossmatched_stars.csv")

# Constants
G <- 6.67430e-11               # m^3 kg^-1 s^-2
c <- 299792458                # m/s
M_sun <- 1.98847e30           # kg
M_sgra <- 4.1e6 * M_sun       # kg

# Convert parsecs to meters
parsec_to_m <- 3.0857e16

# Add time dilation column
data <- data %>%
  mutate(
    r_meters = gaia_dist_to_sgra * parsec_to_m,
    time_dilation = sqrt(1 - (2 * G * M_sgra) / (r_meters * c^2))
  )

library(ggplot2)

ggplot(data, aes(x = gaia_dist_to_sgra, y = time_dilation, color = gaia_phot_g_mean_mag)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c(option = "plasma", direction = -1)+
  labs(
    title = "Time Dilation vs Distance",
    x = "Distance to Sgr A* (parsecs)",
    y = "Time Dilation Factor",
    color = "Brightness of Star"
  ) +
  theme_minimal(base_size = 14)+
theme(
  plot.background = element_rect(fill = "black", color = NA),
  panel.background = element_rect(fill = "black", color = NA),
  legend.background = element_rect(fill = "black"),
  legend.key = element_rect(fill = "black"),
  text = element_text(color = "white"),
  axis.text = element_text(color = "white"),
  axis.title = element_text(color = "white"),
  plot.title = element_text(color = "white", face = "bold"),
  legend.title = element_text(color = "white"),
  legend.text = element_text(color = "white")
)

write.csv(data, "Data/td_data.csv", row.names = FALSE)
data<-read.csv("Data/td_data.csv")
View(data)
head(data)