library(dplyr)

# Load preprocessed data
simbad <- read.csv("Data/cleaned_simbad.csv")
gaia <- read.csv("Data/cleaned_gaia.csv")

degtorad <- function(deg) deg * pi / 180

# Compute angular distance (in radians) between 2 points
angular_dist <- function(ra1, dec1, ra2, dec2) {
  ra1 <- degtorad(ra1)
  dec1 <- degtorad(dec1)
  ra2 <- degtorad(ra2)
  dec2 <- degtorad(dec2)
  
  cos_theta <- sin(dec1) * sin(dec2) + cos(dec1) * cos(dec2) * cos(ra1 - ra2)
  acos(pmin(pmax(cos_theta, -1.0), 1.0))  # Clamp to [-1, 1] to avoid NaNs
}

# Crossmatch SIMBAD with Gaia
crossmatched <- simbad %>%
  rowwise() %>%
  mutate(
    closest_gaia_idx = {
      dists <- angular_dist(ra, dec, gaia$ra, gaia$dec)
      which.min(dists)
    }
  ) %>%
  ungroup() %>%
  mutate(
    gaia_source_id = gaia$source_id[closest_gaia_idx],
    gaia_ra = gaia$ra[closest_gaia_idx],
    gaia_dec = gaia$dec[closest_gaia_idx],
    gaia_distance = gaia$distance[closest_gaia_idx],
    gaia_radial_velocity = gaia$radial_velocity[closest_gaia_idx],
    gaia_dist_to_sgra = gaia$dist_to_sgra[closest_gaia_idx],
    gaia_phot_g_mean_mag = gaia$phot_g_mean_mag[closest_gaia_idx]
  ) %>%
  select(-closest_gaia_idx)

simbad_cols <- names(simbad)
names(crossmatched)[names(crossmatched) %in% simbad_cols] <- paste0("simbad_", simbad_cols)

# Save result
write.csv(crossmatched, "Data/crossmatched_stars.csv", row.names = FALSE)
View(crossmatched)
