library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Fetch Gaia Data from ESA TAP+ API
endpoint <- "https://gea.esac.esa.int/tap-server/tap/sync"

query <- "
SELECT TOP 1000
  source_id,
  ra,
  dec,
  parallax,
  radial_velocity,
  pmra,
  pmdec,
  phot_g_mean_mag
FROM gaiadr3.gaia_source
WHERE
  parallax IS NOT NULL AND
  radial_velocity IS NOT NULL AND
  pmra IS NOT NULL AND
  pmdec IS NOT NULL
"

response <- POST(
  url = endpoint,
  body = list(
    REQUEST = "doQuery",
    LANG = "ADQL",
    FORMAT = "json",
    QUERY = query
  ),
  encode = "form"
)

# Parse Response
if (response$status_code == 200) {
  result_text <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result_text)
  gaia <- as.data.frame(data$data)
  colnames(gaia) <- data$metadata$name
  write.csv(gaia, "Data/gaia_raw_api.csv", row.names = FALSE)
} else {
  stop(paste("Error:", response$status_code))
}

# Load downloaded data
gaia <- read.csv("Data/gaia_raw_api.csv")

# Select relevant columns
gaia <- gaia %>%
  select(source_id, ra, dec, parallax, radial_velocity, pmra, pmdec, phot_g_mean_mag)

# Handle missing values
gaia <- gaia %>%
  filter(!is.na(radial_velocity), !is.na(parallax), !is.na(pmra), !is.na(pmdec))

# Convert parallax to distance (in parsecs)
gaia <- gaia %>%
  mutate(distance = ifelse(parallax > 0, 1000 / parallax, NA)) %>%
  filter(!is.na(distance), distance > 0 & distance < 100000)

# Function to remove IQR outliers
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25)
  Q3 <- quantile(data[[column]], 0.75)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  data %>% filter(data[[column]] >= lower & data[[column]] <= upper)
}

# Apply outlier removal
gaia <- remove_outliers(gaia, "radial_velocity")
gaia <- remove_outliers(gaia, "pmra")
gaia <- remove_outliers(gaia, "pmdec")

# Distance of stars to Sgr A* Black hole
sgrA_ra <- 266.41683
sgrA_dec <- -29.00781
sgrA_distance <- 8000

degtorad <- function(deg) deg * pi / 180

# Angular distance from Sgr A*
gaia <- gaia %>%
  mutate(
    ang_dist_sgra = acos(
      sin(degtorad(dec)) * sin(degtorad(sgrA_dec)) +
        cos(degtorad(dec)) * cos(degtorad(sgrA_dec)) *
        cos(degtorad(ra - sgrA_ra))
    )
  )

# Approximate linear distance to Sgr A* using Law of Cosines
gaia <- gaia %>%
  mutate(
    dist_to_sgra = sqrt(distance^2 + sgrA_distance^2 -
                          2 * distance * sgrA_distance * cos(ang_dist_sgra))
  )

# Normalize 
gaia <- gaia %>%
  mutate(
    radial_velocity = scale(radial_velocity),
    pmra = scale(pmra),
    pmdec = scale(pmdec),
    distance = scale(distance),
  )

# Save cleaned file
write.csv(gaia, "Data/cleaned_gaia.csv", row.names = FALSE)

# Load to check
cleaned <- read.csv("Data/cleaned_gaia.csv")
View(cleaned)
