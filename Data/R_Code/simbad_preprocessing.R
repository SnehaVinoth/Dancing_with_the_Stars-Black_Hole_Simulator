library(httr)
library(jsonlite)

#Data Collection
endpoint <- "https://simbad.u-strasbg.fr/simbad/sim-tap/sync"

query <- "
SELECT TOP 2000
  main_id,
  ra,
  dec,
  rvz_redshift
FROM basic
WHERE
  ra IS NOT NULL
  AND dec IS NOT NULL
  AND rvz_redshift IS NOT NULL
ORDER BY rvz_redshift DESC
"

# Make the POST request
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

# Parse the response
if (response$status_code == 200) {
  result_text <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result_text)
  stars <- as.data.frame(data$`data`)  
  colnames(stars) <- data$`metadata`$name
  print(head(stars,10))
  write.csv(stars,"simbad_data")
} else {
  cat("Error: ", response$status_code, "\n")
}

#Data Preprocessing
library(dplyr)
library(ggplot2)
df<-read.csv("simbad_data")

#Dropping Row Number column
df<-df[,-1]

#Convert redshift to radial velocity
c_km_s <- 299792.458
df$radial_velocity <- df$rvz_redshift * c_km_s
View(df)
#Handling Missing Values
df<-data.frame(lapply(df,function(x){
  if(is.numeric(x)){
    x[is.na(x)]<-mean(x,na.rm=TRUE)
  }
  return(x)
}))

#Analyzing Data to detect Outliers
ggplot(df, aes(x = radial_velocity)) +
  geom_histogram(bins = 200, fill = "lightblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Radial Velocity", x = "Radial Velocity (km/s)", y = "Count")


#Remove Outliers using IQR Method
Q1 <- quantile(df$radial_velocity, 0.25)
Q3 <- quantile(df$radial_velocity, 0.75)
IQR_val <- Q3 - Q1

df <- df[df$radial_velocity > (Q1 - 1.5 * IQR_val) &
           df$radial_velocity < (Q3 + 1.5 * IQR_val), ]

#Normalize Radial Velocity
df$rv_norm <- scale(df$radial_velocity)

#Filtering stars closest to black hole Sgr A*
# Define Sagittarius A* coordinates
sgrA_ra <- 266.41683
sgrA_dec <- -29.00781

degtorad <- function(deg) return(deg * pi / 180)

# Compute angular distance (in radians)
df$ang_dist <- acos(
  sin(degtorad(df$dec)) * sin(degtorad(sgrA_dec)) +
    cos(degtorad(df$dec)) * cos(degtorad(sgrA_dec)) *
    cos(degtorad(df$ra - sgrA_ra))
)
df<-arrange(df,ang_dist)
df<-head(df,1000)

#Save dataset
write.csv(df, "Data/cleaned_simbad.csv", row.names = FALSE)

# Load to check
cleaned <- read.csv("Data/cleaned_simbad.csv")
View(cleaned)
