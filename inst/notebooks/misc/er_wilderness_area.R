library(terra)
library(sf)
dsty <- rast('/Volumes/GoogleDrive/.shortcut-targets-by-id/1HZhH3KecyMfW0wQ8V8iJr7rmCNYOzF7t/RMBL-East River Watershed Forest Data/RMBL 2019/Data/LiDAR/tifs/ls_masked/density_100m_masked.tif')

wild <- st_read('/Volumes/GoogleDrive/.shortcut-targets-by-id/1HZhH3KecyMfW0wQ8V8iJr7rmCNYOzF7t/RMBL-East River Watershed Forest Data/RMBL 2019/Data/Geospatial/USFS_US_Wilderness/S_USA.Wilderness.shp')

wild <- st_transform(wild, 'EPSG:32613')

nobs <- global(dsty, fun='notNA')
nobsw <- global(mask(dsty, wild), fun='notNA')
nobsw/nobs

plot(dsty)
plot(wild)

head(wild)


jk <- read.csv(file.path(config$data$int, 'explainer_names_table.csv'))

plot(snow.ras[[1]], col=eval(parse(text=jk$rastcolors[1])))

study_hours <- c(6, 9, 12, 14, 30, 35, 40, 47, 51, 55, 60)
exam_scores <- c(14, 28, 50, 70, 72, 74, 78, 80, 83, 89, 107)
data <- data.frame(study_hours, exam_scores)

qm <- lm(exam_scores ~ study_hours + I(study_hours^3) , data=data)

predicted_scores <- predict(
  qm,
  newdata = data.frame(
    study_hours = seq(min(study_hours),
                      max(study_hours),
                      length.out = 100
    )
  )
)

# Plot the data points and the predicted scores
plot(
  data$study_hours,
  data$exam_scores,
  main = "Exam Scores vs. Study Hours",
  xlab = "Study Hours",
  ylab = "Exam Scores"
)
lines(seq(min(study_hours),
          max(study_hours),
          length.out = 100),
      predicted_scores, col = "red"
)

