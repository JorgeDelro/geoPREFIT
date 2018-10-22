# DESCRIPTIVES

# Packages
library(tidyverse)
library(rgdal)
library(tmap)
library(ggpubr)
#library(psych)

# read database
db <- read.csv2("full_db.csv")

# Select variables
db <- db %>%
  select(ID_2, Sex_cat, Age, Wgt1, Wgt2, Hgt1, Hgt2, City_cat, HG, SLJ, P4X10M, Sol) %>%
  mutate(Weight = (Wgt1 + Wgt2)/2,
         Height = (Hgt1 + Hgt2)/2)

# Check database
str(db)

# Descriptives
descriptives_PREFIt <- db %>%
                        select(Age, Weight, Height, City_cat, HG, SLJ, P4X10M, Sol) %>%
                        group_by(City_cat) %>%
                        summarize(Age_mean = mean(Age, na.rm=T), Age_sd = sd(Age, na.rm=T),
                            Weight_mean = mean(Weight, na.rm=T), Weight_sd = sd(Weight, na.rm=T),
                            Height_mean = mean(Height, na.rm=T), Height_sd = sd(Height, na.rm=T),
                            HG_mean = mean(HG, na.rm=T), HG_sd = sd(HG, na.rm=T),
                            SLJ_mean = mean(SLJ, na.rm=T), SLJ_sd = sd(SLJ, na.rm=T),
                            P4X10M_mean = mean(P4X10M, na.rm=T), P4X10M_sd = sd(P4X10M, na.rm=T),
                            Sol_mean = mean(Sol, na.rm=T), Sol_sd = sd(Sol, na.rm=T))

  
  
# Create a map of Spain to plot descriptives
# Spain map (OGR data)
spain <- readOGR(dsn = "data", layer = "Provincias_ETRS89_30N")

# Merge Spain map with PREFIT data
## Calculate mean by City
hg_ag <- aggregate(HG ~ City_cat, FUN = mean, data = db)
slj_ag <- aggregate(SLJ ~ City_cat, FUN = mean, data = db)
P4X10M_ag <- aggregate(P4X10M ~ City_cat, FUN = mean, data = db)
Sol_ag <- aggregate(Sol ~ City_cat, FUN = mean, data = db)
# PA_Level <- aggregate(PA_Level ~ City, FUN = mean, data = db)


# Create vectors with the same orders as the db_plot
# Order: Vitoria - Almería - Mallorca - Cádiz - Castellón - Cuenca - Granada - Madrid - Las Palmas - Zaragoza
values_SLJ <- c(75.18425,NA,NA,62.88406,NA,NA,77.84892,NA,NA,NA,72.58393,78.35769,NA,NA,NA,71.09690,NA,73.92500,NA,NA,NA,NA,NA,NA,NA,NA,NA,74.96310,NA,NA,NA,NA,NA,NA,69.10078,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,78.08070,NA,NA)
values_HG <- c(6.722720,NA,NA,6.4,NA,NA,6.910252,NA,NA,NA,6.609541,7.340385,NA,NA,NA,6.344231,NA,8.088529,NA,NA,NA,NA,NA,NA,NA,NA,NA,7.408465,NA,NA,NA,NA,NA,NA,7.311047,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,7.121930,NA,NA)
values_P4X10M <- c(16.45397,NA,NA,17.62975,NA,NA,16.57584,NA,NA,NA,17.15192,18.50204,NA,NA,NA,16.87189,NA,15.73542,NA,NA,NA,NA,NA,NA,NA,NA,NA,17.40730,NA,NA,NA,NA,NA,NA,15.82122,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,16.91176,NA,NA)
values_Sol <- c(12.416217,NA,NA,11.559928,NA,NA,9.251817,NA,NA,NA,12.212350,12.918301,NA,NA,NA,14.228745,NA,22.687893,NA,NA,NA,NA,NA,NA,NA,NA,NA,15.128024,NA,NA,NA,NA,NA,NA,16.136240,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,10.292042,NA,NA)
# values_PA_Level <- c(3.915171,NA,NA,3.392857,NA,NA,3.543165,NA,NA,NA,3.485612,3.579151,NA,NA,NA,3.504098,NA,3.334728,NA,NA,NA,NA,NA,NA,NA,NA,NA,3.521912,NA,NA,NA,NA,NA,NA,3.492188,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3.579505,NA,NA)

# Sol
# Add values
db_plot <- spain$Texto
db_plot <- data.frame(db_plot,values_Sol)
## Check that the name of the variables match
# spain$Texto %in% db_plot$db_plot
# Rename db_plot to Texto to make it match
db_plot <- rename(db_plot, Texto = db_plot)
# Join databases
spain@data <- left_join(spain@data,db_plot)
## Use function fortify()
spain_f <- fortify(spain)
head(spain_f)
# Allocate an id variable to the sp data
spain$id <- row.names(spain) 
# Final check before join 
# head(spain@data, n = 2) 
# Join data
spain_f <- left_join(spain_f, spain@data) 
# Check data frame
#spain_f[1:2, 1:8]

# Plot descriptives


# HG
db_plot <- spain$Texto
db_plot <- data.frame(db_plot,values_HG)
## Check that the name of the variables match
# spain$Texto %in% db_plot$db_plot
# Rename db_plot to Texto to make it match
db_plot <- rename(db_plot, Texto = db_plot)
# Join databases
spain@data <- left_join(spain@data,db_plot)
## Use function fortify()
spain_f <- fortify(spain)
head(spain_f)
# Allocate an id variable to the sp data
spain$id <- row.names(spain) 
# Final check before join 
# head(spain@data, n = 2) 
# Join data
spain_f <- left_join(spain_f, spain@data) 
# Check data frame
#spain_f[1:2, 1:8]
HG_map <- ggplot(spain_f, aes(long, lat, group = group, fill = values_HG)) + 
  geom_polygon() +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)", fill = " ") + 
  ggtitle("Handgrip test (kg)") + 
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        legend.justification=c(1,0), 
        legend.position=c(0.25,0.4), 
        legend.text=element_text(size=6),
        legend.title=element_blank(), 
        legend.key.size = unit(0.4, "cm")) + 
  scale_fill_gradient(low = "yellow", high = "red4")

# SLJ
db_plot <- spain$Texto
db_plot <- data.frame(db_plot,values_SLJ)
## Check that the name of the variables match
# spain$Texto %in% db_plot$db_plot
# Rename db_plot to Texto to make it match
db_plot <- rename(db_plot, Texto = db_plot)
# Join databases
spain@data <- left_join(spain@data,db_plot)
## Use function fortify()
spain_f <- fortify(spain)
head(spain_f)
# Allocate an id variable to the sp data
spain$id <- row.names(spain) 
# Final check before join 
# head(spain@data, n = 2) 
# Join data
spain_f <- left_join(spain_f, spain@data) 
# Check data frame
#spain_f[1:2, 1:8]
SLJ_map <- ggplot(spain_f, aes(long, lat, group = group, fill = values_SLJ)) + 
  geom_polygon() +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)", fill = " ") + 
  ggtitle("Standing long jump test (cm)") + 
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        legend.justification=c(1,0), 
        legend.position=c(0.25,0.4), 
        legend.text=element_text(size=6),
        legend.title=element_blank(), 
        legend.key.size = unit(0.4, "cm")) + 
  scale_fill_gradient(low = "yellow", high = "red4")

# P4X10M
db_plot <- spain$Texto
db_plot <- data.frame(db_plot,values_P4X10M)
## Check that the name of the variables match
# spain$Texto %in% db_plot$db_plot
# Rename db_plot to Texto to make it match
db_plot <- rename(db_plot, Texto = db_plot)
# Join databases
spain@data <- left_join(spain@data,db_plot)
## Use function fortify()
spain_f <- fortify(spain)
head(spain_f)
# Allocate an id variable to the sp data
spain$id <- row.names(spain) 
# Final check before join 
# head(spain@data, n = 2) 
# Join data
spain_f <- left_join(spain_f, spain@data) 
# Check data frame
#spain_f[1:2, 1:8]
P4X10M_map <- ggplot(spain_f, aes(long, lat, group = group, fill = values_P4X10M)) + 
  geom_polygon() +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)", fill = " ") + 
  ggtitle("4x10m shuttle run test (s)") + 
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        legend.justification=c(1,0), 
        legend.position=c(0.25,0.4), 
        legend.text=element_text(size=6),
        legend.title=element_blank(), 
        legend.key.size = unit(0.4, "cm")) + 
  scale_fill_gradient(low = "yellow", high = "red4")


# Sol
# Add values
db_plot <- spain$Texto
db_plot <- data.frame(db_plot,values_Sol)
## Check that the name of the variables match
# spain$Texto %in% db_plot$db_plot
# Rename db_plot to Texto to make it match
db_plot <- rename(db_plot, Texto = db_plot)
# Join databases
spain@data <- left_join(spain@data,bd_plot)
## Use function fortify()
spain_f <- fortify(spain)
head(spain_f)
# Allocate an id variable to the sp data
spain$id <- row.names(spain) 
# Final check before join 
# head(spain@data, n = 2) 
# Join data
spain_f <- left_join(spain_f, spain@data) 
# Check data frame
#spain_f[1:2, 1:8]
Sol_map <- ggplot(spain_f, aes(long, lat, group = group, fill = values_Sol)) + 
  geom_polygon() +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)", fill = " ") + 
  ggtitle("One-leg stance test (s)") + 
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        legend.justification=c(1,0), 
        legend.position=c(0.25,0.4), 
        legend.text=element_text(size=6),
        legend.title=element_blank(), 
        legend.key.size = unit(0.4, "cm")) + 
  scale_fill_gradient(low = "yellow", high = "red4")

# Save plots
ggarrange(HG_map, 
          SLJ_map,
          P4X10M_map,
          Sol_map,
          ncol = 2, nrow = 2)
ggsave("Descriptives.png", height=5, width=5, units='in', dpi=600)






