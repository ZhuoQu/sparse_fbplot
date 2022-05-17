packages <- c("readr")

## Now load or install&load all
package_check <- lapply(
  packages,
  FUN <- function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

### This file plots the original malnutrition data
malnutrition <- read_csv("application/malnutrition/malnutrition.csv")
sample <- subset(malnutrition, select = c(`Geographic area`, Indicator, TIME_PERIOD, OBS_VALUE)) 
unique(sample$TIME_PERIOD)  ### 2000-2015
special_year <- c("Apr-12", "Apr-15", "Apr-18", "Aug-12", "Dec-10", "Feb-13", "Jan-10", "Jan-12", "Jan-15", 
                  "Jan-98", "Jul-12", "Jun-13", "Jun-17", "Mar-18", "May-16", "Nov-16", "Nov-98", "Sep-17")
sample$TIME_PERIOD [sample$TIME_PERIOD %in% special_year] <- c(rep("2013", 2), rep("1998", 2),
                                                               rep("2012", 2), rep("2018", 2),
                                                               rep("2016", 2), rep("2012", 2),
                                                               rep("2015", 2), rep("2017", 2), rep("2010", 2))
weight <- subset(sample, 
                 Indicator == 'Prevalence of low birth weight among new-borns', 
                 select = -Indicator)
height <- subset(sample, 
                 Indicator == 'Height-for-age <-2 SD (stunting)', 
                 select = -Indicator)

mal_time <- seq(1985, 2019, by = 1)
mal_nation <- unique(height$`Geographic area`)[1:151]

mal_height <- matrix(NA, nrow = length(mal_nation),
                     ncol = length(mal_time))
mal_weight <- matrix(NA, nrow = length(mal_nation), 
                     ncol = length(mal_time))

for (i in 1:length(mal_nation)) {
  subtime <- as.numeric(subset(weight, `Geographic area` == mal_nation[i])$TIME_PERIOD)
  time_index <- which(subtime >= 1985)
  samp_index <- subtime[time_index] - mal_time[1] + 1
  samp_weight <- subset(weight, `Geographic area` == mal_nation[i])$OBS_VALUE[time_index]
  mal_weight[i, samp_index] <- samp_weight
}

for (i in 1:length(mal_nation)) {
  subtime <- as.numeric(subset(height, `Geographic area` == mal_nation[i])$TIME_PERIOD)
  time_index <- which(subtime >= 1985)
  samp_index <- subtime[time_index] - mal_time[1] + 1
  samp_height <- subset(height, `Geographic area` == mal_nation[i])$OBS_VALUE[time_index]
  mal_height[i, samp_index] <- samp_height
}
colSums(t(mal_weight), na.rm = T) != 0
colSums(t(mal_height), na.rm = T) != 0
country_index <- intersect(which(colSums(t(mal_weight), na.rm = T) != 0),
                           which(colSums(t(mal_height), na.rm = T) != 0))
com_mal_height <- mal_height[country_index, ]
com_mal_weight <- mal_weight[country_index, ]
select_index <- which(apply(com_mal_height, 1, function(k){sum(!is.na(k)) >= 4}) == TRUE)
com_mal_height <- com_mal_height[select_index, ]
com_mal_weight <- com_mal_weight[select_index, ]
nation <- mal_nation[country_index[select_index]]

pdf(file = "org_mal.pdf", width = 9, height = 4.5)
par(mfrow = c(1,2), mai = c(0.5, 0.55, 0.3, 0.07), 
    mar = c(3.5, 3.5, 2, 1), mgp = c(1.9, 1, 0))
plot(mal_time, com_mal_height[1, ], 
     type = "n",  
     xlim = c(1985, 2019), ylim = c(0, 72),
     xlab = "Year", ylab = "Prevalence (%)", 
     main = "Prevalence of Stunted Growth (Country Level)",
     cex.main = 1, cex.axis = 0.8, cex.lab = 1)
for (i in 1:nrow(com_mal_height)) {
  points(mal_time, com_mal_height[i, ], cex = 0.5)
  height_df <- data.frame(mal_time, com_mal_height[i,])
  lines(na.omit(height_df), col = "grey", lty = 2)
  lines(height_df, col = "black", lty = 1)
}

plot(mal_time, com_mal_weight[1,], 
     type = "n", xlab = "Year", ylab = "Prevalence (%)",
     xlim = c(1985, 2019), ylim = c(0, 72), 
     main = "Prevalence of Low Birth Weight (Country Level)",
     cex.main = 1, cex.axis = 0.8, cex.lab = 1)
for (i in 1:nrow(com_mal_weight)) {
  points(mal_time, com_mal_weight[i, ], cex = 0.5)
  weight_df <- data.frame(mal_time,com_mal_weight[i, ])
  lines(na.omit(weight_df))
}
dev.off()
