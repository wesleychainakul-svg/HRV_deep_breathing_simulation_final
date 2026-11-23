install.packages("ggplot2") 
install.packages("gganimate") 
install.packages("gifski") 
library(ggplot2) 
library(gganimate) 
library(gifski)

#Parameters and variables 
time <- 1:30 
#participant has new baseline each time
HRV_baseline <- max(20, rnorm(1, 30, 4))
HRV_breathing <- HRV_baseline
HRV_normal <- HRV_baseline
#For limiting HRV values in model
HRV_minimum <- 10
HRV_maximum <-100

#Make a table 
data <- data.frame(
  Time = c(0, 0),
  HRV = c(HRV_baseline, HRV_baseline),
  Condition = c("Deep Breathing", "Without Deep Breathing"))

#Model 
for (t in time) {
  
  # Random variation
  breathing_change <- rnorm(1, 1.2, 1)
  normal_change <- rnorm(1, 0.4, 1)
  
  # Update HRV for each minute
  HRV_breathing <- HRV_breathing + breathing_change
  HRV_normal <- HRV_normal + normal_change
  
  #Keeps HRV values from going over 100 or under 10
  HRV_breathing <- max(HRV_minimum, min(HRV_maximum, HRV_breathing))
  HRV_normal <- max(HRV_minimum, min(HRV_maximum, HRV_normal))
  
  # Save deep breathing results
  new_data <- data.frame(
    Time = t,
    HRV = HRV_breathing,
    Condition = "Deep Breathing"
  )
  
  data <- rbind(data, new_data)
  
  # Save normal breathing results
  new_data <- data.frame(
    Time = t,
    HRV = HRV_normal,
    Condition = "Without Deep Breathing"
  )
  
  data <- rbind(data, new_data)
}

#Print result of model 
print(data) 

#Animated plot
graph <- ggplot(data,  aes(x = Time, y = HRV, color = Condition) 
) + 
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2) + 
  
  labs(title = "HRV With and Without Deep Breathing", subtitle = "Time: {round(frame_along)} minutes", 
       x = "Time in Minutes", 
       y = "HRV (ms)", 
       color = "Condition" 
  ) + 
  scale_color_manual(values = c("Deep Breathing" = "blue", "Without Deep Breathing" = "red") 
  ) + 
  theme_minimal() + transition_reveal(Time) 
animation <- animate(graph, nframes = 60,fps = 2) 

#Display
animation

#Save animation (add pound sign to prevent saving)
anim_save("HRV_deep_breathing_final.gif", animation) 
