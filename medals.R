# Creating a bar plot just for fun!

library(tidyverse)
library(patchwork)
library(showtext)

# Establishing theme

font_add("Source Sans Pro", regular = "SourceSansPro-Regular.ttf",
         bold = "SourceSansPro-SemiBold.ttf")# Activar showtext
showtext_auto()
mifont = "Source Sans Pro" 

theme_daniel <- function(){
  font <- mifont
  theme_minimal() %+replace%    
    theme(panel.background =  element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor  = element_line(linetype = "dotted"),
          panel.grid.major.y = element_line(color = "grey", size = 1.2, linetype = "dotted"),
          panel.border = element_blank(),
          axis.line = element_blank(), 
          axis.text.x = element_text(face= "bold", size = 19, colour = "grey20"),
          axis.text.y = element_text(face= "bold", margin = margin(r = 9, l = 15), vjust= 0.5, hjust = 1.4, size = 20, colour = "grey20"),
          text = element_text(size=15, colour = "grey20", family = mifont),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(
            colour = "grey20",
            family = font,
            face = "bold",
            size = 31),               
          plot.subtitle = element_text(
            colour = "grey25",
            family = font, 
            vjust = -1, 
            size = 23),               
          plot.caption = element_text(           
            family = font),  
          axis.title.y = element_text(             
            family = font, face = "bold", size = 22, colour = "grey25", angle = 90),
          axis.text = element_text(              
            family = font),
          legend.title = element_text(family = font, face = "bold", size = 23, colour = "grey25"),
          legend.text = element_text(family = font, size = 23))
}

# Creating the data 

dataframe <- data.frame("Name" = LETTERS[seq(1, 10, 1)],
                        "Number" = seq(10, 100, by = 10))

serie1 <- rnorm(1000, mean = 100, sd = 1)
serie2 <- rnorm(1000, mean = 100, sd = 1)

dataframe2 <- data.frame("var1" = serie1,
                         "var2" = serie2) %>% 
  mutate(color = ifelse(var2 > 100, "#F46036", "#2E294E"))

dataframetime <- data.frame(
  day = as.Date("2017-01-01") + 0:364,
  value = runif(365) + seq(-140, 224)^2 / 10000
)

# Establishing the colors

colors <- c("#03071e", "#370617", "#6a040f", "#9d0208", "#d00000", 
            "#dc2f02", "#e85d04", "#f48c06", "#faa307", "#ffba08")

dataframe <- dataframe %>% arrange(desc(Number)) %>% 
  mutate(color = colors)

colors_density <- c("Fair" = "#ffbe0b", 
                    "Good" = "#fb5607", 
                    "Very Good" = "#ff006e", 
                    "Premium" = "#8338ec", 
                    "Ideal" = "#3a86ff")

####-- BARPLOT

medals_res_0 <- data.frame("Country" = c("USA", "CHN", "JPN", "AUS", "FRA", 
                                       "NLD", "GBR", "KOR", "ITA", "DEU"),
                        "Gold_medals" = c(40, 40, 20, 18, 16, 15, 14, 13, 12, 12),
                        "Silver_medals" = c(44, 27, 12, 19, 26, 7, 22, 9, 13, 13),
                        "Bronze_medals" = c(42, 24, 13, 16, 22, 12, 29, 10, 15, 8))

#medals_0 <- data.frame("Country" = c("United States of America", 
                                   #"People's Republic of China", "Japan", "Australia", 
                                   #"France",
                                   #"Netherlands",
                                   #"Great Britain", 
                                   #"Republic of Korea", 
                                   #"Italy", 
                                   #"Germany"),
                         #"Gold_medals" = c(40, 40, 20, 18, 16, 15, 14, 13, 12, 12),
                         #"Silver_medals" = c(44, 27, 12, 19, 26, 7, 22, 9, 13, 13),
                         #"Bronze_medals" = c(42, 24, 13, 16, 22, 12, 29, 10, 15, 8))

medals_0 <- data.frame("Country" = c("USA", "CHN", "JPN", "AUS", "FRA", 
                                     "NLD", "GBR", "KOR", "ITA", "DEU"),
                       "Gold_medals" = c(40, 40, 20, 18, 16, 15, 14, 13, 12, 12),
                       "Silver_medals" = c(44, 27, 12, 19, 26, 7, 22, 9, 13, 13),
                       "Bronze_medals" = c(42, 24, 13, 16, 22, 12, 29, 10, 15, 8))

medals_res <- medals_res_0 %>% pivot_longer(cols = c(contains("medals")), 
                            names_to = "Medals",
                            values_to = "Number")

medals <- medals_0 %>% pivot_longer(cols = c(contains("medals")), 
                                            names_to = "Medals",
                                            values_to = "Number") %>%
  mutate(Medals = factor(Medals, levels = c("Bronze_medals", 
                                            "Silver_medals", 
                                            "Gold_medals"))) %>%
  mutate(Order = case_when(Medals == "Gold_medals" ~ 1,
                           Medals == "Silver_medals" ~ 2,
                           Medals == "Bronze_medals" ~ 3))

# Color of medals

color_med <- c("Gold_medals" = "#fcc861",
               "Silver_medals" = "#e5e5e5",
               "Bronze_medals" = "#dcb386")
# Plotting

micaption <- "Source: Paris 2024 Olympics, https://olympics.com (Last updated:August 12, 2024)"

g_medals <- medals %>% ggplot(aes(x = reorder(Country, Order), y = Number, fill = Medals)) + 
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Top 10 Countries with the Most Medals in Paris 2024 Olympics",
       subtitle = NULL,
       x = NULL,
       y = "Number of Medals",
       caption = micaption) +
  geom_text(data = medals %>% group_by(Country) %>% summarise(total = sum(Number)),
            aes(x = Country, y = total, label = total),
            inherit.aes = FALSE,
            size = 7, colour = "gray20", fontface = "bold", vjust = -1) +
  geom_text(aes(x = reorder(Country, -Number), 
                y = Number, 
                label = Number),
            position = position_stack(vjust = 0.5),
            size = 5, colour = "gray20", fontface = "bold") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 140),
                     breaks = c(25, 50, 75, 100),
                     labels = c(25, 50, 75, 100)) +
  scale_fill_manual(values = color_med, 
                    name = "Medal Type",
                    labels = c("Bronze", "Silver", "Gold")) +
  theme_daniel() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 55),
        plot.caption = element_text(margin = margin(t = 15),
                                    size = 15)) 

svg("C:/Users/analisis.AMBIENTE/Downloads/medals_gold.svg", width = 17, height = 11, bg = "transparent")
print(g_medals)
dev.off()

#https://olympics.com/en/paris-2024/medals


##########################################



E <- dataframe %>% ggplot(aes(x = Name, y = Number)) +
  geom_point(size = 4.5, color = dataframe$color) +
  labs(title = "An example of a point plot in R",
       subtitle = "Made in ggplot2 package",
       x = "Title of the X axe",
       y = "Title of the Y axe") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

C <- dataframe2 %>% ggplot(aes(x = var1, y = var2)) +
  geom_point(size = 2, col = dataframe2$color) +
  labs(title = "An example of a dispersion plot in R",
       subtitle = "Made in ggplot2 package",
       x = "Title of the X axe",
       y = "Title of the Y axe") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

D <- ggplot(data=diamonds, aes(x = price, group = cut, fill= cut)) +
  geom_density(adjust=1.5, alpha=.4) +
  labs(title = "An example of a density plot in R",
       subtitle = "Made in ggplot2 package",
       x = "Title of the X axe",
       y = "Title of the Y axe") +
  scale_fill_manual(values = colors_density) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))


ggplot(dataframetime, aes(x = day, y = value)) +
  geom_line(col = "#ef233c", linewidth = 1) + 
  labs(title = "An example of a time series plot in R",
       subtitle = "Made in ggplot2 package",
       x = "Title of the X axe",
       y = "Title of the Y axe") +
  scale_x_date(date_breaks = "1 month",
               date_labels = c("dic", "ene", "feb", "mar", "abr", 
                               "may", "jun", "jul", "ago", 
                               "sep", "oct", "nov"),
               expand = c(0.03,0.03))
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
  


  
  
  
# Joining graphs!

all <- A + B + C + D +
  plot_layout(ncol = 2, nrow = 2)



###

medals_0 <- medals_0 %>%
  mutate(Order = rank(-Gold_medals, ties.method = "first"))

medals <- medals_0 %>% pivot_longer(cols = c(contains("medals")), 
                                    names_to = "Medals",
                                    values_to = "Number") %>%
  mutate(Medals = factor(Medals, levels = c("Bronze_medals", 
                                            "Silver_medals", 
                                            "Gold_medals")))

# Color de las medallas
color_med <- c("Gold_medals" = "#fcc861",
               "Silver_medals" = "#e5e5e5",
               "Bronze_medals" = "#dcb386")

g_medals <- medals %>% 
  ggplot(aes(x = reorder(Country, Order), y = Number, fill = Medals)) + 
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Top 10 Countries with the Most Gold Medals in Paris 2024 Olympics",
       subtitle = "Prioritized by gold medals",
       x = NULL,
       y = "Number of Medals",
       caption = micaption) +
  geom_text(data = medals %>% group_by(Country) %>% summarise(total = sum(Number)),
            aes(x = Country, y = total, label = total),
            inherit.aes = FALSE,
            size = 7, colour = "gray20", fontface = "bold", vjust = -1) +
  geom_text(aes(x = reorder(Country, Order), 
                y = Number, 
                label = Number),
            position = position_stack(vjust = 0.5),
            size = 5, colour = "gray20", fontface = "bold") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 140),
                     breaks = c(25, 50, 75, 100),
                     labels = c(25, 50, 75, 100)) +
  scale_fill_manual(values = color_med, 
                    name = "Medal Type",
                    labels = c("Bronze", "Silver", "Gold")) +
  theme_daniel() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 55),
        plot.caption = element_text(margin = margin(t = 15),
                                    size = 15)) 
