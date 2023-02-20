# Packages ####
library("calendR")
library("readxl")
library("highcharter")
library("plotly")
library("reshape2")

# Syllabus ####

dates <- seq(as.Date("2023-04-01"), as.Date("2023-04-30"), by = "1 day")

# Vector of NA and weekends
# (with the week starting on Monday)
events <- ifelse(format(dates, "%w") %in% c(6, 0), "Weekend", NA)

events[c(3, 5, 10, 12, 17, 26, 28)] <- "Classes"
events[19] <- "Mid-term exam"

calendR(year = 2023, 
        month = 04, 
        start = "M",
        title = "Class calendar",
        subtitle = "April 2023",
        special.days = events,
        special.col = c("#00AAAE", "gold", "gray"),
        low.col = "white",
        legend.pos = "bottom", # Legend position
        legend.title = "",
        text = c("16:00-18:00",
                 "11:00-13:00"),             
        text.pos = c(3, 5, 10, 12, 17, 19, 26, 28),        
        text.size = 2.5,              
        text.col = "black")


# Class 1 ####

### conservation publication graph ####
cons_num <- read_xlsx("~/Documents/websites/biologia_della_conservazione/data/number_of_pubs_with_conservation.xlsx",
                      1,
                      col_names = T)

png("./figures/class_1_num_of_pubs.png", width = 10, height = 6, units = "in", res = 300)
plot(cons_num$year, cons_num$num_of_pubs, type = "l", lwd = 2, col = "red",
     xlab = "Years", ylab = "Num. of pubs", main = "Number of scientific publications \nwith the word CONSERVATION in their title.")
dev.off()

### deforestation ####
deforestation <- read.csv("./data/treecover_loss_by_region__ha.csv", header = T)
head(deforestation)

plot_ly(
  data = deforestation,
  x = ~umd_tree_cover_loss__year,
  y = ~umd_tree_cover_loss__ha,
  color = ~iso,
  type = "bar"
  ) %>% 
  layout(barmode = "stack", showlegend = FALSE,
         xaxis = list(title = "Year"),
         yaxis = list(title = "Tree cover loss (ha)"))

deforestation %>% 
  group_by(umd_tree_cover_loss__year) %>%
  mutate(prop = umd_tree_cover_loss__ha/sum(umd_tree_cover_loss__ha)) %>% 
  plot_ly(
    x = ~umd_tree_cover_loss__year,
    y = ~prop,
    color = ~iso,
    stroke = ~iso,
    type = "bar"
  ) %>% 
  layout(barmode = "stack", showlegend = FALSE,
         xaxis = list(title = "Year"),
         yaxis = list(title = "Proportion of tree cover loss"))


hc <- deforestation %>%
  hchart('column', hcaes(x = umd_tree_cover_loss__year,
                         y = umd_tree_cover_loss__ha,
                         color = iso, group = iso),
         stacking = "normal") %>% 
  hc_annotations(NULL)
hc

### fossil fuel consumption ####
ffcons <- read.csv("./data/global-fossil-fuel-consumption.csv", header = T)
head(ffcons)
names(ffcons) <- c("Entity", "Code", "Year", "Gas", "Oil", "Coal")
ffcons_long <- melt(ffcons,                               
                    id.vars = c("Entity", "Code", "Year"))

ffcons_long %>%
  plot_ly(
    x = ~Year, 
    y = ~value, 
    color = ~variable,
    type = 'scatter',
    mode = 'lines') %>% 
  layout(xaxis = list(title = "Year"),
         yaxis = list(title = "TWh"))



