
library(tidyverse)
library(spData)
library(sf)
library(ggfx)
library(viridis)
library(viridisLite)
library(ggalluvial)
library(ggthemes)
library(dplyr)
library(egg)
library(cowplot)
library(patchwork)

data(world)
world <- subset(world, continent != "Antarctica")

setwd('C:\\Users\\FBaudron\\Documents\\CIMMYT\\0. Publi\\Bentley et al\\')

W <-read.csv("wheat.csv")
cor <- read.csv("cor.csv")

W <- W[c(4,6,10,12)]
W[is.na(W)] <- 0

W$Area <- ifelse(W$Area == "China, Hong Kong SAR", "China", W$Area)
W$Area <- ifelse(W$Area == "China, Macao SAR", "China", W$Area)
W$Area <- ifelse(W$Area == "China, mainland", "China", W$Area)
W$Area <- ifelse(W$Area == "China, Taiwan Province of", "China", W$Area)


imp <- W[ which(W$Element == "Import Quantity"), ]
exp <- W[ which(W$Element == "Export Quantity"), ]

names(imp)

imp <- imp[,c(1,4)] %>%
  group_by(Area) %>% 
  summarise_each(funs(mean))

exp <- exp[,c(1,4)] %>%
  group_by(Area) %>% 
  summarise_each(funs(mean))

imp <- merge(imp, cor, by = "Area", all.x = TRUE)
exp <- merge(exp, cor, by = "Area", all.x = TRUE)

imp <- merge(world, imp, by = "name_long", all.x = TRUE)
exp <- merge(world, exp, by = "name_long", all.x = TRUE)

imp <- 
  st_cast(imp, 'MULTIPOLYGON') %>%
  st_transform(crs = "+proj=moll")

exp <- 
  st_cast(exp, 'MULTIPOLYGON') %>%
  st_transform(crs = "+proj=moll")


pimp <- ggplot(imp) +
  geom_sf(aes(fill = Value/1000), size = 0.2, color = "grey90", na.rm = TRUE) +
  scale_fill_viridis_c(name="Million tonnes", option = "F") +
  theme_void() +
  labs(title = "b. Annual wheat imports (means of 2018 & 2019)")+ 
  theme(
    legend.position = c(0.05,0.025), legend.justification = c(0.05,0.025),
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.title = element_text(color = "white", size = 22, face = "bold", hjust=0.05, margin = margin(0,0,10,0)),
    legend.title = element_text(size = 18, color = "white", margin = margin(0,0,10,0)),
    legend.text = element_text(size = 14, color = "white", margin = margin(0,0,10,0)),
    legend.key.size = unit(1, "cm"),
    plot.margin = margin(5, 0, 10, 0))

pimp

pexp <- ggplot(exp) +
  geom_sf(aes(fill = Value/1000), size = 0.2, color = "grey90", na.rm = TRUE) +
  scale_fill_viridis_c(name="Million tonnes", option = "F") +
  theme_void() +
  labs(title = "a. Annual wheat exports (means of 2018 & 2019)")+ 
  theme(
    legend.position = c(0.05,0.025), legend.justification = c(0.05,0.025),
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.title = element_text(color = "white", size = 22, face = "bold", hjust=0.05, margin = margin(0,0,10,0)),
    legend.title = element_text(size = 18, color = "white", margin = margin(0,0,10,0)),
    legend.text = element_text(size = 14, color = "white", margin = margin(0,0,10,0)),
    legend.key.size = unit(1, "cm"),
    plot.margin = margin(10, 0, 5, 0))

pexp


data <-read.csv("wheat trade matrix.csv")

exp <- read.csv("exporters.csv")
imp <- read.csv("importers.csv")

data <- data[data$Year > 2017,]
data <- data[data$Year < 2020,]
data <- data[ which(data$Element == "Export Quantity"), ]

data <- data[,c(4,6,12,14)]
names(data) <- c("Exporters", "Importers", "Year", "Value")

data <- data[,c(1:2,4)] %>%
  group_by(Exporters, Importers) %>% 
  summarise_each(funs(sum))

# exp <- data[,c(1)]
# exp <- unique(exp)
# write.csv(exp, "exporters.csv")
# imp <- data[,c(2)]
# imp <- unique(imp)
# write.csv(imp, "importers.csv")

data <- merge(data, exp, by = "Exporters")
data <- merge(data, imp, by = "Importers")

datimp <- data[,c(1,3)]
datimp <- datimp %>%
  group_by(Importers) %>% 
  summarise_each(funs(sum))


datexp <- data[,c(2,3)]
datexp <- datexp %>%
  group_by(Exporters) %>% 
  summarise_each(funs(sum))


data$Importers2 <- data$Regionimp

data$Importers2 <- ifelse(data$Importers == "Egypt", "Egypt", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Indonesia", "Indonesia", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Turkey", "Turkey", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Algeria", "Algeria", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Italy", "Italy", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Philippines", "Philippines", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Brazil", "Brazil", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Japan", "Japan", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Spain", "Spain", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Mexico", "Mexico", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Netherlands", "Netherlands", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Nigeria", "Nigeria", data$Importers2)
data$Importers2 <- ifelse(data$Importers == "Bangladesh", "Bangladesh", data$Importers2)
# data$Importers2 <- ifelse(data$Importers == "Morocco", "Morocco", data$Importers2)
# data$Importers2 <- ifelse(data$Importers == "Republic of Korea", "Republic of Korea", data$Importers2)
# data$Importers2 <- ifelse(data$Importers == "Germany", "Germany", data$Importers2)
# data$Importers2 <- ifelse(data$Importers == "Viet Nam", "Viet Nam", data$Importers2)
# data$Importers2 <- ifelse(data$Importers == "Belgium", "Belgium", data$Importers2)
# data$Importers2 <- ifelse(data$Importers == "China, mainland", "Mainland China", data$Importers2)
# data$Importers2 <- ifelse(data$Importers == "Saudi Arabia", "Saudi Arabia", data$Importers2)
# data$Importers2 <- ifelse(data$Importers == "Thailand", "Thailand", data$Importers2)
# data$Importers2 <- ifelse(data$Importers == "Yemen", "Yemen", data$Importers2)


data$Importers2 <- ifelse(data$Importers2 == "Asia", "Asia (1)", data$Importers2)
data$Importers2 <- ifelse(data$Importers2 == "Africa", "Africa (2)", data$Importers2)
data$Importers2 <- ifelse(data$Importers2 == "Americas", "Americas (4)", data$Importers2)
data$Importers2 <- ifelse(data$Importers2 == "Europe", "Europe (3)", data$Importers2)
data$Importers2 <- ifelse(data$Importers2 == "Oceania", "", data$Importers2)


data$Exporters2 <- rep("Other", nrow(data))

data$Exporters2 <- ifelse(data$Exporters == "Russian Federation", "Russia", data$Exporters2)
data$Exporters2 <- ifelse(data$Exporters == "United States of America", "USA", data$Exporters2)
data$Exporters2 <- ifelse(data$Exporters == "Canada", "Canada", data$Exporters2)
data$Exporters2 <- ifelse(data$Exporters == "France", "France", data$Exporters2)
data$Exporters2 <- ifelse(data$Exporters == "Ukraine", "Ukraine", data$Exporters2)
data$Exporters2 <- ifelse(data$Exporters == "Argentina", "Argentina", data$Exporters2)
data$Exporters2 <- ifelse(data$Exporters == "Australia", "Australia", data$Exporters2)
# data$Exporters2 <- ifelse(data$Exporters == "Romania", "Romania", data$Exporters2)
# data$Exporters2 <- ifelse(data$Exporters == "Kazakhstan", "Kazakhstan", data$Exporters2)
# data$Exporters2 <- ifelse(data$Exporters == "Germany", "Germany", data$Exporters2)



dat <- data[,c(3,6:7)] %>%
  group_by(Exporters2, Importers2) %>% 
  summarise_each(funs(sum))


# ALLUVIAL DIAGRAMS-------------------------------------------------------------

theme_set(theme_bw(base_size = 18))

ptrad <- ggplot(dat, aes(y = Value, axis1 = Exporters2, axis2 = Importers2)) + 
  theme_void() + ggtitle("c. Wheat trade (sum of 2018 & 2019)") +
  geom_alluvium(aes(fill = Exporters2, weight = Value), decreasing = TRUE, alpha = 0.8
                , 
                width = 0.5
                ) + 
  geom_stratum(decreasing = TRUE, fill = "white", alpha = 0, size = 0.8, colour = "white"
               , 
             width = 0.5
   ) +
  ggrepel::geom_text_repel(stat = "stratum", direction = "y", decreasing = TRUE,
                           label = c("","","","","","","","","","","","","","","","","","","","","","","","","","Oceania"),
                           size = 7, fontface = "bold", segment.color = 'white', colour = "white",
                           nudge_x = rep(c(0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1)),
                           hjust = rep(c(0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1))) +
  geom_text(stat = "stratum", fontface = "bold", aes(label = after_stat(stratum)), decreasing = TRUE, 
            size = 8,
   #         size = c(7, 7, 7, 7, 7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
  #          size = c(8, 8, 8, 8, 8, 8, 8, 8, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4),
   color = "white") + 
  scale_x_continuous(breaks = 1:2, labels = c('Exporters', 'Importers'), limits = c(0.70,2.48)) +
  scale_y_continuous(breaks = NULL, labels = NULL) +
  theme(plot.title = element_text(color = "white", hjust = 0.15, size=22, face="bold"),
        axis.text = element_text(color = "white", size=18, face="bold"),
        plot.background = element_rect(fill = "grey10"),
        legend.position = "none",
        plot.margin = margin(10, 10, 0, 0)
  ) +
  scale_fill_manual(values = c("#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#F69C73FF","#FAEBDDFF","#4C1D4BFF"))
# ggthemes::scale_fill_tableau(name = NULL)
# scale_fill_jcolors(palette = "pal8")

ptrad

# jpeg(file = "Wheat trades - longer.jpeg", units="cm", width = 30, height = 30, res = 320)
# plot1
# dev.off()

# library(scales)
# show_col(viridis_pal(option = "F")(6))

# expimp <- ggarrange(pexp, pimp,  
#                        ncol=1, nrow=2, widths=c(1), heights=c(1,1))


# (pexp/pimp)|(ptrad)
#  plot_layout(widths = c(1,0.5), heights = c(1,1))

# ggsave("PANEL.jpeg", units="cm", width = 20, height = 20, dpi = 320)
  
layout <- "
AAAAAAABBBBB
AAAAAAABBBBB
AAAAAAABBBBB
AAAAAAABBBBB
CCCCCCCBBBBB
CCCCCCCBBBBB
CCCCCCCBBBBB
CCCCCCCBBBBB
"

FIGURE <- pexp + ptrad + pimp + 
  plot_layout(design = layout) + 
  theme(plot.background = element_rect(fill = "grey10")
        ,
                       plot.margin = margin(20, 20, 20, 20)
  )

ggdraw(FIGURE) + 
  theme(plot.background = element_rect(fill = "grey10")
        ,
                       plot.margin = margin(10, 10, 10, 10)
        )

ggsave("PANEL FINAL - 28 May 2022.jpeg", units="cm", width = 60, height = 35, dpi = 320)







