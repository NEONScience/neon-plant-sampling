### Create plots of RD vs. Dlim for various F factors for training materials

### Examples:
##  Plotting multiple data series with ggplot, method 1
df <- data.frame(x, y1, y2)
ggplot(df, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = y1, col = "y1")) + 
  geom_point(aes(y = y2, col = "y2"))

##  Plotting multiple data series, method 2 (works best when there are lots of series to plot)
library(reshape)
# This creates a new data frame with columns x, variable and value
# x is the id, variable holds each of our timeseries designation
df.melted <- melt(df, id = "x")
ggplot(data = df.melted, aes(x = x, y = value, color = variable)) +
  geom_point()


### Create plots for each F value
library(dplyr)
library(ggplot2)

# Read in data from cdw appendix
setwd("~/Documents/neonScienceDocs/gitRepositories/neonPlantSampling/cdw_protocolDev")
df <- read.csv("cdw_lidsDLim_v2.csv", header=T, stringsAsFactors = F)


##  Create plot for F=20
# Only want to plot RD values ≤ 125 cm, and plot Dlim values ≤ 300 meters
df %>%
  filter(RD_cm <= 100) %>%
  ggplot(aes(x=RD_cm, y=F.20)) +
  coord_cartesian(xlim=c(0,100), ylim=c(0,230)) +
  scale_x_continuous(breaks=seq(from=0, to=100, by=5)) +
  scale_y_continuous(breaks=seq(from=0, to=220, by=10)) +
  geom_line(color="black") +
  geom_point(color="blue", size=3, shape=19) +
  labs(title="CDW particle selection: F=20", x="Round Diameter equivalent (cm)", y="Dlim (meters)") +
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12)) -> g1
  
ggsave("cdw_f20SelectionGraph.pdf", g1, width=8, height=6, units="in")
  

##  Create plot for F=15
df %>%
  filter(F.15 <= 225) %>%
  ggplot(aes(x=RD_cm, y=F.15)) +
  coord_cartesian(xlim=c(0,100), ylim=c(0,230)) +
  scale_x_continuous(breaks=seq(from=0, to=100, by=5)) +
  scale_y_continuous(breaks=seq(from=0, to=220, by=10)) +
  geom_line(color="black") +
  geom_point(color="dark green", size=3, shape=19) +
  labs(title="CDW particle selection: F=15", x="Round Diameter equivalent (cm)", y="Dlim (meters)") +
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12)) -> g2

ggsave("cdw_f15SelectionGraph.pdf", g2, width=8, height=6, units="in")


##  Create plot for F=10
df %>%
  filter(F.10 <= 225) %>%
  ggplot(aes(x=RD_cm, y=F.10)) +
  coord_cartesian(xlim=c(0,80), ylim=c(0,230)) +
  scale_x_continuous(breaks=seq(from=0, to=80, by=5)) +
  scale_y_continuous(breaks=seq(from=0, to=220, by=10)) +
  geom_line(color="black") +
  geom_point(color="dark red", size=3, shape=19) +
  labs(title="CDW particle selection: F=10", x="Round Diameter equivalent (cm)", y="Dlim (meters)") +
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12)) -> g3

ggsave("cdw_f10SelectionGraph.pdf", g3, width=8, height=6, units="in")


##  Create plot for F=8
df %>%
  filter(F.8 <= 225) %>%
  ggplot(aes(x=RD_cm, y=F.8)) +
  coord_cartesian(xlim=c(0,70), ylim=c(0,230)) +
  scale_x_continuous(breaks=seq(from=0, to=70, by=5)) +
  scale_y_continuous(breaks=seq(from=0, to=220, by=10)) +
  geom_line(color="black") +
  geom_point(color="dark orange", size=3, shape=19) +
  labs(title="CDW particle selection: F=8", x="Round Diameter equivalent (cm)", y="Dlim (meters)") +
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12)) -> g4

ggsave("cdw_f8SelectionGraph.pdf", g4, width=8, height=6, units="in")


##  Create plot for F=5
df %>%
  filter(F.5 <= 225) %>%
  ggplot(aes(x=RD_cm, y=F.5)) +
  coord_cartesian(xlim=c(0,60), ylim=c(0,230)) +
  scale_x_continuous(breaks=seq(from=0, to=60, by=5)) +
  scale_y_continuous(breaks=seq(from=0, to=220, by=10)) +
  geom_line(color="black") +
  geom_point(color="purple", size=3, shape=19) +
  labs(title="CDW particle selection: F=5", x="Round Diameter equivalent (cm)", y="Dlim (meters)") +
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12)) -> g5

ggsave("cdw_f5SelectionGraph.pdf", g5, width=8, height=6, units="in")
