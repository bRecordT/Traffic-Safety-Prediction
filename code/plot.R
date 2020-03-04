#plots based on previous results
rm(list=ls())
gc()
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(reshape2)

#global theme
mytheme <- theme(plot.title = element_text(vjust = 1, hjust = 0.5, size = 40),
                 axis.text = element_text(vjust = 0.5,hjust = 0.5, size = 25),
                 axis.title = element_text(vjust = 1, hjust = 0.5, size = 25),
                 axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                 axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                 plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"inches"),
                 panel.background = element_rect(fill = "white", color = "black", size = 2),
                 panel.grid.major = element_line(color = "grey", size = 0.5),
                 panel.grid.minor = element_line(color = "grey", size = 0.5, linetype = 2),
                 legend.background = element_rect(color = "black", fill = "white", size = 1),
                 legend.key.size = unit(0.2, "inches"),
                 legend.key.height = unit(0.6,"inches"),
                 legend.key.width = unit(1.1,"inches"),
                 legend.text = element_text(size = 25),
                 legend.text.align = 0,
                 legend.title = element_blank(),
                 legend.position = c(0.85, 0.75),
                 legend.direction = "vertical",
                 legend.justification = c(0.4, 0.4),
                 legend.box = "vertical",
                 legend.box.just = "top")
#plot
#cart
data <- read.csv("G:/*****/CART Overall Node Effect.csv", header = T, stringsAsFactors = F)
#terminal node
ggplot() + geom_line(data = data, aes(x = Minimum.Node.Size, y = Terminal.Node), size=1.2, 
                     color = brewer.pal(7, "Set1")[1]) + 
  labs(x = "Minimum Node Size (%)", y = "Terminal Node") + scale_x_continuous(breaks = 0:10) + mytheme

#overall accuracy
subdata <- data[,c(1,3,11)]
subdataplot <- melt(subdata, id = "Minimum.Node.Size")
ggplot() + geom_line(data = subdataplot, aes(x = Minimum.Node.Size, y = value, linetype = variable), 
                     color = "red", size = 1.2) +
  scale_linetype_manual(name = "", values = c(1, 5), labels = c("Training Dataset", "Test Dataset")) +
  labs(x = "Minimum Node Size (%)", y = "Overall Accuracy") + scale_x_continuous(breaks = 0:10) + 
  scale_y_continuous(limits = c(0.57, 0.62)) + mytheme

#sensitivity/precision
subdata <- data[,c(1, 6, 8, 14, 16)]
subdataplot <- melt(subdata, id = "Minimum.Node.Size")
color1 <- c("red", brewer.pal(7, "Set1")[4], "red", brewer.pal(7, "Set1")[4])
linetype1<-c(1, 1, 5, 5)
ggplot() + geom_line(data = subdataplot, aes(x = Minimum.Node.Size, y = value, color = variable, 
                                             linetype = variable), size = 1.2) +
  scale_color_manual(name = "", values = color1,
                     breaks = c("Train.Sensitivity", "Test.Sensitivity", "Train.Precision", "Test.Precision"),
                     labels = c("Training Sensitivity", "Test Sensitivity", "Training Precision", "Test Precision")) +
  scale_linetype_manual(name = "", values = linetype1,
                        breaks = c("Train.Sensitivity", "Test.Sensitivity", "Train.Precision", "Test.Precision"),
                        labels = c("Training Sensitivity", "Test Sensitivity", "Training Precision", "Test Precision")) +
  labs(x = "Minimum Node Size (%)", y = "Sensitivity / Precision") + scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(limits = c(0.35, 0.7)) + mytheme

#F-1
subdata <- data[,c(1, 9, 17)]
subdataplot <- melt(subdata, id = "Minimum.Node.Size")
ggplot() + geom_line(data = subdataplot, aes(x = Minimum.Node.Size, y = value, linetype = variable), 
                     color = "red", size = 1.2) + 
  scale_linetype_manual(name = "Legend", values = c(1, 5), labels = c("Training Dataset", "Test Dataset")) +
  labs(x = "Minimum Node Size (%)", y = "F-1 Score") + scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(limits = c(0.45, 0.6)) + mytheme

#rf
data <- read.csv("G:/*****/RF Error.csv", header = T, stringsAsFactors = F)

subdataplot <- melt(data, id = "Tree")
color1 <- c(brewer.pal(7, "Set1")[1], brewer.pal(7, "Set1")[2], brewer.pal(7, "Set1")[4])
ggplot(data = subdataplot, aes(x = Tree)) + geom_line(aes(y = value, color = variable), size = 1.2) +
  scale_color_manual(name = "Legend", values = color1, labels = c("OOB Sample", "Fatal / Injury Crash", "PDO Crash")) +
  labs(x = "Number of Trees", y = "Error") + scale_y_continuous(limits = c(0.2, 0.6)) + mytheme

#mvar  
data <- read.csv("G:/*****/RF mvar effect.csv", header = T, stringsAsFactors = F)  
#train
subdata <- data[, c(1, 3:7)]  
subdataplot <- melt(subdata, id = "Number.of.Variables")
color2 <- c(brewer.pal(7, "Set1")[1], brewer.pal(7, "Set1")[2], brewer.pal(7, "Set1")[3], brewer.pal(7, "Set1")[4],
            brewer.pal(7, "Set1")[5], brewer.pal(7, "Set1")[7])
ggplot() + geom_line(data = subdataplot, aes(x = Number.of.Variables, y = value, color = variable), size = 1.2) +
  scale_color_manual(name = "", values = color2, labels = c("OOB Error", "Overall Accuracy", "Sensitivity",
                                                            "Precision", "F-1 Score")) +
  labs(x = "Number of Candidate Variables at Each Split", y = "") + scale_y_continuous(limits = c(0.3, 0.75)) +
  mytheme
#test 
subdata <- data[,c(1, 8:11)]  
subdataplot <- melt(subdata, id = "Number.of.Variables")
color3 <- c(brewer.pal(7, "Set1")[2], brewer.pal(7, "Set1")[3], brewer.pal(7, "Set1")[4],
            brewer.pal(7, "Set1")[5], brewer.pal(7, "Set1")[7])
ggplot() + geom_line(data = subdataplot, aes(x = Number.of.Variables, y = value, color = variable), size = 1.2) +
  scale_color_manual(name = "", values = color3, labels = c("Overall Accuracy", "Sensitivity", "Precision", "F-1 Score")) +
  labs(x = "Number of Candidate Variables at Each Split", y = "") + scale_y_continuous(limits = c(0.4, 0.75)) +
  mytheme


