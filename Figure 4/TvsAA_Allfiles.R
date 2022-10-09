setwd("D:/Gastro_review/Fig_BA profile across individuals/All_files_TvsAA")

##Female feces##
df_merge_norm<- read.csv("Fe_feces.csv", sep =",", quote = "\"") #manually replaces NA with 0 and added the column of PlotOrder
library(data.table)
setDT(df_merge_norm)
mergedatalong<-melt(df_merge_norm, id.vars=c("PlotOrder"), varibale.name = "BA", value.name = "TIC") #changing the data from wide to long table form required for stacked bar chart
library(tibble)
mergedtalong_tibble<-as_tibble(mergedatalong) #converting to tibble data type required for ggplot2
library(ggplot2)
png("Fe_feces_TvsAA.png")
ggplot() + geom_bar(aes(y= TIC, x = PlotOrder, fill = variable), data = mergedatalong, stat = "identity", width = 1) + scale_fill_manual(values = c("#655D8A","#7897AB")) + theme_classic() + scale_y_continuous(expand=c(0, 0)) + scale_x_continuous(expand=c(0, 0)) + labs(y="Normalized ion intensities", x="Female fecal samples") + theme(axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"))
dev.off()
svg("Fe_feces_TvsAA.svg")
ggplot() + geom_bar(aes(y= TIC, x = PlotOrder, fill = variable), data = mergedatalong, stat = "identity", width = 1) + scale_fill_manual(values = c("#655D8A","#7897AB")) + theme_classic() + scale_y_continuous(expand=c(0, 0)) + scale_x_continuous(expand=c(0, 0)) + labs(y="Normalized ion intensities", x="Female fecal samples") + theme(axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"))
dev.off()


##Male feces##
df_merge_norm<- read.csv("Ma_feces.csv", sep =",", quote = "\"") #manually replaces NA with 0 and added the column of PlotOrder
library(data.table)
setDT(df_merge_norm)
mergedatalong<-melt(df_merge_norm, id.vars=c("PlotOrder"), varibale.name = "BA", value.name = "TIC") #changing the data from wide to long table form required for stacked bar chart
library(tibble)
mergedtalong_tibble<-as_tibble(mergedatalong) #converting to tibble data type required for ggplot2
library(ggplot2)
png("Ma_feces_TvsAA.png")
ggplot() + geom_bar(aes(y= TIC, x = PlotOrder, fill = variable), data = mergedatalong, stat = "identity", width = 1) + scale_fill_manual(values = c("#655D8A","#7897AB")) + theme_classic() + scale_y_continuous(expand=c(0, 0)) + scale_x_continuous(expand=c(0, 0)) + labs(y="Normalized ion intensities", x="Male fecal samples") + theme(axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"))
dev.off()
svg("Ma_feces_TvsAA.svg")
ggplot() + geom_bar(aes(y= TIC, x = PlotOrder, fill = variable), data = mergedatalong, stat = "identity", width = 1) + scale_fill_manual(values = c("#655D8A","#7897AB")) + theme_classic() + scale_y_continuous(expand=c(0, 0)) + scale_x_continuous(expand=c(0, 0)) + labs(y="Normalized ion intensities", x="Male fecal samples") + theme(axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"))
dev.off()
