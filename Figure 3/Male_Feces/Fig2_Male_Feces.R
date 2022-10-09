###reading the files of male fecal samples obtained from the GNPS classical molecular network###
df_hits<- read.csv("Libhit_ma_feces.csv", sep =",", quote = "\"")
df_files<- read.csv("Ma_feces.csv", sep =",", quote = "\"")

###merging the files with bile acids using scan number###
df_merged<-merge(x=df_hits,y=df_files,by="Scan",all.x=TRUE)
df_merged_fil<-df_merged[-c(1,2,4:47)]
write.table(df_merged_fil,"IM_ReDU_male_fecal_filtered.tsv", sep="\t", row.names=FALSE)
unique(df_merged_fil$Compound_Name)

###reading the file with the different group classification of bile acids required for Figure 3###
df_BA_grp<-read.csv("BA_groups.csv", sep =",", quote = "\"")
df_merged_grp<-merge(x=df_BA_grp,y=df_merged_fil,by="Compound_Name",all.x=TRUE)
df_merged_grp<-df_merged_grp[-c(1)]
df_merged_grp[is.na(df_merged_grp)] = 0

###grouping the same bile acids with different names in the GNPS library using the sum function###
####For eg, cholic acid is present as both cholic acid and cholate####
library(dplyr)
df_merged_grp_sum<-df_merged_grp %>% group_by(Group)  %>%
  summarise(across(everything(), list(sum)))


df_merged_grp_sum<-t(df_merged_grp_sum)

write.table(df_merged_grp_sum,"IM_ReDU_male_fecal_grp.csv", sep=",", row.names=TRUE)

###manually replaces NA with 0 and added the column of PlotOrder to ensure the representative distribution of increasing unconjugated bile acids###
df_merge_norm<- read.csv("Ma_feces_norm.csv", sep =",", quote = "\"") 
library(data.table)
setDT(df_merge_norm)
mergedatalong<-melt(df_merge_norm, id.vars=c("PlotOrder"), varibale.name = "BA", value.name = "TIC") #changing the data from wide to long table form required for stacked bar chart
library(tibble)
mergedtalong_tibble<-as_tibble(mergedatalong) #converting to tibble data type required for ggplot2
library(ggplot2)

###exporting the figure as png and svg for further use in adobe illustrator###
png("Fig2_male_feces.png")
ggplot() + geom_bar(aes(y= TIC, x = PlotOrder, fill = variable), data = mergedatalong, stat = "identity", width = 1) + scale_fill_manual(values = c("#D885A3", "#2983B1", "#655D8A", "#7897AB",  "#FEC260",  "#42855B", "#A62349")) + theme_classic() + scale_y_continuous(expand=c(0, 0)) + scale_x_continuous(expand=c(0, 0)) + labs(y="Normalized ion intensities", x="Male fecal samples") + theme(axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"))
dev.off()
svg("Fig2_male_feces.svg")
ggplot() + geom_bar(aes(y= TIC, x = PlotOrder, fill = variable), data = mergedatalong, stat = "identity", width = 1) + scale_fill_manual(values = c("#D885A3", "#2983B1", "#655D8A", "#7897AB",  "#FEC260",  "#42855B", "#A62349")) + theme_classic() + scale_y_continuous(expand=c(0, 0)) + scale_x_continuous(expand=c(0, 0)) + labs(y="Normalized ion intensities", x="Male fecal samples") + theme(axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"))
dev.off()



























