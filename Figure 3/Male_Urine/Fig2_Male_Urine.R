#############################################################################################################
######Same comments as Fig2_Male_Feces.R file################################################################
#############################################################################################################

df_hits<- read.csv("Libhit_Ma_urine.csv", sep =",", quote = "\"")
df_files<- read.csv("Ma_urine.csv", sep =",", quote = "\"")
df_merged<-merge(x=df_hits,y=df_files,by="Scan",all.x=TRUE)
df_merged_fil<-df_merged[-c(1,2,4:47)]
write.table(df_merged_fil,"IM_ReDU_male_urine_filtered.tsv", sep="\t", row.names=FALSE)

unique(df_merged_fil$Compound_Name)
df_BA_grp<-read.csv("BA_groups.csv", sep =",", quote = "\"")

df_merged_grp<-merge(x=df_BA_grp,y=df_merged_fil,by="Compound_Name",all.x=TRUE)
df_merged_grp<-df_merged_grp[-c(1,3)]
df_merged_grp[is.na(df_merged_grp)] = 0
library(dplyr)
df_merged_grp_sum<-df_merged_grp %>% group_by(Group)  %>%
  summarise(across(everything(), list(sum)))


df_merged_grp_sum<-t(df_merged_grp_sum)

write.table(df_merged_grp_sum,"IM_ReDU_male_urine_grp.csv", sep=",", row.names=TRUE)


df_merge_norm<- read.csv("Ma_urine_norm.csv", sep =",", quote = "\"") #manually replaces NA with 0 and added the column of PlotOrder
library(data.table)
setDT(df_merge_norm)
mergedatalong<-melt(df_merge_norm, id.vars=c("PlotOrder"), varibale.name = "BA", value.name = "TIC") #changing the data from wide to long table form required for stacked bar chart
library(tibble)
mergedtalong_tibble<-as_tibble(mergedatalong) #converting to tibble data type required for ggplot2
library(ggplot2)
library(viridis) #color theme
library(hrbrthemes)
png("Fig2_male_urine.png")
ggplot() + geom_bar(aes(y= TIC, x = PlotOrder, fill = variable), data = mergedatalong, stat = "identity", width = 1) + scale_fill_manual(values = c("#D885A3", "#2983B1", "#655D8A", "#7897AB",  "#FEC260",  "#42855B", "#A62349")) + theme_classic() + scale_y_continuous(expand=c(0, 0)) + scale_x_continuous(expand=c(0, 0)) + labs(y="Normalized ion intensities", x="Male Urine samples") + theme(axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"))
dev.off()
svg("Fig2_male_urine.svg")
ggplot() + geom_bar(aes(y= TIC, x = PlotOrder, fill = variable), data = mergedatalong, stat = "identity", width = 1) + scale_fill_manual(values = c("#D885A3", "#2983B1", "#655D8A", "#7897AB",  "#FEC260",  "#42855B", "#A62349")) + theme_classic() + scale_y_continuous(expand=c(0, 0)) + scale_x_continuous(expand=c(0, 0)) + labs(y="Normalized ion intensities", x="Male Urine samples") + theme(axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"))
dev.off()
