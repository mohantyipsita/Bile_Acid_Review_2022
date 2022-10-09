suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

###Reading the information from ReDU###
df_mus <- fread("ReDU_all_identifications.tsv", sep="\t", header=TRUE, stringsAsFactors = FALSE)
metadata <- fread("REDU_all_sampleinformation.tsv", sep="\t", header=TRUE, stringsAsFactors = FALSE)
metadata$filename <- sub("^f.","", metadata$filename)

###Sub-setting files from mus and rattus only###
mouse_files <- subset(metadata, metadata$NCBITaxonomy %in% c("10090|Mus musculus", "10088|Mus", "10105|Mus minutoides" , "10114|Rattus" , "10116|Rattus norvegius")) 

###Creating a list of bile acids from ReDU sample information###
BA_list <-subset(df_mus, tolower(df_mus$Compound_Name) %like% "cholic")
mouse_parsed_df <- BA_list[BA_list$full_CCMS_path %in% mouse_files$filename,]
unique(mouse_parsed_df$Compound_Name)
df_mus_1 <- merge(mouse_parsed_df, metadata, by.x="full_CCMS_path", by.y="filename")
df_mus_1 <- table(df_mus_1[,c("UBERONBodyPartName","Compound_Name")])
df_mus_1 <- as.data.frame(df_mus_1)
df_mus_1 <- as.data.frame.matrix(df_mus_1)

###combining bile acids with different names###
df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Cholic Acid", "CA",
                                ifelse(Compound_Name == "CHOLIC ACID", "CA",
                                       ifelse(Compound_Name == "Spectral Match to Cholic acid from NIST14", "CA", 
                                              ifelse(Compound_Name == "Cholic acid", "CA", 
                                                     ifelse(Compound_Name == "cholic acid", "CA", 
                                                            ifelse(Compound_Name == "MoNA:3501268 Cholic acid", "CA",df_mus_1$Compound_Name)))))))
df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "deoxycholic acid", "DCA",
                                ifelse(Compound_Name == "Deoxycholic Acid", "DCA",
                                       ifelse(Compound_Name == "Spectral Match to Deoxycholic acid from NIST14", "DCA", df_mus_1$Compound_Name))))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "taurocholic acid", "TCA",
                                ifelse(Compound_Name == "Taurocholic Acid", "TCA",
                                       ifelse(Compound_Name == "Spectral Match to Taurocholic acid from NIST14", "TCA", 
                                              ifelse(Compound_Name == "HMDB:HMDB00036-61 Taurocholic acid", "TCA", 
                                                     ifelse(Compound_Name == "Massbank: Taurocholate", "TCA",df_mus_1$Compound_Name))))))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "taurodeoxycholic acid", "TDCA", df_mus_1$Compound_Name))
                                
df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Spectral Match to Tauroursodeoxycholic acid from NIST14", "TUDCA",
                                ifelse(Compound_Name == "tauroursodeoxycholic acid", "TUDCA", df_mus_1$Compound_Name)))
                                      
df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "chenodeoxycholic acid", "CDA",
                                       ifelse(Compound_Name == "Spectral Match to Chenodeoxycholic acid from NIST14", "CDA", df_mus_1$Compound_Name)))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Spectral Match to Glycodeoxycholic acid from NIST14", "GDCA",
                                ifelse(Compound_Name == "Massbank:MT000040 Glycodeoxycholic acid|glycodeoxycholate", "GDCA",
                                       ifelse(Compound_Name == "glycodeoxycholic acid", "GDCA", 
                                              ifelse(Compound_Name == "HMDB:HMDB00631-862 Deoxycholic acid glycine conjugate", "GDCA",
                                                     ifelse(Compound_Name == "Glycodeoxycholic acid", "GDCA", df_mus_1$Compound_Name))))))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Spectral Match to Hyodeoxycholic acid from NIST14", "HDCA",
                                ifelse(Compound_Name == "hyodeoxycholic acid", "HDCA", 
                                       ifelse(Compound_Name == "MoNA:3514523 Hyodeoxycholate", "HDCA",df_mus_1$Compound_Name))))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Glycocholic Acid", "GCA",
                                ifelse(Compound_Name == "glycocholic acid", "GCA",
                                       ifelse(Compound_Name == "MLS001332546-01!Glycocholic acid hydrate475-31-0", "GCA",
                                              ifelse(Compound_Name == "Spectral Match to Glycocholic acid from NIST14", "GCA", 
                                                     ifelse(Compound_Name == "Glycocholic acid", "GCA", 
                                                            ifelse(Compound_Name == "Massbank:KO008975 Glycocholate|3alpha,7alpha,12alpha-Trihydroxy-5beta-cholan-24-oylglycine|Glycocholic acid", "GCA", 
                                                                   ifelse(Compound_Name == "GLYCOCHOLATE", "GCA",df_mus_1$Compound_Name))))))))


df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "ursodeoxycholic acid", "UDCA",
                                ifelse(Compound_Name == "MLS000028461-01!URSODEOXYCHOLIC ACID", "UDCA",
                                       ifelse(Compound_Name == "Spectral Match to Ursodeoxycholic acid from NIST14", "UDCA", df_mus_1$Compound_Name))))
df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "lithocholic acid", "LCA",
                                ifelse(Compound_Name == "Spectral Match to Lithocholic acid from NIST14", "LCA", 
                                       ifelse(Compound_Name == "Lithocholic acid", "LCA",df_mus_1$Compound_Name))))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Spectral Match to Dehydrocholic acid from NIST14", "Dehydro-CA",
                                ifelse(Compound_Name == "DEHYDROCHOLIC ACID", "Dehydro-CA", df_mus_1$Compound_Name)))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "glutamate conjugated cholic acid", "Glu-CA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Ile/Leu Conjugated Muricholic acid", "Ile/Leu-MCA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Phenylalanocholic acid", "Phe-Ala-CA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Leucocholic acid", "Leu-CA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Serine-Cholic Acid", "Ser-CA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Serine-Cholic Acid", "Ser-CA",df_mus_1$Compound_Name))


df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Spectral Match to Glycoursodeoxycholic acid from NIST14", "GUDCA acid",
                                ifelse(Compound_Name == "glycoursodeoxycholic acid", "GUDCA acid",df_mus_1$Compound_Name)))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Tyrosocholic acid", "Tyr-CA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "glutamate conjugated chenodeoxycholic acid", "Glu-CDCA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "phenylalanine conjugated chenodeoxycholic acid", "Phe-CDCA", 
                                ifelse(Compound_Name == "Putative Chenodeoxycholic acid Phenylalanine conjugate", "Phe-CDCA",df_mus_1$Compound_Name)))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Spectral Match to 12-Ketodeoxycholic acid from NIST14", "12-Ketodeoxycholic acid",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "threonine conjugated cholic acid", "Thr-CA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Tyrosine Conjugated Muricholic acid", "Tyr-MCA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Tryptophan Conjugated Muricholic Acid", "Trp-MCA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "taurochenodeoxycholic acid", "TCDCA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "taurohyodeoxycholic acid", "THDCA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "GLYCOCHENODEOXYCHOLIC ACID", "GCDCA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "taurolithocholic acid", "TLCA",
                                ifelse(Compound_Name == "Taurocholic Acid Sulfate", "TLCA-sulphate",df_mus_1$Compound_Name)))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "hyocholic acid", "HCA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "taurohyocholic acid", "THCA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "glycohyocholic acid", "GHCA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Isoleucocholic acid", "Ile-CA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "12-Ketodeoxycholic acid", "12-Keto DCA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Cholic Acid, Methyl Ester", "Methylated-CA",df_mus_1$Compound_Name))

df_mus_1 <- df_mus_1 %>%
  mutate(Compound_Name = ifelse(Compound_Name == "Sulfated glycocholic acid murine", "GMCA-sulphate",df_mus_1$Compound_Name))

###combining blood serum and blood plasma into one group###
df_mus_1 <- df_mus_1 %>%
  mutate(UBERONBodyPartName = ifelse(UBERONBodyPartName == "blood serum", "blood",
                                     ifelse(UBERONBodyPartName == "blood plasma", "blood",df_mus_1$UBERONBodyPartName)))
###normalizing across different body parts###
df_mus_1 <- cbind(rownames(df_mus_1), df_mus_1)
colnames(df_mus_1)[1] <- "UBERONBodyPartName_1"
number_files <- metadata %>% group_by(UBERONBodyPartName) %>% tally()
df_mus_1 <- df_mus_1 %>% left_join(number_files, by="UBERONBodyPartName")
df_mus_1 <- cbind(df_mus_1, df_mus_1$Freq/df_mus_1$n)
df_mus_1<- df_mus_1 %>% rename(ratio = 6)
df_mus_wide <- dcast(df_mus_1[c("UBERONBodyPartName","Compound_Name","ratio")], UBERONBodyPartName~Compound_Name, value.var = "ratio",fun.aggregate = mean)
colnames(df_mus_1)[1] <- "UBERONBodyPartName"

###exporting the file with bile acid counts in each body part###
write.table(df_mus_wide,"IM_ReDU_mouse_BA.csv", sep=",", row.names=FALSE)

