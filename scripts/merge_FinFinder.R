## Merge FinFinder output and adjust for continuous time analysis in Raven
## Leah Crowe August 2023

library(dplyr)
library(lubridate)

# deployment and instrument
  
#example: "FF03_01"
  deployment<-"Nancy01_01"
#example: "6706"  
  ST_ID<-"5870"
  
# list of file paths ----
  
 path <- paste0("Z:/",deployment,"_",ST_ID)

 #get full path
 wav_list_ff <- list.files(path, recursive = F, pattern = "*.wav", full.names = T)  
 #get trunc path to link to FinFinder output
 wav_list <- list.files(path, recursive = F, pattern = "*.wav", full.names = F)
 
 wav_df<-as.data.frame(wav_list)%>%
   mutate(`Begin Path` = wav_list_ff)
 
  
# merge all FinFinder output txts ----
  
 selection_table_path<-paste0(path,"/FinFinder_output/Delphinid whistles")
 
 selection_tables_list <- list.files(paste0(selection_table_path,"/Raven_Pro_Selection_Tables"), recursive = T, pattern = "*.txt", full.names = T)
 
 read_selection_tables<-lapply(selection_tables_list, function(x) read.delim(x, header = T))
 
 selection_tables_merge <- do.call(rbind, read_selection_tables)

# toss Begin.File and add in Begin Path for use in Raven ----

 delph_merge<-selection_tables_merge%>%
   left_join(wav_df, by = c(Begin.File = "wav_list"))%>%
   dplyr::select(Selection, View, Channel, `Begin Path`, File.Offset..s., Begin.Time..s., End.Time..s., Low.Freq..Hz., High.Freq..Hz., Detection.Type, Probability.Score....)%>%
   dplyr::rename(`File Offset (s)` = File.Offset..s., `Begin Time (s)` = Begin.Time..s., `End Time (s)` = End.Time..s., `Low Freq (Hz)` = Low.Freq..Hz., `High Freq (Hz)` = High.Freq..Hz., `Detection Type` = Detection.Type, `Probability Score (%)` = Probability.Score....)%>%
   mutate(Selection = 1:n())

 write.table(delph_merge, paste0(selection_table_path,"/",deployment,"_",ST_ID,"-","Delphinid_whistles-Raven_selection_tables-merge_",Sys.Date(),".txt"), sep = "\t", dec = ".", quote = F,
             row.names = F, col.names = TRUE)  

 