## Merge FinFinder output and adjust for continuous time analysis in Raven
## Leah Crowe August 2023

library(dplyr)
library(lubridate)

# deployment and instrument
  
#example: "FF03_01"
  deployment<-"Anchor01_03"
#example: "6706"  
  ST_ID<-"6705"
#drive
  drive = "X"
# "Delphinid whistles" or "HBWs"
  type = "Delphinid whistles"
  #type = "HBWs"
  
# list of file paths ----
  
 path <- paste0(drive,":/",deployment,"_",ST_ID)

 #get full path
 wav_list_ff <- list.files(path, recursive = F, pattern = "*.wav", full.names = T)
 #grab time of file start from filename, need to adjust substr argument for different deployments
 # Dagg = substr(Begin.Path, 24, 40)
 # Anchor = substr(Begin.Path, 26, 42)
 file_time_start<-data.frame(Begin.Path = as.character(wav_list_ff))%>%mutate(begin_time = ymd_hms(paste0("20",substr(Begin.Path, 26, 42))))
 write.csv(file_time_start, paste0("C:/Users/jenni/Desktop/Leah_file_list/",deployment,"_",ST_ID,".csv"), row.names = F)
 #get trunc path to link to FinFinder output
 wav_list <- list.files(path, recursive = F, pattern = "*.wav", full.names = F)
 
 wav_df<-as.data.frame(wav_list)%>%
   mutate(`Begin Path` = wav_list_ff)
 
  
# merge all FinFinder output txts ----
  
 selection_table_path<-paste0(path,"/FinFinder_output/",type)
 
 selection_tables_list <- list.files(paste0(selection_table_path,"/Raven_Pro_Selection_Tables"), recursive = T, pattern = "*.txt", full.names = T)
 
 read_selection_tables<-lapply(selection_tables_list, function(x) read.delim(x, header = T))
 
 selection_tables_merge <- do.call(rbind, read_selection_tables)

# toss Begin.File and add in Begin Path for use in Raven ----

 if (type == "Delphinid whistles"){
   
   merge<-selection_tables_merge%>%
     left_join(wav_df, by = c(Begin.File = "wav_list"))%>%
     dplyr::select(Selection, View, Channel, `Begin Path`, File.Offset..s., Begin.Time..s., End.Time..s., Low.Freq..Hz., High.Freq..Hz., Detection.Type, Probability.Score....)%>%
     dplyr::rename(`File Offset (s)` = File.Offset..s., `Begin Time (s)` = Begin.Time..s., `End Time (s)` = End.Time..s., `Low Freq (Hz)` = Low.Freq..Hz., `High Freq (Hz)` = High.Freq..Hz., `Detection Type` = Detection.Type, `Probability Score (%)` = Probability.Score....)%>%
     mutate(Selection = 1:n())

 
 } else if (type == "HBWs"){
   
   merge<-selection_tables_merge%>%
     left_join(wav_df, by = c(Begin.File = "wav_list"))%>%
     dplyr::select(Selection, View, Channel, `Begin Path`, File.Offset..s., Begin.Time..s., End.Time..s., Low.Freq..Hz., High.Freq..Hz., Trigger.Probability.Score...., End.Classification, Classification.Probability.Score...., Spectrogram.File)%>%
     dplyr::rename(`File Offset (s)` = File.Offset..s., `Begin Time (s)` = Begin.Time..s., `End Time (s)` = End.Time..s., `Low Freq (Hz)` = Low.Freq..Hz., `High Freq (Hz)` = High.Freq..Hz., `Trigger Probability Score (%)` = Trigger.Probability.Score...., `End Classification` = End.Classification, `Classification Probability Score (%)` = Classification.Probability.Score...., `Spectrogram File` = Spectrogram.File)%>%
     mutate(Selection = 1:n(),
            `File Offset (s)` = `Begin Time (s)`)

 }
 
 write.table(merge, paste0(selection_table_path,"/",deployment,"_",ST_ID,"-",type,"-Raven_selection_tables-merge_",Sys.Date(),".txt"), sep = "\t", dec = ".", quote = F,
             row.names = F, col.names = TRUE) 
 