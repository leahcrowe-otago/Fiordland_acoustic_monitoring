## Merge FinFinder output and adjust for continuous time analysis in Raven
## Leah Crowe August 2023

library(dplyr)
library(lubridate)
  
  deployment<-"Anchor01_01"
  ST_ID<-"6708"
  
  # file paths and merge----
  
 path <- paste0("Z:/",deployment,"_",ST_ID)
  
 wav_list <- list.files(path, recursive = F, pattern = "*.wav", full.names = F)
 origin_wav = lubridate::ymd_hms(paste(stringr::str_sub(wav_list[1],6,11), time = stringr::str_sub(wav_list[1],12,17)))
  
 selection_table_path<-paste0(path,"/FinFinder_output/Delphinid whistles")
 
 selection_tables_list <- list.files(paste0(selection_table_path,"/Raven_Pro_Selection_Tables"), recursive = T, pattern = "*.txt", full.names = T)
 
 read_selection_tables<-lapply(selection_tables_list, function(x) read.delim(x, header = T))
 
 # ncol<-lapply(read_selection_tables, function(x) ncol(x))
 # 
 # read_selection_tables[1508]
 
 selection_tables_merge <- do.call(rbind, read_selection_tables)

 st_merge<-selection_tables_merge%>%
   dplyr::select(Selection, View, Channel, Begin.File, File.Offset..s., Begin.Time..s., End.Time..s., Low.Freq..Hz., High.Freq..Hz., Detection.Type, Probability.Score....)%>%
   dplyr::rename(`Begin File` = Begin.File, `File Offset (s)` = File.Offset..s., `Begin Time (s)` = Begin.Time..s., `End Time (s)` = End.Time..s., `Low Freq (Hz)` = Low.Freq..Hz., `High Freq (Hz)` = High.Freq..Hz., `Detection Type` = Detection.Type, `Probability Score (%)` = Probability.Score....)%>%
   mutate(Selection = 1:n())
 
 write.table(st_merge, paste0(selection_table_path,"/Delph-merge_raw_",Sys.Date(),".txt"), sep = "\t", dec = ".", quote = F,
             row.names = F, col.names = TRUE)  
 
 head(selection_tables_merge)

 time_offset_merge<-selection_tables_merge%>%
   #turn filename into datetime
   mutate(datetime = lubridate::ymd_hms(paste(stringr::str_sub(Begin.File,6,11), time = stringr::str_sub(Begin.File,12,17))))%>%
   #get origin datetime from first filename
   mutate(origin = origin_wav)%>%
   #get datetime of detection by adding offset to datetime of file
   mutate(offset = datetime + lubridate::seconds(File.Offset..s.),
          offset_end = datetime + lubridate::seconds(End.Time..s.))%>%
   mutate(origin_offset = as.numeric(offset - origin, unit = "secs"),
          origin_end_offset = as.numeric(offset_end - origin, unit = "secs"))%>%
   mutate(#File.Offset..s. = origin_offset,
          Begin.Time..s. = origin_offset,
          End.Time..s. = origin_end_offset)%>%
   dplyr::select(Selection, View, Channel, Begin.File, File.Offset..s., Begin.Time..s., End.Time..s., Low.Freq..Hz., High.Freq..Hz., Detection.Type, Probability.Score....)%>%
   dplyr::rename(`Begin File` = Begin.File, `File Offset (s)` = File.Offset..s., `Begin Time (s)` = Begin.Time..s., `End Time (s)` = End.Time..s., `Low Freq (Hz)` = Low.Freq..Hz., `High Freq (Hz)` = High.Freq..Hz., `Detection Type` = Detection.Type, `Probability Score (%)` = Probability.Score....)%>%
   mutate(Selection = 1:n())
 
 head(time_offset_merge)
 
 #write.csv(time_offset_merge, paste0(path,"/FinFinder output merge_",Sys.Date(),".csv"), row.names = F) 
 write.table(time_offset_merge, paste0(selection_table_path,"/Delph-merge_",Sys.Date(),".txt"), sep = "\t", dec = ".", quote = F,
             row.names = F, col.names = TRUE)  

 ##
 
 Sys.time()
 lubridate::ymd_hms("2022-02-19 18:40:10") + lubridate::seconds(3250)
 lubridate::ymd_hms("2022-02-20 10:40:10") + lubridate::seconds(3250)
 origin_wav + 11827201
 ##
 
 mutate(origin_offset_1 = origin_diff + File.Offset..s.)%>%
   #get datetime of detection by adding offset to datetime of file
   mutate(offset = datetime + lubridate::seconds(File.Offset..s.),
          offset_end = datetime + lubridate::seconds(End.Time..s.))%>%
   mutate(origin_offset = as.numeric(offset - origin)*60,
          origin_end_offset = as.numeric(offset_end - origin)*60)%>%
   mutate(File.Offset..s. = origin_offset,
          Begin.Time..s. = origin_offset + 4,
          End.Time..s. = origin_end_offset + 4)%>%