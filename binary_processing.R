############################################################
#                                                          #
#   processing microbeJ binary segmented microscopy data   #
#                                                          #
############################################################

#### import libraries ####

library(tidyverse)
library(rstudioapi)

#### select the directory containing the data to be processed ####

setwd(selectDirectory("select folder with CSV files"))

#### import all .csv to global environment as data frames and name F1, F2, etc ####

temp = list.files(pattern="*.csv")
fov = paste("F",1:length(temp),sep="") 
for (i in 1:length(temp)) assign(fov[i], read.csv(temp[i]))

#### get dataframes from environment and make a list of dataframes ####

F <- Filter(function(x) is(x, "data.frame"), mget(ls()))

#### remove unwanted columns and rename colnames ####

F <- lapply(F, "[",,c("EXPERIMENT","SHAPE.area","INTENSITY.ch1.mean",
                      "INTENSITY.ch1.mean_c","POSITION","SHAPE.area",
                      "SHAPE.length","SHAPE.width.mean"))

F <- lapply(F, setNames, c("Image.name","Cell.ID","Mean.intensity","Corrected.mean.intensity",
                           "Frame","Cell.area","Cell.length","Mean.cell.width"))


#### rename unique cells to sequence of 1, 2, 3 ... etc ####

id_cleaner <- function(df){
  
  old_cell_id <- unique((df$Cell.ID))
  new_cell_id <- 1:length(old_cell_id)
  id_df <- as.data.frame(old_cell_id)
  id_df$new_cell_id <- new_cell_id
  updated_df <- left_join(df, id_df, by = c("Cell.ID" = "old_cell_id"))
  updated_df$Cell.ID <- NULL
  names(updated_df)[names(updated_df) == "new_cell_id"] <- "Cell.ID"
  return(updated_df)
}

F <- lapply(F, id_cleaner)

#### quick check of the data ####

F$F1 %>%
  mutate(Cell.ID = as.factor(Cell.ID)) %>%
  ggplot(aes(Frame,Corrected.mean.intensity)) +
  geom_point(aes(colour=Cell.ID)) 

#### sort columns by ascending cell ID ####

colsorter <- function(df){
  sortedcols <- df %>% arrange(Cell.ID)
  return(sortedcols)
  }

F <- lapply(F, colsorter)

#### covert the list of cleaned dataframes back to individual dataframes  ####

lapply(names(F), function(x) assign(x, F[[x]], envir = .GlobalEnv))

#### export intensity vs time data as csv ####

for (i in 1:length(temp)){
  write.csv(get(fov[i]),paste(fov[i],".csv",sep=""), row.names = FALSE)
}

#### clean up global environment ####

rm(list = ls())

