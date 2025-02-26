#########################################################
#HW 5, changing lab 3 code to tidyverse
########################################################

library(stringr)
library(jsonlite)
library(tidyverse)

current.filename <- "The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json" #create our file name

file.name.parts<- str_split(current.filename, "-", simplify=TRUE) #splits our name when - occurs

artist<-str_sub(file.name.parts[1])
album<-file.name.parts[2]
song.name<-str_sub(file.name.parts[3], end=-6L) #these 3 lines creates objects for artist, album and song name

folder <- paste("EssentiaOutput/", current.filename, sep ="") #file path


song.data <- fromJSON(folder) #load song data from JSON


#extracting our relevant song data from .JSON and creating objects for them 
overall.loudness <- song.data$lowlevel$loudness_ebu128$integrated

spectral.energy <- song.data$lowlevel$spectral_energy

dissonance <- song.data$lowlevel$dissonance$mean

pitch.salience <- song.data$lowlevel$pitch_salience

bpm <- song.data$rhythm$bpm

beat.loudness <- song.data$rhythm$beats_loudness

danceability <- song.data$rhythm$danceability

tuning.frequency <- song.data$tonal$tuning_frequency


json.files <- list.files("EssentiaOutput/", pattern = "\\.json$", full.names = TRUE) #checking the list of all of the JSON files in the folder


# Create an empty data frame with correct column names
#switched our data frame to a tibble 
song_data <- tibble(
  artist = character(),
  album = character(),
  song_name = character(),
  overall_loudness = numeric(),
  spectral_energy = numeric(),
  dissonance = numeric(),
  pitch_salience = numeric(),
  bpm = numeric(),
  beat_loudness = numeric(),
  danceability = numeric(),
  tuning_frequency = numeric(),
)

#############################################################################################################################################################
#for loop for creating our data frame of the json files and extracting the column information
#############################################################################################################################################################
for(file in json.files){
  current.file.name <- basename(file)
  file.name.parts<- str_split(current.file.name, "-", simplify=TRUE)
  
  artist<-str_trim(file.name.parts[1])
  album<-str_trim(file.name.parts[2])
  song_name<-str_sub(str_trim(file.name.parts[3]), end=-6L)
  
  song.data <- fromJSON(file)
  
  overall_loudness <- song.data$lowlevel$loudness_ebu128$integrated  
  spectral_energy <- song.data$lowlevel$spectral_energy$mean
  dissonance <- song.data$lowlevel$dissonance$mean
  pitch_salience <- song.data$lowlevel$pitch_salience$mean
  bpm <- song.data$rhythm$bpm
  beat_loudness <- song.data$rhythm$beats_loudness$mean
  danceability <- song.data$rhythm$danceability
  tuning_frequency <- song.data$tonal$tuning_frequency
  
  song_data<-song_data |>
    bind_rows(tibble(artist, album, song_name, overall_loudness, spectral_energy,
    dissonance, pitch_salience, bpm, beat_loudness, danceability, tuning_frequency,
    
  )) #we replaced rbind with bind_rows and then replaced data frame with tibble and are using pipes instead 
}

song_data <- song_data |>
  rename(track = song_name)#rename song name column to track so that it matches with our other tibbles

#############################################################################################################################################################
#cleaning and editing essentia csv file
#############################################################################################################################################################

essentia.df <- read_csv("EssentiaOutput/EssentiaModelOutput.csv") #this reads our csv file and now create the object for it

essentia.df.cleaned <- essentia.df |>
  mutate(average.valence = rowMeans(across(c(deam_valence, emo_valence, muse_valence)), na.rm=T),
         average.arousal = rowMeans(across(c(deam_arousal, emo_arousal, emo_arousal)), na.rm=T),
         average.aggressive = rowMeans(across(c(nn_aggressive, eff_aggressive)), na.rm=T),
         average.happy = rowMeans(across(c(nn_happy, eff_happy)), na.rm=T),
         average.party = rowMeans(across(c(nn_party, eff_party)), na.rm=T) ,
         average.relax = rowMeans(across(c(nn_relax, eff_relax)), na.rm=T) ,
         average.sad = rowMeans(across(c(nn_sad, eff_sad)), na.rm=T) ,
         average.acoustic = rowMeans(across(c(nn_acoustic, eff_acoustic)), na.rm=T) ,
         average.electronic = rowMeans(across(c(nn_electronic, eff_electronic)), na.rm=T), 
         average.instrumental = rowMeans(across(c(nn_instrumental, eff_instrumental)), na.rm=T)) |>
           rename(timbreBright = eff_timbre_bright)|>
           select("artist", "album", "track", "average.valence", "average.arousal",
                    "average.aggressive", "average.happy", "average.party", "average.relax",
                    "average.sad", "average.acoustic" , "average.electronic", "average.instrumental",
                    "timbreBright")

#here we take the average across these rows and create new columns with the label average, we also rename eff_timbre_bright and then we only select the features that we just created

#############################################################################################################################################################
#cleaning and editing the LIWC csv file and merging the 3 data frames together
#############################################################################################################################################################


LIWC.df <- read_csv("LIWCOutput/LIWCOutput.csv") #this reads our csv file and now create the object for it

common.cols <- colnames(song_data)[1:3] #we create an object that we can reference in our merging data to ensure the first 3 columns are the same and no extra rows are created

#merge all 3 data sets by the first 3 columns
data.merged <- song_data |>
  merge(essentia.df.cleaned, by = common.cols) |>
  merge(LIWC.df, by = common.cols)

#write csv including everything except for allentown
training.data <- data.merged |>
  filter(track != "Allentown")|>
  write_csv("trainingdata.csv")

#write csv only including allentown 
testing.data <- data.merged |>
  filter(track == "Allentown")|>
  write_csv("testingdata.csv")


#############################################################################################################################################################
#plotting the data
#############################################################################################################################################################


library(ggplot2)


instrumental_plot <- ggplot(training.data, aes(x = artist, y = average.instrumental)) +
  geom_boxplot()

testing.data$average.instrumental

loudness_plot <- ggplot(training.data, aes(x = artist, y = overall.loudness)) +
  geom_boxplot()

testing.data$overall_loudness

ggsave("instrumental_plot.pdf", plot = instrumental_plot, width = 7, height = 5)

ggsave("loudness_plot.pdf", plot = loudness_plot, width = 7, height = 5)
