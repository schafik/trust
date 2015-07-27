#--------------------
# Split DEEP risk and time data into files
#-------------------


test_that("we have processed DEEP data", {
  expect_that(nrow(read.csv("expiremental_data/data/DEEP/deeptime.csv")), equals(124), #TODO: update! 
              info = print("if you get an error be sure to process the DEEP data by visiting here: https://sites.google.com/a/decisionsciences.columbia.edu/pamlab/ReferenceLibrary/deep/deep-r-code"))
  }) 
  
if ((nrow(read.csv("expiremental_data/data/DEEP/deeptime.csv")) == 120)) { #TODO: update! 
# Instructions: Move all csv files into the working directory and change the. 
# within that working directory create a risk folder and a time folder.
# Download your data and Name the file: deeptime.csv 

#----Time Data----
# read in good serials
time_serials <- read.csv("expiremental_data/data/cleaned_data.csv") %>%
                  dplyr::select(serial)
# read in raw time
time <- read.csv("expiremental_data/data/DEEP/deeptime.csv", header = TRUE) #be sure to update if new data comes in!
# make sure serial is all integers
time <- transform(time, serial = as.integer(serial))
# subset time data to only have good serials
time <- subset(time, serial  %in% time_serials$serial)

# write good serials file w/o header
serials_time_good <- data.frame(serial=time$serial)
write.table(serials_time_good, "expiremental_data/data/DEEP/serials_time_1.csv",
            row.names=FALSE, col.names=FALSE, sep=',')

# Get the adaptive questions
adaptive_questions_time_1 <- time[, grepl('adaptive_question_id', names(time))]
write.table(adaptive_questions_time_1, 
            'expiremental_data/data/DEEP/adaptive_questions_time_1.csv',
            row.names=FALSE, col.names=FALSE, sep=',')

# get the choices
choices_time_1 <- time[, grepl('adaptive_response_answer', names(time))]
write.table(choices_time_1, 'expiremental_data/data/deep/choices_time_1.csv', 
            row.names=FALSE, col.names=FALSE, sep=',')

remove(adaptive_questions_time_1, choices_time_1, serials_time_good,
       time, time_serials)

#RUN the time_unbounded_100712.R script!
print("RUN the time_unbounded_100712.R script!")
}

