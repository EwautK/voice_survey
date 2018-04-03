library(data.table)
library(magrittr)

source('functions.R',local=T)
dt_subject = fread('subject_info.csv')

# Build 100 surveys
dt_survey = build_survey(dt_subject, dt_group,100)

# RANDOMIZE PER SURVEY ----------------------------------------------------
tmp = dt_survey[survey_id==23]
# tmp[dt_subject,on=.(subject_id),nomatch=0]

file_type = c('video','audio')
minimum_question_lag = 2

tapply(file_type,function(x) 0)
id_list=c()
for(i in 1:length(tmp$subject_id)){
  
  new_id = sample(tmp$subject_id,1)
  
  
}

