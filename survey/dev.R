library(data.table)
library(magrittr)

dt_subject = fread('subject_info.csv')
dt_subject$count=0

dt_group = data.table(phenotype=c('control','patient'),subjects_per_survey=c(3,7))
# dt_group = CJ(phenotype=c('control','patient'),sex=c('male','female'))
# dt_group$subjects_per_survey=c(2,1,5,2)

# TODO: check if subjects_per_survey is not greater thannumber of subject present in dt_subject

# BUILD SURVEYS -----------------------------------------------------------
surveys = 100

dt_subject$count=0
dt_survey =  Reduce(function(init,x){
  
  # 1. GET SUBJECTS FOR NEW SURVEY
  survey_subjects = dt_group[,{
    
    # 1.1. Get subjects for the current subgroup defined by .BY
    dt_subject_subgroup = dt_subject[.BY,on=names(.BY)]
    
    ## 1.2. Sort subjects by count and get count range for the number of subjects_per_survey that have to be taken for this group
    count_range = dt_subject_subgroup[order(count)][1:.SD$subjects_per_survey,.(min=min(count),max=max(count))]
    
    ## 1.3. If the minimum and maximum count in the count range are different,
    ##      then make sure to get all subjects with count < maximum range count
    out=c()
    if(count_range$max!=count_range$min){
      out = dt_subject_subgroup[count<count_range$max,subject_id]
    }
    
    ### 1.4. Return the selected subjects and add
    ###      subjects that have the maximum range count by sampling them as many
    ###      times as needed to fill up the subjects_per_survey
    c(out,{
      sample_subjects = dt_subject_subgroup[count==count_range$max,subject_id]
      if(length(sample_subjects)==1)
        sample_subjects
      else
        sample(sample_subjects,(.SD$subjects_per_survey-length(out)))
      })
  },by=c(names(dt_group)[names(dt_group)!='subjects_per_survey'])]$V1
  
  # 2. ADD COUNT TO SELECTED SUBJECTS
  dt_subject[subject_id%in%survey_subjects,count:=count+1]
  
  # 3. ADD NEW SURVEY TO THE SURVEY TABLE
  init = rbind(
    init,
    dt_subject[subject_id%in%survey_subjects,.(survey_id=x,subject_id)]
  )
},1:surveys,init=data.table())

# Test
smry = dt_survey[,.(num_subject=.N),by=.(subject_id)][order(subject_id)]
smry[,.N,by=.(num_subject)][order(num_subject)]

# write.csv(dt_survey,'dt_survey.csv',row.names=F)

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

