#' Build surveys by sampling subjects using subject info limits
#'
#' @param subject_info Information about the subjects
#' @param subject_groups Limitations that have to be used per subject group in a survey
#' @param surveys  Number of surveys to create
#'
#' @return Subjects per survey
#' @export
build_survey = function(subject_info, subject_groups, surveys=1){
  
  # Make sure that the sum of the number of subjects per group <= number of subjects
  overshoot = subject_info[,.N,by=c(names(subject_groups)[names(subject_groups)!='subjects_per_survey'])] %>% 
    .[subject_groups,
      subject_overshoot:=subjects_per_survey-N,
      on=names(subject_groups)[names(subject_groups)!='subjects_per_survey']
      ] %>% .[,N:=NULL] %>% 
    .[subject_overshoot>0,
      sprintf("Group '%s' requires %d more subjects",
              paste0(.BY,collapse='-'),
              subject_overshoot
      ),by=c(names(.)[names(.)!='subject_overshoot'])]
  overshoot = overshoot$V1
  if(length(overshoot)>0) stop(paste0(overshoot,collapse='\n  '))
  
  
  subject_info$count=0
  dt_survey =  Reduce(function(init,x){
    
    # 1. GET SUBJECTS FOR NEW SURVEY
    survey_subjects = subject_groups[,{
      
      # 1.1. Get subjects for the current subgroup defined by .BY
      subject_info_subgroup = subject_info[.BY,on=names(.BY)]
      
      ## 1.2. Sort subjects by count and get count range for the number of subjects_per_survey that have to be taken for this group
      count_range = subject_info_subgroup[order(count)][1:.SD$subjects_per_survey,.(min=min(count),max=max(count))]
      
      ## 1.3. If the minimum and maximum count in the count range are different,
      ##      then make sure to get all subjects with count < maximum range count
      out=c()
      if(count_range$max!=count_range$min){
        out = subject_info_subgroup[count<count_range$max,subject_id]
      }
      
      ### 1.4. Return the selected subjects and add
      ###      subjects that have the maximum range count by sampling them as many
      ###      times as needed to fill up the subjects_per_survey
      c(out,{
        sample_subjects = subject_info_subgroup[count==count_range$max,subject_id]
        if(length(sample_subjects)==1)
          sample_subjects
        else
          sample(sample_subjects,(.SD$subjects_per_survey-length(out)))
      })
    },by=c(names(subject_groups)[names(subject_groups)!='subjects_per_survey'])]$V1
    
    # 2. ADD COUNT TO SELECTED SUBJECTS
    subject_info[subject_id%in%survey_subjects,count:=count+1]
    
    # 3. ADD NEW SURVEY TO THE SURVEY TABLE
    init = rbind(
      init,
      subject_info[subject_id%in%survey_subjects,.(survey_id=x,subject_id)]
    )
  },1:surveys,init=data.table())
}