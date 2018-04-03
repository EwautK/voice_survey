library(data.table)
library(magrittr)

source('functions.R',local=T)
dt_subject = fread('subject_info.csv')

# SINGLE GROUP ------------------------------------------------------------

# Build group limits per survey
dt_group = data.table(phenotype=c('control','patient'),subjects_per_survey=c(3,7))

# Build 100 surveys
dt_survey = build_survey(dt_subject, dt_group,100)

# Add subject info
dt_survey = dt_survey[dt_subject,on=.(subject_id),nomatch=0]

# Summary of the surveys by group
dt_survey[,.(num_subject=.N),by=.(phenotype)]
dt_survey[,.(num_subject=.N),by=.(phenotype,sex)]

# Summary of the surveys by subject
smry = dt_survey[,.(num_subject=.N),by=.(subject_id)][order(subject_id)][dt_subject,on=.(subject_id),nomatch=0]
smry

# Density of the subject counts
smry[,.N,by=.(num_subject)][order(num_subject)]


# MULTIPLE GROUPS ---------------------------------------------------------

# Build group limits per survey
dt_group = CJ(phenotype=c('control','patient'),sex=c('male','female'))
dt_group$subjects_per_survey=c(2,1,5,2)

# Build 10 surveys
dt_survey = build_survey(dt_subject, dt_group,10)

# Add subject info
dt_survey = dt_survey[dt_subject,on=.(subject_id),nomatch=0]

# Summary of the surveys by group
dt_survey[,.(num_subject=.N),by=.(phenotype)]
dt_survey[,.(num_subject=.N),by=.(phenotype,sex)]

# Summary of the surveys by subject
smry = dt_survey[,.(num_subject=.N),by=.(subject_id)][order(subject_id)][dt_subject,on=.(subject_id),nomatch=0]
smry

# Density of the subject counts
smry[,.N,by=.(num_subject)][order(num_subject)]


# ERROR EXAMPLE -----------------------------------------------------------
# Build group limits per survey
dt_group = CJ(phenotype=c('control','patient'),sex=c('male','female'))
dt_group$subjects_per_survey=c(15,1,5,20)

# Build 10 surveys
dt_survey = build_survey(dt_subject, dt_group,10)