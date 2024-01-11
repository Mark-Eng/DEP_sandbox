
#———————————————————————————————————————————————————————————————
#Setup of the R packages, library and folder identification ####
#———————————————————————————————————————————————————————————————

# Runnning the install packages module in case there not installed on the server. 
# Note: If it's your first time R might ask you to choose a mirror for download. Choose the location the closest to where you are.
# Note: Once done you can add a "#" in front of the install.packages() command to avoid running it every time.

#install.packages("readxl")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("data.table")
#install.packages("tidyverse")
#install.packages("reshape2")
#install.packages("writexl")

# Creating the library - library are important in r to load the different packages

library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(tidyverse)
library(reshape2)
library(writexl)

# Identifying folder where the dataset are located and where the results will be outsheeted

setwd("C:/Users/ME/3ie/REAPER Evidence synthesis project - Ag EGM/02-technical_documents/05-analysis/05-Analysis for final report/Final analysis/Mark/Test")
# setwd('C:/Users/FionaKastel/iCloudDrive/3ie/REAPER - Ag EGM/R script/')

# Deactivating warnings for smooth run
options(warn=-1)


#—————————————————————————————————————————————————-
# Importing EGM Export sheet for transformation ####
#—————————————————————————————————————————————————-

print("[Part 1/6] EGM Custom fields transformation ongoing...")

# We load the dataset of customfields

# egm=read_excel('custom_fields_data1629848668.xlsx')
egm=read_excel("AgEGM custom fields.xlsx", sheet = "Worksheet")

# We load the variable of intervention in the dataset
# Note: if there is more than 4 intervention arms, you should add it in the line below following the same pattern ",egm$'name_of_your_variable'" and rename it afterwards
# currently 13 arms and worded differently

# egm_data <- data.frame(egm$'Record ID',egm$'Intervention arm 1 (Multi Select)',egm$'Intervention arm 2 (Multi Select)',egm$'Intervention arm 3 (Multi Select)',egm$'Intervention arm 4 (Multi Select)')
egm_data <- data.frame(egm$'Record ID',egm$'Intervention 1 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Intervention 2 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Intervention 3 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Intervention 4 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Intervention 5 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Intervention 6 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Intervention 7 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Intervention 8 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Intervention 9 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Intervention 10 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Intervention 11 (Multi Select - separate values with single vertical bar (|))')

# We rename the variable selected in the dataset

# names(egm_data)[names(egm_data) == 'egm..Record.ID.'] <- 'record_id'
# names(egm_data)[names(egm_data) == 'egm..Intervention.arm.1..Multi.Select..'] <- '1'
# names(egm_data)[names(egm_data) == 'egm..Intervention.arm.2..Multi.Select..'] <- '2'
# names(egm_data)[names(egm_data) == 'egm..Intervention.arm.3..Multi.Select..'] <- '3'
# names(egm_data)[names(egm_data) == 'egm..Intervention.arm.4..Multi.Select..'] <- '4'

# faster way to rename all the columns
colnames(egm_data) <- c('record_id', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11') 


# We work on the intervention data to reshape it in a way according to the template. The line below reshape it from wide to long, 
# remove "NA" and "This field does not exist and is read only" and sort it

egm_data <- melt(setDT(egm_data), id.vars = c("record_id"), variable.name = "Arm")
egm_data <-na.omit(egm_data)
egm_data <- egm_data %>% filter(value != "This field does not exist and is read only")
egm_data <-egm_data[order(egm_data$'record_id'),]

# We identify study with multiple component to create the variable mul_int_desc and identify them as multiple component

egm_data <- egm_data %>% mutate(dummy = str_count(egm_data$value,"\\|"))
egm_data <- egm_data %>% mutate(mul_int_desc = ifelse(dummy>=1,value,""))
egm_data <- egm_data %>% mutate(value = ifelse(dummy>=1,"Multiple Component",value))
egm_data_int = subset(egm_data, select = -c(dummy) )

# We are done with the intervention part

names(egm_data_int)[names(egm_data_int) == 'value'] <- 'Intervention'

# We load the variable of outcomes in the dataset
# Note: if there is more than 4 intervention arms, you should add it in the line below following the same pattern ",egm$'name_of_your_variable'" and rename it afterwards

egm_data <- data.frame(egm$'Record ID',egm$'Outcome 1 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Outcome 2 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Outcome 3 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Outcome 4 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Outcome 5 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Outcome 6 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Outcome 7 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Outcome 8 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Outcome 9 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Outcome 10 (Multi Select - separate values with single vertical bar (|))',
                       egm$'Outcome 11 (Multi Select - separate values with single vertical bar (|))')

# We rename the variable selected in the dataset

# names(egm_data)[names(egm_data) == 'egm..Record.ID.'] <- 'record_id'
# names(egm_data)[names(egm_data) == 'egm..Outcome.arm.1..Multi.Select..'] <- '1'
# names(egm_data)[names(egm_data) == 'egm..Outcome.arm.2..Multi.Select..'] <- '2'
# names(egm_data)[names(egm_data) == 'egm..Outcome.arm.3..Multi.Select..'] <- '3'
# names(egm_data)[names(egm_data) == 'egm..Outcome.arm.4..Multi.Select..'] <- '4'

# faster way to rename all the columns
colnames(egm_data) <- c('record_id', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11') 

# We work on the intervention data to reshape it in a way according to the template. The line below reshape it from wide to long, remove "NA" and sort it

egm_data <- melt(setDT(egm_data), id.vars = c("record_id"), variable.name = "Arm")
egm_data <-na.omit(egm_data)
egm_data <- egm_data %>% filter(value != "This field does not exist and is read only")
egm_data <-egm_data[order(egm_data$"record_id"),]

# We will split the outcome data after each "|". The line below shows that we allow for 20 splits. In case there is more you might need to add numbers in the same pattern - e.g ',"22","23"'

egm_data<-egm_data%>% mutate(colsplit(egm_data$"value","\\|", names = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")))
egm_data = subset(egm_data, select = -c(value) )

# We perform another reshape from wide to long to also sort multiple outcome inside each observation after the parsing of above

egm_data <- melt(setDT(egm_data), id.vars = c("record_id","Arm"), variable.name = "Arm2")
egm_data = subset(egm_data, select = -c(Arm2) )
egm_data[egm_data==''] <- NA
egm_data <-na.omit(egm_data)

# We are done with the outcome part

egm_data_out <-egm_data[order(egm_data$"record_id"),]
names(egm_data_out)[names(egm_data_out) == 'value'] <- 'Outcome'

# We merge the data from outcome and intervention to obtain the prepared dataset of customfieds "egm_data"

egm_data <- merge(egm_data_int,egm_data_out,by=c("record_id","Arm"))
names(egm_data)[names(egm_data) == 'Outcome'] <- 'outcome'
names(egm_data)[names(egm_data) == 'Intervention'] <- 'intervention'
names(egm_data)[names(egm_data) == 'record_id'] <- 'id'

# We are numbering the rows for later merge

egm_data$time <- ave(as.character(egm_data$'id'), egm_data$'id', FUN=seq_along)

# We load the rest of the variable that we need in the custom fields data.
# Note: if you need additional variable from the one listed below, you should add it in the line below following the same pattern ",egm$'name_of_your_variable'" and eventually rename it afterwards

egm_full <- data.frame(egm$'Project ID', egm$'Record type', egm$'Record ID',egm$'Record Title', egm$'Record authors',egm$'Publication year',
                       egm$'URL link',egm$'Short title',egm$'Cross-cutting themes (Multi Select - separate values with single vertical bar (|))',
                       egm$'Sex',egm$'Age (Multi Select - separate values with single vertical bar (|))',
                       egm$'Settings (Multi Select - separate values with single vertical bar (|))')

names(egm_full)[names(egm_full) == 'egm..Record.ID.'] <- 'id'
names(egm_full)[names(egm_full) == 'egm..Project.ID.'] <- 'Project ID'
names(egm_full)[names(egm_full) == 'egm..Record.type.'] <- 'Record type'
names(egm_full)[names(egm_full) == 'egm..Record.Title.'] <- 'Record Title'
names(egm_full)[names(egm_full) == 'egm..Record.authors.'] <- 'Record authors'
names(egm_full)[names(egm_full) == 'egm..Publication.year.'] <- 'Publication year'
names(egm_full)[names(egm_full) == 'egm..URL.link.'] <- 'URL link'
names(egm_full)[names(egm_full) == 'egm..Short.title.'] <- 'Short title'
# names(egm_full)[names(egm_full) == 'egm..Cost.Evidence.'] <- 'Cost Evidence'
# names(egm_full)[names(egm_full) == 'egm..Cost.Evidence.Cont...Multi.Select..'] <- 'Cost Evidence Cont. (Multi Select)'
# names(egm_full)[names(egm_full) == 'egm..Cost.Evidence.Description.'] <- 'Cost Evidence Description'
names(egm_full)[names(egm_full) == 'egm..Cross.cutting.themes..Multi.Select...separate.values.with.single.vertical.bar......'] <- 'Cross-cutting themes (Multi Select)'
names(egm_full)[names(egm_full) == 'egm.Sex'] <- 'Sex'
names(egm_full)[names(egm_full) == 'egm..Age..Multi.Select...separate.values.with.single.vertical.bar......'] <- 'Age (Multi Select)'
names(egm_full)[names(egm_full) == 'egm..Settings..Multi.Select...separate.values.with.single.vertical.bar......'] <- 'Settings (Multi Select)'
# names(egm_full)[names(egm_full) == 'egm..Population..Multi.Select..'] <- 'Population (Multi Select)'
# names(egm_full)[names(egm_full) == 'egm..Human.Rights..Multi.Select..'] <- 'Human Rights (Multi Select)'
# names(egm_full)[names(egm_full) == 'egm..Multi.component.Intervention.'] <- 'Multi-component Intervention'
# names(egm_full)[names(egm_full) == 'egm.Comments'] <- 'Comments'

# We are also numbering the rows for allowing the merge with the intervention and outcome data

egm_full$time <- ave(as.character(egm_full$'id'), egm_full$'id', FUN=seq_along)

egm_data <- merge (x = egm_data, y = egm_full, by=c("id","time"), all = TRUE )

print("[Part 1/6] EGM Custom fields transformation completed")

#———————————————————————————————————————————————————
# Importing IER Export sheet for transformation ####
#———————————————————————————————————————————————————

print("[Part 2/6] IER Export sheet transformation ongoing...")

# We load the dataset of IER for variable with multiple answers

# ier=read_excel('1629851403-ier-records.xlsx')
ier=read_excel("AgEGM IER.xlsx")

# We make sure that we don't have duplicated by removing duplicated observations
ier<-ier[!duplicated(ier), ]

# We load the relevant variable from the IER dataset with multiple entry. 
# Note: if there is additional variable that you would need to be treated, you should add it in the line below following the same pattern ",ier$'name_of_your_variable'" and rename it afterwards

#(2022-09-18) Trying to add ,ier$'publication_type', but not working; checking with Fred. -Mark

ier_data <-data.frame(ier$'id',ier$'sub_sector',ier$'un_sustainable_development_goal',ier$'population',ier$'other_topics',ier$'equity_focus',ier$'equity_dimension',ier$'keywords',ier$'primary_dataset_format',ier$'analysis_code_format',ier$'study_materials_list',ier$'pre_registration_location',ier$'region',ier$'stateprovince_name',ier$'district_name',ier$'citytown_name',ier$'location_name',ier$'unit_of_observation')
names(ier_data)[names(ier_data) == 'ier.id'] <- 'record_id'
names(ier_data)[names(ier_data) == 'ier.sub_sector'] <- 'sub_sector'
names(ier_data)[names(ier_data) == 'ier.un_sustainable_development_goal'] <- 'un_sustainable_development_goal'
names(ier_data)[names(ier_data) == 'ier.population'] <- 'population'
names(ier_data)[names(ier_data) == 'ier.other_topics'] <- 'other_topics'
names(ier_data)[names(ier_data) == 'ier.equity_focus'] <- 'equity_focus'
names(ier_data)[names(ier_data) == 'ier.equity_dimension'] <- 'equity_dimension'
names(ier_data)[names(ier_data) == 'ier.keywords'] <- 'keywords'
names(ier_data)[names(ier_data) == 'ier.primary_dataset_format'] <- 'primary_dataset_format'
names(ier_data)[names(ier_data) == 'ier.analysis_code_format'] <- 'analysis_code_format'
names(ier_data)[names(ier_data) == 'ier.study_materials_list'] <- 'study_materials_list'
names(ier_data)[names(ier_data) == 'ier.pre_registration_location'] <- 'pre_registration_location'
# names(ier_data)[names(ier_data) == 'ier.region'] <- 'region'
# names(ier_data)[names(ier_data) == 'ier.stateprovince_name'] <- 'stateprovince_name'
# names(ier_data)[names(ier_data) == 'ier.district_name'] <- 'district_name'
# names(ier_data)[names(ier_data) == 'ier.citytown_name'] <- 'citytown_name'
# names(ier_data)[names(ier_data) == 'ier.location_name'] <- 'location_name'
names(ier_data)[names(ier_data) == 'ier.unit_of_observation'] <- 'unit_of_observation'
# names(ier_data)[names(ier_data) == 'ier.publication_type'] <- 'publication_type'

# We work on the IER data to reshape it in a way according to the template. The line below reshape it from wide to long, remove "NA" and sort it

ier_data <- melt(setDT(ier_data), id.vars = c('record_id'), variable.name = 'Arm')
ier_data <-na.omit(ier_data)
ier_data <-ier_data[order(ier_data$"record_id"),]

# We will split the IER data after each "|". The line below shows that we allow for 200 splits. In case there is more you might need to add numbers in the same pattern - e.g ',"201","202"'

ier_data <-ier_data%>% mutate(colsplit(ier_data$"value","\\|", names = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200")))
ier_data = subset(ier_data, select = -c(value) )
ier_data<-ier_data[!duplicated(ier_data), ]

# We reshape the data from wide to long now that we splitted the data and then reshape back to wide to be able to obtain the data in the right format.
# Note: Here we have 200 variables in the reshape following the parse of data. If you increase the number in the parse, you should also increase it here.

ier_data<- reshape(data=ier_data, idvar=c("record_id","Arm"),varying = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200"),v.name=c("value"),direction="long")
ier_data[ier_data==''] <- NA
ier_data <-na.omit(ier_data)

ier_data<- reshape(data=ier_data, idvar=c("record_id","time"),v.names ="value", timevar = "Arm", direction="wide")

# We rename the variable to obtain the accurate version of IER

names(ier_data)[names(ier_data) == 'value.sub_sector'] <- 'sub_sector'
names(ier_data)[names(ier_data) == 'value.un_sustainable_development_goal'] <- 'un_sustainable_development_goal'
names(ier_data)[names(ier_data) == 'value.population'] <- 'population'
names(ier_data)[names(ier_data) == 'value.other_topics'] <- 'other_topics'
names(ier_data)[names(ier_data) == 'value.equity_focus'] <- 'equity_focus'
names(ier_data)[names(ier_data) == 'value.equity_dimension'] <- 'equity_dimension'
names(ier_data)[names(ier_data) == 'value.keywords'] <- 'keywords'
names(ier_data)[names(ier_data) == 'value.primary_dataset_format'] <- 'primary_dataset_format'
names(ier_data)[names(ier_data) == 'value.analysis_code_format'] <- 'analysis_code_format'
names(ier_data)[names(ier_data) == 'value.study_materials_list'] <- 'study_materials_list'
names(ier_data)[names(ier_data) == 'value.pre_registration_location'] <- 'pre_registration_location'
# names(ier_data)[names(ier_data) == 'value.region'] <- 'region'
# names(ier_data)[names(ier_data) == 'value.stateprovince_name'] <- 'stateprovince_name'
# names(ier_data)[names(ier_data) == 'value.district_name'] <- 'district_name'
# names(ier_data)[names(ier_data) == 'value.citytown_name'] <- 'citytown_name'
# names(ier_data)[names(ier_data) == 'value.location_name'] <- 'location_name'
names(ier_data)[names(ier_data) == 'value.unit_of_observation'] <- 'unit_of_observation'
# names(ier_data)[names(ier_data) == 'value.publication_type'] <- 'publication_type'
names(ier_data)[names(ier_data) == 'record_id'] <- 'id'

print("[Part 2/6] IER Export sheet transformation completed")

#——————————————————————————————————————————————————-
# Importing SRR Export sheet for transformation ####
#———————————————————————————————————————————————————

print("[Part 3/6] SRR Export sheet transformation ongoing...")

# We load the dataset of SRR for variable with multiple answers

srr=read_excel("AgEGM SRR.xlsx", sheet = "Sheet1")

# We make sure that we don't have duplicated by removing duplicated observations
srr<-srr[!duplicated(srr), ]

# We load the relevant variable from the SRR dataset with multiple entry. 
# Note: if there is additional variable that you would need to be treated, you should add it in the line below following the same pattern ",srr$'name_of_your_variable'" and rename it afterwards

#(2022-09-18) ,srr$'publication_type'

srr_data <-data.frame(srr$'id',srr$'sub_sector',srr$'un_sustainable_development_goal',srr$'population',srr$'other_topics',srr$'equity_focus',srr$'equity_dimension',srr$'keywords',srr$'primary_dataset_format',srr$'analysis_code_format',srr$'study_materials_list',srr$'pre_registration_location')
names(srr_data)[names(srr_data) == 'srr.id'] <- 'record_id'
names(srr_data)[names(srr_data) == 'srr.sub_sector'] <- 'sub_sector'
names(srr_data)[names(srr_data) == 'srr.un_sustainable_development_goal'] <- 'un_sustainable_development_goal'
names(srr_data)[names(srr_data) == 'srr.population'] <- 'population'
names(srr_data)[names(srr_data) == 'srr.other_topics'] <- 'other_topics'
names(srr_data)[names(srr_data) == 'srr.equity_focus'] <- 'equity_focus'
names(srr_data)[names(srr_data) == 'srr.equity_dimension'] <- 'equity_dimension'
names(srr_data)[names(srr_data) == 'srr.keywords'] <- 'keywords'
names(srr_data)[names(srr_data) == 'srr.primary_dataset_format'] <- 'primary_dataset_format'
names(srr_data)[names(srr_data) == 'srr.analysis_code_format'] <- 'analysis_code_format'
names(srr_data)[names(srr_data) == 'srr.study_materials_list'] <- 'study_materials_list'
names(srr_data)[names(srr_data) == 'srr.pre_registration_location'] <- 'pre_registration_location'
# names(srr_data)[names(srr_data) == 'srr.publication_type'] <- 'publication_type'

# We work on the SRR data to reshape it in a way according to the template. The line below reshape it from wide to long, remove "NA" and sort it

srr_data <- melt(setDT(srr_data), id.vars = c('record_id'), variable.name = 'Arm')
srr_data <-na.omit(srr_data)
srr_data <-srr_data[order(srr_data$"record_id"),]

# We will split the SRR data after each "|". The line below shows that we allow for 200 splits. In case there is more you might need to add numbers in the same pattern - e.g ',"201","202"'

srr_data <- srr_data%>% mutate(colsplit(srr_data$"value","\\|", names = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200")))
srr_data = subset(srr_data, select = -c(value) )
srr_data<-srr_data[!duplicated(srr_data), ]

# We reshape the data from wide to long now that we split the data and then reshape back to wide to be able to obtain the data in the right format.
# Note: Here we have 200 variables in the reshape following the parse of data. If you increase the number in the parse, you should also increase it here.

srr_data<- reshape(data=srr_data, idvar=c("record_id","Arm"),varying = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200"),v.name=c("value"),direction="long")
srr_data[srr_data==''] <- NA
srr_data <-na.omit(srr_data)

srr_data<- reshape(data=srr_data, idvar=c("record_id","time"),v.names ="value", timevar = "Arm", direction="wide")

# We rename the variable to obtain the accurate version of SRR

names(srr_data)[names(srr_data) == 'value.sub_sector'] <- 'sub_sector'
names(srr_data)[names(srr_data) == 'value.un_sustainable_development_goal'] <- 'un_sustainable_development_goal'
names(srr_data)[names(srr_data) == 'value.population'] <- 'population'
names(srr_data)[names(srr_data) == 'value.other_topics'] <- 'other_topics'
names(srr_data)[names(srr_data) == 'value.equity_focus'] <- 'equity_focus'
names(srr_data)[names(srr_data) == 'value.equity_dimension'] <- 'equity_dimension'
names(srr_data)[names(srr_data) == 'value.keywords'] <- 'keywords'
names(srr_data)[names(srr_data) == 'value.primary_dataset_format'] <- 'primary_dataset_format'
names(srr_data)[names(srr_data) == 'value.analysis_code_format'] <- 'analysis_code_format'
names(srr_data)[names(srr_data) == 'value.study_materials_list'] <- 'study_materials_list'
names(srr_data)[names(srr_data) == 'value.pre_registration_location'] <- 'pre_registration_location'
names(srr_data)[names(srr_data) == 'value.region'] <- 'region'
names(srr_data)[names(srr_data) == 'value.stateprovince_name'] <- 'stateprovince_name'
names(srr_data)[names(srr_data) == 'value.district_name'] <- 'district_name'
names(srr_data)[names(srr_data) == 'value.citytown_name'] <- 'citytown_name'
names(srr_data)[names(srr_data) == 'value.location_name'] <- 'location_name'
names(srr_data)[names(srr_data) == 'value.unit_of_observation'] <- 'unit_of_observation'
# names(srr_data)[names(srr_data) == 'value.publication_type'] <- 'publication_type'
names(srr_data)[names(srr_data) == 'record_id'] <- 'id'

print("[Part 3/6] SRR Export sheet transformation completed")

#——————————————————————————————————————————————————————————————————————————————
# Importing IER Export sheet for inclusion and forward fill transformation ####
#——————————————————————————————————————————————————————————————————————————————

print("[Part 4/6] IER inclusion of components ongoing...")

# We load the whole dataset of IER to include the customfields dataset

# ier_df=read_excel('1629851403-ier-records.xlsx')
ier_df=read_excel('AgEGM IER.xlsx')
ier_df$time <- ave(as.character(ier_df$id), ier_df$id, FUN=seq_along)

# We merge the customfields dataset

# ier_df <- merge(x = ier_df, y = egm_data, by=c("id", "time"), all = TRUE)

# only merge the Record type ier here, and in srr code can merge the other ones and then append to each other
# first forward fill in the record type field by id
egm_data_ier <- egm_data %>% group_by('id') %>% do(fill(.,'Record type'))
unique(egm_data_ier$`Record type`) # any missing?
egm_data_ier <- egm_data_ier %>% filter(`Record type` != "srr") # drop srr
egm_data_ier$time <- as.double(egm_data_ier$time)
egm_data_ier$id <- as.double(egm_data_ier$id)
egm_data_ier <-egm_data_ier[order(egm_data_ier$'id',egm_data_ier$'time'),]
ier_df <- merge(x = ier_df, y = egm_data_ier, by=c("id", "time"), all = TRUE)

# We rename variables we want to keep

names(ier_df)[names(ier_df) == 'intervention.y'] <- 'EGM_Intervention'
names(ier_df)[names(ier_df) == 'outcome.y'] <- 'EGM_Outcome'
names(ier_df)[names(ier_df) == 'intervention.x'] <- 'DEP_intervention'
names(ier_df)[names(ier_df) == 'outcome.x'] <- 'DEP_outcome'

# We merge the IER multiple row components to the whole dataset

ier_df <- merge(x = ier_df, y = ier_data, by=c("id", "time"), all = TRUE, suffixes = c(".y",""))
columns_to_remove <- grep("_.y", names(ier_df))
ier_df <- ier_df[,-columns_to_remove]

# We prepare the forward fill the relevant data by sorting the data

ier_df$time <- as.double(ier_df$time)
ier_df$id <- as.double(ier_df$id)
ier_df <-ier_df[order(ier_df$'id',ier_df$'time'),]

# Note: In the following section, we fill the data selected to be in the IER dataset. If you need to add one you must copy the line and change as:
# ier_df57 <- ier_df56 %>% group_by('id') %>% do(fill(.,'variable_that_need_to_be_filled'))
# ier_df58 <- ier_df57 %>% group_by('id') %>% do(fill(.,'other_variable_that_need_to_be_filled'))
# colnames(ier_df)
# hours and minutes no longer exist so switched them for screening_minutes and 3ie_funded_minutes; could add language, authors, etc.

ier_df$other_resources[which(ier_df$'time' == 1 & is.na(ier_df$other_resources))] <- ""

ier_df1 <- ier_df %>% group_by('id') %>% do(fill(.,'assigned_to'))
ier_df2 <- ier_df1 %>% group_by('id') %>% do(fill(.,'type'))
ier_df3 <- ier_df2 %>% group_by('id') %>% do(fill(.,'created_at'))
ier_df4 <- ier_df3 %>% group_by('id') %>% do(fill(.,'updated_at'))
ier_df5 <- ier_df4 %>% group_by('id') %>% do(fill(.,'status'))
ier_df6 <- ier_df5 %>% group_by('id') %>% do(fill(.,'title'))
ier_df7 <- ier_df6 %>% group_by('id') %>% do(fill(.,'study_status'))
ier_df8 <- ier_df7 %>% group_by('id') %>% do(fill(.,'short_title'))
ier_df9 <- ier_df8 %>% group_by('id') %>% do(fill(.,'journal'))
ier_df10 <- ier_df9 %>% group_by('id') %>% do(fill(.,'other_journal_name'))
ier_df11 <- ier_df10 %>% group_by('id') %>% do(fill(.,'journal_volume'))
ier_df12 <- ier_df11 %>% group_by('id') %>% do(fill(.,'journal_issue'))
ier_df13 <- ier_df12 %>% group_by('id') %>% do(fill(.,'pages'))
ier_df14 <- ier_df13 %>% group_by('id') %>% do(fill(.,'year_of_publication'))
ier_df15 <- ier_df14 %>% group_by('id') %>% do(fill(.,'publication_url'))
ier_df16 <- ier_df15 %>% group_by('id') %>% do(fill(.,'open_access'))
ier_df17 <- ier_df16 %>% group_by('id') %>% do(fill(.,'3ie_funded'))
ier_df18 <- ier_df17 %>% group_by('id') %>% do(fill(.,'doi'))
ier_df19 <- ier_df18 %>% group_by('id') %>% do(fill(.,'other_resources'))
ier_df20 <- ier_df19 %>% group_by('id') %>% do(fill(.,'abstract'))
ier_df21 <- ier_df20 %>% group_by('id') %>% do(fill(.,'sector_name'))
ier_df22 <- ier_df21 %>% group_by('id') %>% do(fill(.,'primary_theme'))
ier_df23 <- ier_df22 %>% group_by('id') %>% do(fill(.,'sub_primary_theme'))
ier_df24 <- ier_df23 %>% group_by('id') %>% do(fill(.,'primary_dac_code'))
ier_df25 <- ier_df24 %>% group_by('id') %>% do(fill(.,'secondary_dac_code'))
ier_df26 <- ier_df24 %>% group_by('id') %>% do(fill(.,'crs_voluntary_dac_code'))
ier_df27 <- ier_df25 %>% group_by('id') %>% do(fill(.,'equity_description'))
ier_df28 <- ier_df26 %>% group_by('id') %>% do(fill(.,'primary_dataset_availability'))
ier_df29 <- ier_df27 %>% group_by('id') %>% do(fill(.,'primary_dataset_location'))
ier_df30 <- ier_df28 %>% group_by('id') %>% do(fill(.,'primary_dataset_url'))
ier_df31 <- ier_df29 %>% group_by('id') %>% do(fill(.,'secondary_dataset_disclosure'))
ier_df32 <- ier_df30 %>% group_by('id') %>% do(fill(.,'secondary_dataset_name'))
ier_df33 <- ier_df31 %>% group_by('id') %>% do(fill(.,'secondary_dataset_location'))
ier_df34 <- ier_df32 %>% group_by('id') %>% do(fill(.,'additional_dataset_info'))
ier_df35 <- ier_df33 %>% group_by('id') %>% do(fill(.,'analysis_code_availability'))
ier_df35 <- ier_df34 %>% group_by('id') %>% do(fill(.,'analysis_code_format_other'))
ier_df36 <- ier_df35 %>% group_by('id') %>% do(fill(.,'study_materials_availability'))
ier_df37 <- ier_df36 %>% group_by('id') %>% do(fill(.,'study_materials_other'))
ier_df38 <- ier_df37 %>% group_by('id') %>% do(fill(.,'pre_registration'))
ier_df39 <- ier_df38 %>% group_by('id') %>% do(fill(.,'pre_registration_url'))
ier_df40 <- ier_df39 %>% group_by('id') %>% do(fill(.,'protocol_pre_analysis_plan'))
ier_df41 <- ier_df40 %>% group_by('id') %>% do(fill(.,'ethics_approval'))
ier_df42 <- ier_df41 %>% group_by('id') %>% do(fill(.,'dep'))
ier_df43 <- ier_df42 %>% group_by('id') %>% do(fill(.,'exclusion_reason'))
ier_df44 <- ier_df43 %>% group_by('id') %>% do(fill(.,'exclusion_open_answer'))
ier_df45 <- ier_df44 %>% group_by('id') %>% do(fill(.,'screening_minutes'))
ier_df46 <- ier_df45 %>% group_by('id') %>% do(fill(.,'3ie_funded_minutes'))
ier_df47 <- ier_df46 %>% group_by('id') %>% do(fill(.,'project'))
ier_df48 <- ier_df47 %>% group_by('id') %>% do(fill(.,'project_ids'))
ier_df49 <- ier_df48 %>% group_by('id') %>% do(fill(.,'first_year_of_intervention'))
ier_df50 <- ier_df49 %>% group_by('id') %>% do(fill(.,'evaluation_design'))
ier_df51 <- ier_df50 %>% group_by('id') %>% do(fill(.,'evaluation_method'))
ier_df52 <- ier_df51 %>% group_by('id') %>% do(fill(.,'mixed_methods'))
ier_df53 <- ier_df52 %>% group_by('id') %>% do(fill(.,'additional_method'))
ier_df54 <- ier_df53 %>% group_by('id') %>% do(fill(.,'additional_method_2'))
ier_df55 <- ier_df54 %>% group_by('id') %>% do(fill(.,'project_name'))
ier_df56 <- ier_df55 %>% group_by('id') %>% do(fill(.,'intervention_description'))
# ier_df57 <- ier_df56 %>% group_by('id') %>% do(fill(.,'publication_type'))

# We finalize the IER side
# Note: you might need to change the ier_df56 if you include or remove variable following the pattern above

ier_df <- ier_df56

print("[Part 4/6] IER inclusion of components completed")

#——————————————————————————————————————————————————————————————————————————————
# Importing SRR Export sheet for inclusion and forward fill transformation ####
#——————————————————————————————————————————————————————————————————————————————

print("[Part 5/6] SRR inclusion of components ongoing...")

# We load the whole dataset of SRR to include the customfields dataset

srr_df=read_excel('AgEGM SRR.xlsx', sheet = "Sheet1")
srr_df$time <- ave(as.character(srr_df$id), srr_df$id, FUN=seq_along)

# only merge the Record type srr here, then append
# first forward fill in the record type field by id
egm_data_srr <- egm_data %>% group_by('id') %>% do(fill(.,'Record type'))
unique(egm_data_srr$`Record type`) # any missing?
egm_data_srr <- egm_data_srr %>% filter(`Record type` == "srr") # keep only srr
egm_data_srr$time <- as.double(egm_data_srr$time)
egm_data_srr$id <- as.double(egm_data_srr$id)
egm_data_srr <-egm_data_srr[order(egm_data_srr$'id',egm_data_srr$'time'),]
srr_df <- merge(x = srr_df, y = egm_data_srr, by=c("id", "time"), all = TRUE)
# 
# # We rename variables we want to keep
# 
names(srr_df)[names(srr_df) == 'intervention.y'] <- 'EGM_Intervention'
names(srr_df)[names(srr_df) == 'outcome.y'] <- 'EGM_Outcome'
# names(srr_df)[names(srr_df) == 'intervention.x'] <- 'DEP_intervention'
# names(srr_df)[names(srr_df) == 'outcome.x'] <- 'DEP_outcome'

# We merge the SRR multiple row components to the whole dataset

srr_df <- merge(x = srr_df, y = srr_data, by=c("id", "time"), all = TRUE, suffixes = c(".y",""))
columns_to_remove <- grep("_.y", names(srr_df))
srr_df <- srr_df[,-columns_to_remove]

# We prepare the forward fill the relevant data by sorting the data

srr_df$time <- as.double(srr_df$time)
srr_df$id <- as.double(srr_df$id)
srr_df <-srr_df[order(srr_df$'id',srr_df$'time'),]

# Note: In the following section, we fill the data selected to be in the SRR dataset. If you need to add one you must copy the line and change as:
# srr_df109 <- srr_df108 %>% group_by('id') %>% do(fill(.,'variable_that_need_to_be_filled'))
# srr_df110 <- srr_df109 %>% group_by('id') %>% do(fill(.,'other_variable_that_need_to_be_filled'))

srr_df1 <- srr_df %>% group_by('id') %>% do(fill(.,'assigned_to'))
srr_df2 <- srr_df1 %>% group_by('id') %>% do(fill(.,'type'))
srr_df3 <- srr_df2 %>% group_by('id') %>% do(fill(.,'created_at'))
srr_df4 <- srr_df3 %>% group_by('id') %>% do(fill(.,'updated_at'))
srr_df5 <- srr_df4 %>% group_by('id') %>% do(fill(.,'status'))
srr_df6 <- srr_df5 %>% group_by('id') %>% do(fill(.,'title'))
srr_df7 <- srr_df6 %>% group_by('id') %>% do(fill(.,'study_status'))
srr_df8 <- srr_df7 %>% group_by('id') %>% do(fill(.,'short_title'))
srr_df9 <- srr_df8 %>% group_by('id') %>% do(fill(.,'journal'))
srr_df10 <- srr_df9 %>% group_by('id') %>% do(fill(.,'other_journal_name'))
srr_df11 <- srr_df10 %>% group_by('id') %>% do(fill(.,'journal_volume'))
srr_df12 <- srr_df11 %>% group_by('id') %>% do(fill(.,'journal_issue'))
srr_df13 <- srr_df12 %>% group_by('id') %>% do(fill(.,'pages'))
srr_df14 <- srr_df13 %>% group_by('id') %>% do(fill(.,'year_of_publication'))
srr_df15 <- srr_df14 %>% group_by('id') %>% do(fill(.,'publication_url'))
srr_df16 <- srr_df15 %>% group_by('id') %>% do(fill(.,'open_access'))
srr_df17 <- srr_df16 %>% group_by('id') %>% do(fill(.,'3ie_funded'))
srr_df18 <- srr_df17 %>% group_by('id') %>% do(fill(.,'doi'))
srr_df19 <- srr_df18 %>% group_by('id') %>% do(fill(.,'other_resources'))
srr_df20 <- srr_df19 %>% group_by('id') %>% do(fill(.,'abstract'))
srr_df21 <- srr_df20 %>% group_by('id') %>% do(fill(.,'sector_name'))
srr_df22 <- srr_df21 %>% group_by('id') %>% do(fill(.,'primary_theme'))
srr_df23 <- srr_df22 %>% group_by('id') %>% do(fill(.,'sub_primary_theme'))
srr_df24 <- srr_df23 %>% group_by('id') %>% do(fill(.,'primary_dac_code'))
srr_df25 <- srr_df24 %>% group_by('id') %>% do(fill(.,'secondary_dac_code'))
srr_df26 <- srr_df25 %>% group_by('id') %>% do(fill(.,'crs_voluntary_dac_code'))
srr_df27 <- srr_df26 %>% group_by('id') %>% do(fill(.,'equity_description'))
srr_df28 <- srr_df27 %>% group_by('id') %>% do(fill(.,'of_studies'))
srr_df29 <- srr_df28 %>% group_by('id') %>% do(fill(.,'of_high_quality_studies'))
srr_df30 <- srr_df29 %>% group_by('id') %>% do(fill(.,'of_medium_quality_studies'))
srr_df31 <- srr_df30 %>% group_by('id') %>% do(fill(.,'primary_dataset_availability'))
srr_df32 <- srr_df31 %>% group_by('id') %>% do(fill(.,'primary_dataset_location'))
srr_df33 <- srr_df32 %>% group_by('id') %>% do(fill(.,'primary_dataset_url'))
srr_df34 <- srr_df33 %>% group_by('id') %>% do(fill(.,'secondary_dataset_disclosure'))
srr_df35 <- srr_df34 %>% group_by('id') %>% do(fill(.,'secondary_dataset_name'))
srr_df36 <- srr_df35 %>% group_by('id') %>% do(fill(.,'secondary_dataset_location'))
srr_df37 <- srr_df36 %>% group_by('id') %>% do(fill(.,'additional_dataset_info'))
srr_df38 <- srr_df37 %>% group_by('id') %>% do(fill(.,'analysis_code_availability'))
srr_df39 <- srr_df38 %>% group_by('id') %>% do(fill(.,'analysis_code_format_other'))
srr_df40 <- srr_df39 %>% group_by('id') %>% do(fill(.,'study_materials_availability'))
srr_df41 <- srr_df40 %>% group_by('id') %>% do(fill(.,'study_materials_other'))
srr_df42 <- srr_df41 %>% group_by('id') %>% do(fill(.,'pre_registration'))
srr_df43 <- srr_df42 %>% group_by('id') %>% do(fill(.,'pre_registration_url'))
srr_df44 <- srr_df43 %>% group_by('id') %>% do(fill(.,'protocol_pre_analysis_plan'))
srr_df45 <- srr_df44 %>% group_by('id') %>% do(fill(.,'ethics_approval'))
srr_df46 <- srr_df45 %>% group_by('id') %>% do(fill(.,'dep'))
srr_df47 <- srr_df46 %>% group_by('id') %>% do(fill(.,'exclusion_reason'))
srr_df48 <- srr_df47 %>% group_by('id') %>% do(fill(.,'exclusion_open_answer'))
srr_df49 <- srr_df48 %>% group_by('id') %>% do(fill(.,'screening_minutes'))
srr_df50 <- srr_df49 %>% group_by('id') %>% do(fill(.,'transparency_minutes'))
srr_df51 <- srr_df50 %>% group_by('id') %>% do(fill(.,'project'))
srr_df52 <- srr_df51 %>% group_by('id') %>% do(fill(.,'project_ids'))
srr_df53 <- srr_df52 %>% group_by('id') %>% do(fill(.,'did_the_authors_specify_the_participants_settings_and_populations_to_be_included'))
srr_df54 <- srr_df53 %>% group_by('id') %>% do(fill(.,'did_the_authors_specify_the_interventions_to_be_included'))
srr_df55 <- srr_df54 %>% group_by('id') %>% do(fill(.,'did_the_authors_specify_the_outcomes_to_be_included'))
srr_df56 <- srr_df55 %>% group_by('id') %>% do(fill(.,'were_the_criteria_used_for_deciding_which_studies_to_include_in_the_review_reported'))
srr_df57 <- srr_df56 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_1_5'))
srr_df58 <- srr_df57 %>% group_by('id') %>% do(fill(.,'did_the_authors_avoid_a_language_bias_in_the_search'))
srr_df59 <- srr_df58 %>% group_by('id') %>% do(fill(.,'was_greyunpublished_literature_included_in_the_search'))
srr_df60 <- srr_df59 %>% group_by('id') %>% do(fill(.,'were_the_relevant_databases_searched'))
srr_df61 <- srr_df60 %>% group_by('id') %>% do(fill(.,'were_the_reference_lists_of_included_articles_checked'))
srr_df62 <- srr_df61 %>% group_by('id') %>% do(fill(.,'were_authorsexperts_contacted'))
srr_df63 <- srr_df62 %>% group_by('id') %>% do(fill(.,'was_the_search_for_evidence_reasonably_comprehensive'))
srr_df64 <- srr_df63 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_7_12'))
srr_df65 <- srr_df64 %>% group_by('id') %>% do(fill(.,'does_the_review_cover_an_appropriate_time_period'))
srr_df66 <- srr_df65 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_14'))
srr_df67 <- srr_df66 %>% group_by('id') %>% do(fill(.,'is_a_list_of_the_included_studies_provided'))
srr_df68 <- srr_df67 %>% group_by('id') %>% do(fill(.,'is_a_list_of_the_excluded_studies_provided'))
srr_df69 <- srr_df68 %>% group_by('id') %>% do(fill(.,'was_bias_in_the_selection_of_articles_avoided'))
srr_df70 <- srr_df69 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_16_19'))
srr_df71 <- srr_df70 %>% group_by('id') %>% do(fill(.,'do_the_authors_report_the_criteria_used_for_assessing_the_quality_risk_of_bias'))
srr_df72 <- srr_df71 %>% group_by('id') %>% do(fill(.,'is_there_a_table_or_summary_reporting_the_assessment_of_each_included_study_for_each_criteria'))
srr_df73 <- srr_df72 %>% group_by('id') %>% do(fill(.,'were_sensible_criteria_used_that_focus_on_the_qualityrisk_of_bias_and_not_other_qualities_of_the_studies_such_as_precision_or_applicabilityexternal_validity'))
srr_df74 <- srr_df73 %>% group_by('id') %>% do(fill(.,'did_the_authors_use_appropriate_criteria_to_assess_the_quality_and_risk_of_bias_in_analysing_the_studies_that_are_included'))
srr_df75 <- srr_df74 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_21_24'))
srr_df76 <- srr_df75 %>% group_by('id') %>% do(fill(.,'overall_how_much_confidence_do_you_have_in_the_methods_used_to_identify_include_and_critically_appraise_studies'))
srr_df77 <- srr_df76 %>% group_by('id') %>% do(fill(.,'compile_the_important_comments_from_questions_1_25_on_methods_used_to_identify_include_and_critically_appraise_studies_as_an_overall_summary_for_the_section'))
srr_df78 <- srr_df77 %>% group_by('id') %>% do(fill(.,'is_there_a_table_or_summary_of_the_characteristics_of_the_participants_intervention_and_outcomes_for_the_included_studies'))
srr_df79 <- srr_df78 %>% group_by('id') %>% do(fill(.,'is_there_a_table_or_summary_of_the_results_of_all_the_included_studies'))
srr_df80 <- srr_df79 %>% group_by('id') %>% do(fill(.,'were_the_characteristics_and_results_of_the_included_studies_reliably_reported'))
srr_df81 <- srr_df80 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_26_31'))
srr_df82 <- srr_df81 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_33'))
srr_df83 <- srr_df82 %>% group_by('id') %>% do(fill(.,'did_the_review_ensure_that_included_studies_were_similar_enough_that_it_made_sense_to_combine_them_sensibly_divide_the_included_studies_into_homogeneous_groups_or_sensibly_conclude_that_it_did_not_make_sense_to_combine_or_group_the_included_studies'))
srr_df84 <- srr_df83 %>% group_by('id') %>% do(fill(.,'did_the_review_describe_the_extent_to_which_there_were_important_differences_in_the_results_of_the_included_studies'))
srr_df85 <- srr_df84 %>% group_by('id') %>% do(fill(.,'if_a_meta_analysis_was_done_was_the_isup2sup_or_chi_squared_test_for_heterogeneity_or_other_appropriate_statistic_reported_if_no_statistical_test_was_reported_is_a_qualitative_justification_made_for_the_use_of_random_effects'))
srr_df86 <- srr_df85 %>% group_by('id') %>% do(fill(.,'did_the_review_describe_the_extent_of_heterogeneity'))
srr_df87 <- srr_df86 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_35_38'))
srr_df88 <- srr_df87 %>% group_by('id') %>% do(fill(.,'how_was_the_data_analysis_done_enter_multiple_responses_if_needed'))
srr_df89 <- srr_df88 %>% group_by('id') %>% do(fill(.,'how_were_the_studies_weighted_in_the_analysis_enter_multiple_responses_if_needed'))
srr_df90 <- srr_df89 %>% group_by('id') %>% do(fill(.,'did_the_review_address_unit_of_analysis_errors'))
srr_df91 <- srr_df90 %>% group_by('id') %>% do(fill(.,'were_the_findings_of_the_relevant_studies_combined_or_not_combined_appropriately_relative_to_the_primary_question_the_review_addresses_and_the_available_data'))
srr_df92 <- srr_df91 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_40_43'))
srr_df93 <- srr_df92 %>% group_by('id') %>% do(fill(.,'does_the_review_make_clear_which_evidence_is_subject_to_low_risk_of_bias_in_assessing_causality_attribution_of_outcomes_to_intervention_and_which_is_likely_to_be_biased_and_does_so_appropriately'))
srr_df94 <- srr_df93 %>% group_by('id') %>% do(fill(.,'does_the_review_report_evidence_appropriately'))
srr_df95 <- srr_df94 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_45_47'))
srr_df96 <- srr_df95 %>% group_by('id') %>% do(fill(.,'were_factors_that_the_review_authors_considered_as_likely_explanatory_factors_clearly_described'))
srr_df97 <- srr_df96 %>% group_by('id') %>% do(fill(.,'was_a_sensible_method_used_to_explore_the_extent_to_which_key_factors_explained_heterogeneity_enter_multiple_responses_if_needed'))
srr_df98 <- srr_df97 %>% group_by('id') %>% do(fill(.,'did_the_review_examine_the_extent_to_which_specific_factors_might_explain_differences_in_the_results_of_the_included_studies'))
srr_df99 <- srr_df98 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_49_51'))
srr_df100 <- srr_df99 %>% group_by('id') %>% do(fill(.,'overall_how_much_confidence_do_you_have_in_the_methods_used_to_analyse_the_findings_relative_to_the_primary_question_addressed_in_the_review'))
srr_df101 <- srr_df100 %>% group_by('id') %>% do(fill(.,'compile_the_important_comments_from_questions_28_53_on_methods_used_to_analyse_the_findings_as_an_overall_summary_for_the_section'))
srr_df102 <- srr_df101 %>% group_by('id') %>% do(fill(.,'are_there_any_other_aspects_of_the_review_not_mentioned_before_which_lead_you_to_question_the_results_enter_multiple_responses_if_needed'))
srr_df103 <- srr_df102 %>% group_by('id') %>% do(fill(.,'are_there_any_mitigating_factors_which_should_be_taken_into_account_in_determining_the_reviews_reliability_enter_multiple_responses_if_needed'))
srr_df104 <- srr_df103 %>% group_by('id') %>% do(fill(.,'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_53_56'))
srr_df105 <- srr_df104 %>% group_by('id') %>% do(fill(.,'based_on_the_above_assessments_of_the_methods_how_would_you_rate_the_reliability_of_the_review'))
srr_df106 <- srr_df105 %>% group_by('id') %>% do(fill(.,'provide_an_overall_of_the_assessment_use_consistent_style_and_wording'))
srr_df107 <- srr_df106 %>% group_by('id') %>% do(fill(.,'intervention_description'))
# srr_df108 <- srr_df107 %>% group_by('id') %>% do(fill(.,'publication_type'))

# We finalize the SRR side
# Note: you might need to change the srr_df108 if you include or remove variable following the pattern above

srr_df <- srr_df107

print("[Part 5/6] SRR inclusion of components completed")

#———————————————————————————————————————————————————————————————————————————
# Appending SSR and IER export and re-ordering data for final delivery  ####
#———————————————————————————————————————————————————————————————————————————

print("[Part 6/6] IER - SRR appending and finalization ongoing...")

# We append the dataframe generated from IER and SRR export

final_df = rbind(ier_df, srr_df)

# We rename variable and drop variable that were created for the merge

names(final_df)[names(final_df) == 'id'] <- 'record_id'

final_df = subset(final_df, select = -c(time))
final_df = subset(final_df, select = -c(`"id"`))

# We order the variable. To do so we create a list of variable in the order we want it to be.
# Note: if you remove some variables or if you had some you might need to update this list following the same pattern and put the variable in the order you want.
# Some of these didn't exist anymore so had to change - double check (and rename intervention and outcome)

col_order <- c('record_id',
	'legacy_id',
	'assigned_to',
	'type',
	'created_at',
	'updated_at',
	'status',
	'title',
	'database_source',
	'study_status',
	'short_title',
	'foreign_title',
	'language',
	'authors',
  'author_affiliation',
  'department',
  'author_country',
	'publication_type',
  'journal',
  'other_journal_name',
  'journal_volume',
  'journal_issue',
  'pages',
  'year_of_publication',
  'publication_url',
  'publisher_location',
  'open_access',
  '3ie_funded',
  'doi',
  'other_resources',
  'abstract',
  'sector_name',
  'sub_sector',
  'primary_theme',
  'sub_primary_theme',
  'additional_theme',
  'additional_sub_theme',
  'primary_dac_code',
  'secondary_dac_code',
  'crs_voluntary_dac_code',
  'additional_primary_dac_code',
  'additional_secondary_dac_code',
  'additional_crs_voluntary_dac_code',
  'un_sustainable_development_goal',
  'population',
  'other_topics',
	'first_year_of_intervention',
  'equity_focus',
  'equity_dimension',
  'equity_description',
  'keywords',
  'continent',
  'country',
  'income_level',
  'fcv_status',
# 	'region',
# 	'stateprovince_name',
# 	'district_name',
# 	'citytown_name',
#   'location_name',
	'evaluation_design',
	'evaluation_method',
	'mixed_methods',
	'additional_method',
	'additional_method_2',
	'unit_of_observation',
	'project_name',
	'implementation_agencies',
	'implementation_agencies_name',
	'program_funding_agency',
	'program_funding_agency_name',
  'research_funding_agency',
  'research_funding_agency_name',
  'DEP_intervention',
  'intervention_description',
  'DEP_outcome',
  'outcome_description',
  'indicator',
  'indicator_description',
  'subgroup',
  'direction_of_effect',
  'significance_of_effect',
  'of_participants',
  'comments',
  'format',
  'point_estimate',
  'significance_level',
  'confidence_levellow',
  'confidence_levelhigh',
  'chi_squared',
  'i_squared',
  'tau_squared',
  'tau_sqaure_number',
  'of_studies',
  'of_high_quality_studies',
  'of_medium_quality_studies',
  'primary_dataset_availability',
  'primary_dataset_location',
  'primary_dataset_url',
  'primary_dataset_format',
  'secondary_dataset_disclosure',
  'secondary_dataset_name',
  'secondary_dataset_location',
  'additional_dataset_info',
  'analysis_code_availability',
  'analysis_code_format',
  'analysis_code_format_other',
  'study_materials_availability',
  'study_materials_list',
  'study_materials_other',
  'pre_registration',
  'pre_registration_location',
  'pre_registration_url',
  'protocol_pre_analysis_plan',
  'ethics_approval',
  'dep',
  'exclusion_reason',
  'exclusion_open_answer',
  'screening_minutes',
  '3ie_funded_minutes',
  'project',
  'project_ids',
  'grantholding_institution',
  'evidence_programme',
  'synopsis',
  'context',
  'research_questions',
  'methodology',
  'main_findings',
  'instances_of_evidence_use',
  'review_other',
  'quantitative_method',
  'quantitative_method_other',
  'qualitative_method',
  'qualitative_method_other',
  'quality_assessment_tool',
  'quality_assessment_tool_other',
  'extract_comment',
  'overall_of_studies',
  'overall_of_high_quality_studies',
  'overall_of_medium_quality_studies',
  'background',
  'objectives',
  'headline_findings',
  'evidence_findings',
  'policy_findings',
  'research_findings',
  'methodology_summary',
  'external_validity',
  'external_validity_treatment',
  'did_the_authors_specify_the_participants_settings_and_populations_to_be_included',
  'did_the_authors_specify_the_interventions_to_be_included',
  'did_the_authors_specify_the_outcomes_to_be_included',
  'were_the_criteria_used_for_deciding_which_studies_to_include_in_the_review_reported',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_1_5',
  'did_the_authors_avoid_a_language_bias_in_the_search',
  'was_greyunpublished_literature_included_in_the_search',
  'were_the_relevant_databases_searched',
  'were_the_reference_lists_of_included_articles_checked',
  'were_authorsexperts_contacted',
  'was_the_search_for_evidence_reasonably_comprehensive',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_7_12',
  'does_the_review_cover_an_appropriate_time_period',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_14',
  'is_a_list_of_the_included_studies_provided',
  'is_a_list_of_the_excluded_studies_provided',
  'was_bias_in_the_selection_of_articles_avoided',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_16_19',
  'do_the_authors_report_the_criteria_used_for_assessing_the_quality_risk_of_bias',
  'is_there_a_table_or_summary_reporting_the_assessment_of_each_included_study_for_each_criteria',
  'were_sensible_criteria_used_that_focus_on_the_qualityrisk_of_bias_and_not_other_qualities_of_the_studies_such_as_precision_or_applicabilityexternal_validity',
  'did_the_authors_use_appropriate_criteria_to_assess_the_quality_and_risk_of_bias_in_analysing_the_studies_that_are_included',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_21_24',
  'overall_how_much_confidence_do_you_have_in_the_methods_used_to_identify_include_and_critically_appraise_studies',
  'compile_the_important_comments_from_questions_1_25_on_methods_used_to_identify_include_and_critically_appraise_studies_as_an_overall_summary_for_the_section',
  'is_there_a_table_or_summary_of_the_characteristics_of_the_participants_intervention_and_outcomes_for_the_included_studies',
  'is_there_a_table_or_summary_of_the_results_of_all_the_included_studies',
  'were_the_characteristics_and_results_of_the_included_studies_reliably_reported',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_questions_provide_evidence_such_as_page_numbers_for_your_decisions_26_31',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_33',
  'did_the_review_ensure_that_included_studies_were_similar_enough_that_it_made_sense_to_combine_them_sensibly_divide_the_included_studies_into_homogeneous_groups_or_sensibly_conclude_that_it_did_not_make_sense_to_combine_or_group_the_included_studies',
  'did_the_review_describe_the_extent_to_which_there_were_important_differences_in_the_results_of_the_included_studies',
	'if_a_meta_analysis_was_done_was_the_isup2sup_or_chi_squared_test_for_heterogeneity_or_other_appropriate_statistic_reported_if_no_statistical_test_was_reported_is_a_qualitative_justification_made_for_the_use_of_random_effects',
  'did_the_review_describe_the_extent_of_heterogeneity',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_35_38',
  'how_was_the_data_analysis_done_enter_multiple_responses_if_needed',
  'how_were_the_studies_weighted_in_the_analysis_enter_multiple_responses_if_needed',
  'did_the_review_address_unit_of_analysis_errors',
	'were_the_findings_of_the_relevant_studies_combined_or_not_combined_appropriately_relative_to_the_primary_question_the_review_addresses_and_the_available_data',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_40_43',
	'does_the_review_make_clear_which_evidence_is_subject_to_low_risk_of_bias_in_assessing_causality_attribution_of_outcomes_to_intervention_and_which_is_likely_to_be_biased_and_does_so_appropriately',
  'does_the_review_report_evidence_appropriately',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_45_47',
  'were_factors_that_the_review_authors_considered_as_likely_explanatory_factors_clearly_described',
  'was_a_sensible_method_used_to_explore_the_extent_to_which_key_factors_explained_heterogeneity_enter_multiple_responses_if_needed',
  'did_the_review_examine_the_extent_to_which_specific_factors_might_explain_differences_in_the_results_of_the_included_studies',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_49_51',
  'overall_how_much_confidence_do_you_have_in_the_methods_used_to_analyse_the_findings_relative_to_the_primary_question_addressed_in_the_review',
  'compile_the_important_comments_from_questions_28_53_on_methods_used_to_analyse_the_findings_as_an_overall_summary_for_the_section',
  'are_there_any_other_aspects_of_the_review_not_mentioned_before_which_lead_you_to_question_the_results_enter_multiple_responses_if_needed',
  'are_there_any_mitigating_factors_which_should_be_taken_into_account_in_determining_the_reviews_reliability_enter_multiple_responses_if_needed',
  'note_important_limitations_and_any_uncertainty_related_to_the_above_question_provide_evidence_such_as_page_numbers_for_your_decisions_53_56',
  'based_on_the_above_assessments_of_the_methods_how_would_you_rate_the_reliability_of_the_review',
  'provide_an_overall_of_the_assessment_use_consistent_style_and_wording',
  'Project ID',
  'Record type',
  'Record Title',
  'Record authors',
  'Publication year',
  'URL link',
  'Short title',
  'Sex', 
  'Age (Multi Select)', 
  'Settings (Multi Select)', 
  'Cross-cutting themes (Multi Select)',
  'Arm',
  'EGM_Intervention',
  'EGM_Outcome',
  'mul_int_desc')
 
# We use the list of variable computed to order the final dataset
   
final_df <- final_df[, col_order]

#——————————————————————————
#Ag team modifications ####
#——————————————————————————


#Combine fixed effects and diff-in-diff
final_df <- final_df %>% 
  mutate(evaluation_method = case_when(
    evaluation_method == "Randomised controlled trial" ~ "Randomised controlled trial",
    evaluation_method == "Instrumental variable estimation" ~ "Instrumental variable estimation",
    evaluation_method == "Interrupted time series analysis" ~ "Interrupted time series analysis",
    evaluation_method == "Regression discontinuity design" ~ "Regression discontinuity design",
    evaluation_method == "Statistical matching" ~ "Statistical matching",
    evaluation_method == "Synthetic control" ~ "Synthetic control",
    (evaluation_method == "Difference-in-difference" | evaluation_method == "Fixed effects estimation") ~ "Fixed effects (incl. diff-in-diff)"
  ))

#Clean agency names
final_df <- final_df %>% 
  mutate(implementation_agencies_name = case_when(
    implementation_agencies_name == "Not applicable" ~ "Not specified",
    implementation_agencies_name == "NA" ~ "Not specified",
    TRUE ~ implementation_agencies_name
  ))

final_df <- final_df %>% 
  mutate(program_funding_agency_name = case_when(
    program_funding_agency_name == "Not applicable" ~ "Not specified",
    program_funding_agency_name == "NA" ~ "Not specified",
    TRUE ~ program_funding_agency_name
  ))

final_df <- final_df %>% 
  mutate(research_funding_agency_name = case_when(
    research_funding_agency_name == "Not applicable" ~ "Not specified",
    research_funding_agency_name == "NA" ~ "Not specified",
    research_findings == "No appl" ~ "Not specified",
    TRUE ~ research_funding_agency_name
  ))

#Clean leading spaces from equity focus
final_df <- final_df %>% 
  mutate(equity_focus = case_when(
    equity_focus == " Equity sensitive methodology" ~ "Equity sensitive methodology",
    equity_focus == " Equity sensitive research process" ~ "Equity sensitive research process",
    equity_focus == " Equity sensitive analytical framework" ~ "Equity sensitive analytical framework",
    equity_focus == " Sub-group analysis by sex" ~ "Sub-group analysis by sex",
    equity_focus == " Sub-group analysis (other than sex)" ~ "Sub-group analysis (other than sex)",
    equity_focus == " Intervention targets vulnerable population" ~ "Intervention targets vulnerable population",
    equity_focus == " Measures effects on an inequality outcome" ~ "Measures effects on an inequality outcome",
    TRUE ~ equity_focus
  ))


#Clean leading spaces from equity dimension
final_df <- final_df %>% 
  mutate(equity_dimension = case_when(
    equity_dimension == " Age" ~ "Age",
    equity_dimension == " Caste" ~ "Caste",
    equity_dimension == " Conflict-affected" ~ "Conflict-affected",
    equity_dimension == " Disability" ~ "Disability",
    equity_dimension == " Education" ~ "Education",
    equity_dimension == " Ethnicity" ~ "Ethnicity",
    equity_dimension == " Head of household" ~ "Head of household",
    equity_dimension == " HIV/AIDS" ~ "HIV/AIDS",
    equity_dimension == " Land ownership" ~ "Land ownership",
    equity_dimension == " Land size" ~ "Land size",
    equity_dimension == " Other" ~ "Other",
    equity_dimension == " Place of residence" ~ "Place of residence",
    equity_dimension == " Sex" ~ "Sex",
    equity_dimension == " Sexual identity" ~ "Sexual identity",
    equity_dimension == " Socioeconomic status" ~ "Socioeconomic status",
    TRUE ~ equity_dimension
  ))

#Create a column for the intervention domain
final_df <- final_df %>%
  mutate(egm_int_domain = case_when(
    (EGM_Intervention == "Disseminating productivity/sustainability-focused crop/livestock/fisheries information" 
     | EGM_Intervention == "Promoting access to and application of improved inputs"
     | EGM_Intervention == "Introduction of irrigation systems"
     | EGM_Intervention == "Building the capacity of extension systems and non-producer private sector actors"
     | EGM_Intervention == "Facilitating access to productivity-enhancing technologies"
     | EGM_Intervention == "Funding for agricultural research and development"
     | EGM_Intervention == "Forest conservation schemes") ~ "Training & innovations",
    (EGM_Intervention == "Women’s empowerment/ engagement of women and other marginalized actors in the ag sector" 
     | EGM_Intervention == "Disseminating information on marketing/business skills"
     | EGM_Intervention == "Disseminating combined productivity and marketing information" 
     | EGM_Intervention == "Agricultural market information"
     | EGM_Intervention == "Weather information systems for farmers"
     | EGM_Intervention == "Market information systems for (non-producer) SMEs"
     | EGM_Intervention == "Producer/marketing group formation/capacity building"
     | EGM_Intervention == "Linking farmers to purchasers/processors" 
     | EGM_Intervention == "Building business-to-business and business-to-government linkages"
     | EGM_Intervention == "Facilitating farmer access to innovative and/or existing insurance products"
     | EGM_Intervention == "Facilitating farmer access to financial products (incl. credit)"
     | EGM_Intervention == "Building relationships with private sector investors" 
     | EGM_Intervention == "Government loan guarantees"
     | EGM_Intervention == "Building/maintaining rural roads"
     | EGM_Intervention == "Building/maintaining agricultural storage facilities" 
     | EGM_Intervention == "Rural electrical/telecommunications infrastructure") ~ "Markets",
    (EGM_Intervention == "Improving ag’ ministries’ risk management procedures"
     | EGM_Intervention == "Promoting private investment in R&D"
     | EGM_Intervention == "Capacity building for ag research in LMICs" 
     | EGM_Intervention == "Supporting ag ministries in developing sound R&D investment strategies"
     | EGM_Intervention == "Improving systems for evidence generation and use (market research, monitoring & evaluation)"
     | EGM_Intervention == "Supporting transparent and inclusive processes for ag-related policymaking") ~ "Evidence use",
    (EGM_Intervention == "Enacting/enforcing legislation to promote competitiveness (incl. antitrust)"
     | EGM_Intervention == "Price regulations (incl. subsidies)"
     | EGM_Intervention == "Trade policy"
     | EGM_Intervention == "Land rights reform"
     | EGM_Intervention == "Financial sector reform" 
     | EGM_Intervention == "Agricultural practice recommendations"
     | EGM_Intervention == "Migration policy/programmes") ~ "Regulatory environment"
    ))

# create new multi-component columns
# check if that many splits are needed
# unique(final_df$multi13)
final_df<-final_df%>% mutate(colsplit(final_df$"mul_int_desc","\\|", names = c("multi1","multi2","multi3","multi4","multi5","multi6","multi7")))

# function above does the same, but creates new variables rather than replacing
final_df<-final_df %>% 
  mutate_at(
    vars(one_of("multi1","multi2","multi3","multi4","multi5","multi6","multi7")),
    funs(case_when(
      (. == "Disseminating productivity/sustainability-focused crop/livestock/fisheries information" 
       | . == "Promoting access to and application of improved inputs"
       | . == "Introduction of irrigation systems" 
       | . == "Building the capacity of extension systems and non-producer private sector actors" 
       | . == "Facilitating access to productivity-enhancing technologies"
       | . == "Funding for agricultural research and development"
       | . == "Forest conservation schemes" | . == "Cash/asset transfer and farmer training") ~ 1, 
      (. == "Women’s empowerment/ engagement of women and other marginalized actors in the ag sector" 
       | . == "Disseminating information on marketing/business skills"
       | . == "Disseminating combined productivity and marketing information" 
       | . == "Agricultural market information"
       | . == "Weather information systems for farmers"
       | . == "Market information systems for (non-producer) SMEs"
       | . == "Producer/marketing group formation/capacity building"
       | . == "Linking farmers to purchasers/processors" 
       | . == "Building business-to-business and business-to-government linkages"
       | . == "Facilitating farmer access to innovative and/or existing insurance products"
       | . == "Facilitating farmer access to financial products (incl. credit)"
       | . == "Building relationships with private sector investors" 
       | . == "Government loan guarantees"
       | . == "Building/maintaining rural roads"
       | . == "Building/maintaining agricultural storage facilities"
       | . == "Rural electrical/telecommunications infrastructure") ~ 2,
      (. == "Improving ag’ ministries’ risk management procedures"
       | . == "Promoting private investment in R&D"
       | . == "Capacity building for ag research in LMICs" 
       | . == "Supporting ag ministries in developing sound R&D investment strategies"
       | . == "Improving systems for evidence generation and use (market research, monitoring & evaluation)"
       | . == "Supporting transparent and inclusive processes for ag-related policymaking") ~ 3,
      (. == "Enacting/enforcing legislation to promote competitiveness (incl. antitrust)"
       | . == "Price regulations (incl. subsidies)"
       | . == "Trade policy" | . == "Land rights reform"
       | . == "Financial sector reform" 
       | . == "Agricultural practice recommendations"
       | . == "Migration policy/programmes") ~ 4,
      TRUE ~ NA_real_ # This is for all other values 
    )))

# create new variable that is multi-component category
final_df_new <- mutate(final_df,
                       multi_cat = case_when(
                         (multi1 == 1) & (multi2 == 1) & (multi3 == 1 | is.na(multi3)) & (multi4 == 1 | is.na(multi4)) & (multi5 == 1 | is.na(multi5)) & (multi6 == 1 | is.na(multi6)) & (multi7 == 1 | is.na(multi7)) ~ 1, 
                         (multi1 == 2) & (multi2 == 2 | is.na(multi2)) & (multi3 == 2 | is.na(multi3)) & (multi4 == 2 | is.na(multi4)) & (multi5 == 2 | is.na(multi5)) & (multi6 == 2 | is.na(multi6)) & (multi7 == 2 | is.na(multi7)) ~ 2,
                         (multi1 == 3) & (multi2 == 3 | is.na(multi2)) & (multi3 == 3 | is.na(multi3)) & (multi4 == 3 | is.na(multi4)) & (multi5 == 3 | is.na(multi5)) & (multi6 == 3 | is.na(multi6)) & (multi7 == 3 | is.na(multi7)) ~ 3,
                         (multi1 == 4) & (multi2 == 4 | is.na(multi2)) & (multi3 == 4 | is.na(multi3)) & (multi4 == 4 | is.na(multi4)) & (multi5 == 4 | is.na(multi5)) & (multi6 == 4 | is.na(multi6)) & (multi7 == 4 | is.na(multi7)) ~ 4,
                         ((multi1 == 1) | (multi1 == 2)) & ((multi2 == 1 | multi2 == 2)) & ((multi3 == 1) | (multi3 == 2) | is.na(multi3)) & ((multi4 == 1) | (multi4 == 2) | is.na(multi4)) & ((multi5 == 1) | (multi5 == 2) | is.na(multi5)) & ((multi6 == 1) | (multi6 == 2) | is.na(multi6)) & ((multi7 == 1) | (multi7 == 2) | is.na(multi7)) ~ 12,
                         ((multi1 == 1) | (multi1 == 3)) & ((multi2 == 1 | multi2 == 3)) & ((multi3 == 1) | (multi3 == 3) | is.na(multi3)) & ((multi4 == 1) | (multi4 == 3) | is.na(multi4)) & ((multi5 == 1) | (multi5 == 3) | is.na(multi5)) & ((multi6 == 1) | (multi6 == 3) | is.na(multi6)) & ((multi7 == 1) | (multi7 == 3) | is.na(multi7)) ~ 13,
                         ((multi1 == 1) | (multi1 == 4)) & ((multi2 == 1 | multi2 == 4)) & ((multi3 == 1) | (multi3 == 4) | is.na(multi3)) & ((multi4 == 1) | (multi4 == 4) | is.na(multi4)) & ((multi5 == 1) | (multi5 == 4) | is.na(multi5)) & ((multi6 == 1) | (multi6 == 4) | is.na(multi6)) & ((multi7 == 1) | (multi7 == 4) | is.na(multi7)) ~ 14,
                         ((multi1 == 2) | (multi1 == 3)) & ((multi2 == 2 | multi2 == 3)) & ((multi3 == 2) | (multi3 == 3) | is.na(multi3)) & ((multi4 == 2) | (multi4 == 3) | is.na(multi4)) & ((multi5 == 2) | (multi5 == 3) | is.na(multi5)) & ((multi6 == 2) | (multi6 == 3) | is.na(multi6)) & ((multi7 == 2) | (multi7 == 3) | is.na(multi7)) ~ 23,
                         ((multi1 == 2) | (multi1 == 4)) & ((multi2 == 2 | multi2 == 4)) & ((multi3 == 2) | (multi3 == 4) | is.na(multi3)) & ((multi4 == 2) | (multi4 == 4) | is.na(multi4)) & ((multi5 == 2) | (multi5 == 4) | is.na(multi5)) & ((multi6 == 2) | (multi6 == 4) | is.na(multi6)) & ((multi7 == 2) | (multi7 == 4) | is.na(multi7)) ~ 24,
                         ((multi1 == 3) | (multi1 == 4)) & ((multi2 == 3 | multi2 == 4)) & ((multi3 == 3) | (multi3 == 4) | is.na(multi3)) & ((multi4 == 3) | (multi4 == 4) | is.na(multi4)) & ((multi5 == 3) | (multi5 == 4) | is.na(multi5)) & ((multi6 == 3) | (multi6 == 4) | is.na(multi6)) & ((multi7 == 3) | (multi7 == 4) | is.na(multi7)) ~ 34,
                         TRUE ~ NA_real_ # This is for all other values 
                       ))

# replace with the name of the category
final_df_new$multi_cat_name<-ifelse(final_df_new$multi_cat==1, "Multi-comp training & innovations",
                                    ifelse(final_df_new$multi_cat==2, "Multi-comp markets",
                                           ifelse(final_df_new$multi_cat==3, "Multi-comp evidence use", 
                                                  ifelse(final_df_new$multi_cat==4, "Multi-comp regulatory enviro.",
                                                         ifelse(final_df_new$multi_cat==12, "Info + markets",
                                                                ifelse(final_df_new$multi_cat==13, "Info + evidence use",
                                                                       ifelse(final_df_new$multi_cat==14, "Info + regulatory",
                                                                              ifelse(final_df_new$multi_cat==23, "Markets + evidence use",
                                                                                     ifelse(final_df_new$multi_cat==24, "Markets + regulatory",
                                                                                            ifelse(final_df_new$multi_cat==34, "Evidence use + regulatory",  NA))))))))))

                                                                                        
# If you want to split any of the multi select columns into their own columns: 
# names(final_df)[names(final_df) == 'Cross-cutting themes (Multi Select)'] <- 'Cross_cutting_themes'
# final_df <- final_df %>% mutate(partner_local_dev = str_extract(Cross_cutting_themes, "Partnerships and locally-led development"))
# final_df <- final_df %>% mutate(climate_change = str_extract(Cross_cutting_themes, "Climate change"))
# final_df <- final_df %>% mutate(cost_analysis = str_extract(Cross_cutting_themes, "Cost data/ analysis"))

print("[Part 6/6] IER - SRR appending and finalization completed")

# We outsheet the final version 

write_xlsx(final_df_new,'AgEGM_data_pub_test.xlsx')

print("Final document is outsheeted - Program is done.") 

