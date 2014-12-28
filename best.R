best <- function(state, outcome) {
    ## Read outcome data
    outcome_a <- read.csv("outcome-of-care-measures.csv")
    ##get states names
    outcome_states <-outcome_a[,7]
    index <-duplicated(outcome_states)
    outcome_states_only <-outcome_states[!index]
    ##get outcomes names
    outcome_names <- c("heart attack","heart failure", "pneumonia")
    ## Check that state and outcome are valid
    ##invalid outcome
    b <-0
    for(i in outcome_names)
    {
        if(i==outcome){
            ##转化成首字母大写   
            outcome <-capwords(outcome)
            outcome_names_full <-paste("Hospital 30-Day Death (Mortality) Rates from",outcome, sep =" ")
         b <- b+1               
        }
        
    }
    if(b==0){
        
        stop("invalid outcome")
    }
    ##invalid state
    c<- 0
    for(i in outcome_states_only)
    {
        if(i==state){
            outcome_a <-outcome_a[outcome_a["State"]==state,]
           c <- c+1               
        }

    }
    if(c==0){
        
        
        stop("invalid state")
    }

    ##creat a df for col_name
    col_name <-c("Provider Number","Hospital Name","Address 1","Address 2","Address 3","City","State","ZIP Code","County Name","Phone Number","Hospital 30-Day Death (Mortality) Rates from Heart Attack","Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Attack","Hospital 30-Day Death (Mortality) Rates from Heart Failure","Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Failure","Hospital 30-Day Death (Mortality) Rates from Pneumonia","Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Number of Patients - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Footnote - Hospital 30-Day Death (Mortality) Rates from Pneumonia","Hospital 30-Day Readmission Rates from Heart Attack","Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Attack","Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack","Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack","Number of Patients - Hospital 30-Day Readmission Rates from Heart Attack","Footnote - Hospital 30-Day Readmission Rates from Heart Attack","Hospital 30-Day Readmission Rates from Heart Failure","Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Failure","Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure","Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure","Number of Patients - Hospital 30-Day Readmission Rates from Heart Failure","Footnote - Hospital 30-Day Readmission Rates from Heart Failure","Hospital 30-Day Readmission Rates from Pneumonia","Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Pneumonia","Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia","Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia","Number of Patients - Hospital 30-Day Readmission Rates from Pneumonia","Footnote - Hospital 30-Day Readmission Rates from Pneumonia")
    col_df <-data.frame(col_name,stringsAsFactors = FALSE)
    ##change name to index
    name_to_index <-which(col_df==outcome_names_full)
    ##get a new data.frame for hospital and mor ,no NA
    ##outcome_a[,name_to_index] <-as.numeric(outcome_a[,name_to_index])
    outcome_a<-outcome_a[(outcome_a[,name_to_index]!="Not Available"),]
    ##empty df
    length_a <-length.POSIXlt(outcome_a)
    outcome_s <-data.frame(1:length_a)
    ##input new col to new df
    outcome_s<-cbind(outcome_s,outcome_a[,2])
    outcome_s<-cbind(outcome_s,outcome_a[,name_to_index])
    ##sort
    outcome_o <-outcome_s[order(outcome_a[, name_to_index]),]
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    outcome_o [1,2]
}



##首字母大写 from http://stat.ethz.ch/R-manual/R-patched/library/base/html/chartr.html
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}