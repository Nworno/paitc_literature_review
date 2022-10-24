library(stringr)
library(tidyr)
library(readr)
library(dplyr)


data = read_csv("data/extraction/results_dm.csv")

data$Significant = "NA"


for(i in 1:nrow(data)) {
  if(substr(data$questions[i],1,10) == "p-value fo"){

    if(substr(data$answer[i],1,1) != "[" & substr(data$answer[i],1,1) != "<"){
      if(data$answer[i] < 0.05){
        data$Significant[i] = "Yes"
      }else if(data$answer[i] >= 0.05){
        data$Significant[i] = "No"
      }else{
        data$Significant[i] = "error"
      }
    }else if(substr(data$answer[i],1,1) == "<"){
      pvalue = parse_number(data$answer[i])
      if(pvalue <= 0.05){
        data$Significant[i] = "Yes"
      }else if(pvalue > 0.05){
        data$Significant[i] = "No"
      }else{
        data$Significant[i] = "error1"
      }
    }else if(substr(data$answer[i],1,1) == "["){
      min = parse_number(data$answer[i])
      max = parse_number(gsub(toString(min),"",data$answer[i]))
      if(max == 0)
      {
        max = parse_number(gsub(paste(toString(min),"0", sep = ""),"",data$answer[i]))
      }
      if(min %% 1 == 0)
      {
        max = parse_number(gsub(paste(toString(min),".0", sep = ""),"",data$answer[i]))
      }

      type = dplyr::filter(data, doi == data$doi[i] & n_itc == data$n_itc[i] &
                             questions == "Primary outcome: treatment effect contrast" ) %>%
        select("answer")

      centre  = 0

      if(type== "OR" | type== "RR" | type== "HR"| type== "Means Ratio" |
         type== "Incidence Rate Ratio")
      {
        centre  = 1
      }else if(type == "Means difference" | type == "Risk difference" |
               type == "Rate difference" | type == "Proportions difference" |
               type == "Median difference")
      {
        centre = 0
      }else
      {
        print(i)
        print("error 3")
      }


      if((min < centre & max < centre) | (min > centre & max > centre))
      {
        data$Significant[i] = "Yes"
      }else
      {
        data$Significant[i] = "No"
      }
    }else{
      data$Significant[i] = "error2"

    }
  }else{
    data$Significant[i] = ""
  }
}

