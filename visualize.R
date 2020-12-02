# ************************************************
# COM3018 PRACTICAL BUSINESS ANALYTICS ASSESSMENT
# Visualize Dataset 
# Based on real-world dataset
#
# DATE: 1st December 2020
# VERSION: v1.00
# AUTHOR: Tarek Elkelani
#
# UPDATE
# 1.00 01/12/2020 Initial Version
#
# ************************************************


#Dataset Analysis 
library(formattable)
source("lab4DataPrepNew.R")
# ************************************************
# how_many_records():
# Displays the amount of records in a table for both datasets
#
# INPUT : dataframe - dataset_math - Math dataset
#         dataframe - dataset_por - Portuguese Dataset
#
# OUTPUT : No return value - Views amount of records as table
# ************************************************
how_many_records = function(dataset_math, dataset_por){
  how_many = data.frame(
    row.names = c("math", "portuguese"),
    "Total Records" = c(nrow(dataset_math), nrow(dataset_por))
  )
}
# ************************************************
# merge_datasets()
# Put both datasets together
# INPUT : dataframe - dataset_math - Math dataset
#         dataframe - dataset_por - Portuguese dataset
#
# OUTPUT : No return value
# ************************************************
merge_datasets = function(dataset_math, dataset_por){
  d3=merge(dataset_math,dataset_por,
           by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
           ,suffixes=c("_math","_por"))
  not_the_same = d3[which(d3$guardian_math!=d3$guardian_por),c("guardian_math","guardian_por")]
}
# ************************************************
# Visualise_data()
# call the prettyDataset function from lab 3
# INPUT  : dataframe - dataset - the dataset to be displayed
# 
# OUTPUT : No return values 
# ************************************************
visualise_data = function(dataset){
  NPREPROCESSING_prettyDataset(dataset)
  
}
# ************************************************
# skewed_vars()
# Display the box plots of numeric variables with skeweness of
# =>1 or =<-1
#
# INPUT  : dataframe - dataset - dataset to analyze
# 
# OUTPUT : graphics - plots - Returns the plot 
# ************************************************
skewed_vars = function(dataset){
  numeric_data = Filter(is.numeric,dataset)
  highly = c()
  for (n in 1:ncol(numeric_data)){
    #-1 and 1 are indicators that a datapoint is highly skewed
    if (moments::skewness(numeric_data[,n]) >= 1 || moments::skewness(numeric_data[,n]) <= -1){
      
      highly = append(highly,names(numeric_data)[n])
    }
  }
  
  #Plot box plot
  skewed = dataset[,which(names(dataset) %in% highly)]
  plots = boxplot(skewed, ylim=c(0,80))
  return (plots)
  
}
# ************************************************
# plot_two_categorical_bars()
# Plot two categorical variables where one of the variables 
# is plotted inside the bar 
# 
# INPUT : dataframe - dataset - The dataset to be used
#       : vector - columns - The categorical column names 
#
# OUTPUT : No return values - plots the bar chart using ggplot
# ************************************************
plot_two_categorical_bars = function(dataset, columns){
  library(ggplot2)
  col1 = as.factor(dataset[,columns[1]])
  col2 = as.factor(dataset[,columns[2]])
  #aes is what the x column will be, fill is the variable that fills the original bars
  # Instead of for example, red, it will be filled with the values of col2
  ggplot(data=dataset)+geom_bar(aes(x=col1, fill=col2))+
  ggtitle(label = paste(columns[1],"and",columns[2]))+
  theme_bw()+labs(x=columns[1],fill=columns[2])
  #labs = labels
  #ggtitle = title of visualization
}

# ************************************************
# plot_multiple_categorical_bars()
# Plot multiple bars of categorical variables together on the same plot
# 
# INPUT : dataframe - dataset - The dataset to be used
#       : vector - columns - name of categorical columns
# ************************************************
plot_multiple_categorical_bars = function(dataset, columns){
  library(miscset) 
  ncol = ifelse(length(columns)==2,2,3)
  # If the amount of columns to plot are 2, make the width 2, else make it 3
  ggplotGrid(ncol = ncol,
  lapply(columns,
  function(col) {
  ggplot(dataset, aes_string(col)) + geom_bar(fill="red") + coord_flip()+theme_grey() 
  })) #lapply applies a function over a vector
      #basically the ggplot function is being applied to each column in the vector
      #aes_string is the variable bar will plot, geom_bar is the type of chart, coord_flip() makes it horizontal
      #theme_grey() is just a  color scheme
}


main = function(){
  dataset_math = read.csv("student-mat.csv", sep=";")
  visualise_data(dataset_math)
  dataset_por = read.csv("student-por.csv", sep=";")
  visualise_data(dataset_por)
  how_many_records(dataset_math,dataset_por)
  merge_datasets(dataset_math, dataset_por)
  
  
  
  skewed_math = skewed_vars(dataset_math)
  skewed_por = skewed_vars(dataset_por)
  
  plot_school_and_address = plot_two_categorical_bars(dataset_por,c("school","address"))
  print(plot_school_and_address)
  
  plot_school_and_sex = plot_two_categorical_bars(dataset_por,c("school","sex"))
  print(plot_school_and_sex)
  
  plot_school_and_schoolsup = plot_two_categorical_bars(dataset_math,c("school","schoolsup"))
  print(plot_school_and_schoolsup)
  plot_family_attributes = plot_multiple_categorical_bars(dataset_por,
                                                          c("Pstatus","famsup","guardian","Mjob","Fjob","famsize")
  )
  plot_education_attributes = plot_multiple_categorical_bars(dataset_por,
                                                             c("paid","higher","activities","nursery","reason"))
  plot_personal_attributes = plot_multiple_categorical_bars(dataset_por,
                                                            c("internet", "romantic"))
}

