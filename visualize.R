#Dataset Analysis 
library(formattable)
source("lab4DataPrepNew.R")

how_many_records = function(dataset_math, dataset_por){
  how_many = data.frame(
    row.names = c("math", "portuguese"),
    "Total Records" = c(nrow(dataset_math), nrow(dataset_por))
  )
}

merge_datasets = function(dataset_math, dataset_por){
  d3=merge(dataset_math,dataset_por,
           by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
           ,suffixes=c("_math","_por"))
  not_the_same = d3[which(d3$guardian_math!=d3$guardian_por),c("guardian_math","guardian_por")]
}
visualise_data = function(dataset){
  NPREPROCESSING_prettyDataset(dataset)
  
}

skewed_vars = function(dataset){
  numeric_data = Filter(is.numeric,dataset)
  highly = c()
  for (n in 1:ncol(numeric_data)){
   
    if (moments::skewness(numeric_data[,n]) >= 1 || moments::skewness(numeric_data[,n]) <= -1){
      
      highly = append(highly,names(numeric_data)[n])
    }
  }
  
  #Plot box plot
  skewed = dataset[,which(names(dataset) %in% highly)]
  plots = boxplot(skewed, ylim=c(0,80))
  return (plots)
  
}

plot_two_categorical_bars = function(dataset, columns){
  library(ggplot2)
  col1 = as.factor(dataset[,columns[1]])
  col2 = as.factor(dataset[,columns[2]])
  ggplot(data=dataset)+geom_bar(aes(x=col1, fill=col2))+
  ggtitle(label = paste(columns[1],"and",columns[2]))+
  theme_bw()+labs(x=columns[1],fill=columns[2])
}

plot_multiple_categorical_bars = function(dataset, columns){
  library(miscset) # install from CRAN if required
  ncol = ifelse(length(columns)==2,2,3)
  print(columns)
  ggplotGrid(ncol = ncol,
  lapply(columns,
  function(col) {
  ggplot(dataset, aes_string(col)) + geom_bar(fill="red") + coord_flip()+theme_grey() 
  }))
}



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
