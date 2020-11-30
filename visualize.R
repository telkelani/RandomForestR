#Dataset Analysis 
library(formattable)
source("lab4DataPrepNew.R")

how_many_records = function(dataset_math, dataset_por){
  how_many = data.frame(
    row.names = c("math", "portuguese"),
    "Total Records" = c(nrow(dataset_math), nrow(dataset_por))
  )
  View(how_many, title="How many records")
}

merge_datasets = function(dataset_math, dataset_por){
  d3=merge(dataset_math,dataset_por,
           by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
           ,suffixes=c("_math","_por"))
  not_the_same = d3[which(d3$guardian_math!=d3$guardian_por),c("guardian_math","guardian_por")]
  View(not_the_same, title="Not the same guardian")
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

plot_categorical_bars = function(dataset){
  
}



dataset_math = read.csv("student-mat.csv", sep=";")
visualise_data(dataset_math)
dataset_por = read.csv("student-por.csv", sep=";")
visualise_data(dataset_por)
how_many_records(dataset_math,dataset_por)
merge_datasets(dataset_math, dataset_por)

skewed_math = skewed_vars(dataset_math)

sex = as.factor(dataset_math$sex)
address = as.factor(dataset_math$address)
# library(ggplot2)
# ggplot(data=dataset_math)+geom_bar(aes(x=school, fill=address))+
#   ggtitle(label = "School and Address")+
#   theme_bw()



library(miscset) # install from CRAN if required
ggplotGrid(ncol = 2,
lapply(c("internet","romantic"),
function(col) {
ggplot(dataset_math, aes_string(col)) + geom_bar(fill="red") + coord_flip()+theme_grey() 
}))


