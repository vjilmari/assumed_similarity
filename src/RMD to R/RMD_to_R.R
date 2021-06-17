#Extract script from all R markdown documents and save to same folder

library(knitr)

Rmd.files<-
  list.files(pattern = ".Rmd", recursive = TRUE)

#loop through the files
for (i in 1:length(Rmd.files)){
  purl(Rmd.files[i],substr(Rmd.files[i],1,nchar(Rmd.files[i])-2))
}
