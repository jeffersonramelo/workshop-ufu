install.packages("bibliometrix")
library(bibliometrix)

S <- convert2df("C:/Users/jeffe/Dropbox/PÓS DOC/1º worshop/scopus.csv", dbsource = "scopus", format = "csv")

View(S)

write.table(S,file = "x.csv", sep = ";")

biblioshiny()
