packages = c('rcompanion', 'vcd', 'psych', 'DescTools', 'epitools', 'reshape2', 'dplyr', 'maditr')
lapply(packages, library, character.only = TRUE)

setwd('E:/AED')

data=read.table('E:/AED/kuisAED.csv', sep=',', header=TRUE)
summary(data)
data2 <- within(data, {
  id <- factor(id)
  race <- factor(race)
  ses  <- factor(ses)
  schtyp <- factor(schtyp)
  gender <- factor(gender)
  prog <- factor(prog)
}) 
summary(data2)

#Ses & Schtype
df_coba= data2 %>%
  group_by(ses,schtyp) %>%
  summarise(jumlah = n())

df_coba = dcast(df_coba, ses ~ schtyp, value.var='jumlah')
df_coba

data_matrix = data.matrix(df_coba[ ,2:3])
data_matrix

cramerV(data_matrix, digits = 4)

assocstats(data_matrix)

data_hist = matrix(rbind(df_coba$private, df_coba$public), nrow = 3)
rownames(data_hist) <- c('High', 'Low', 'Middle')
colnames(data_hist) <- c('Private', 'Public')
data_hist

data_hist2 = t(data_hist)

barplot(data_hist,
        main = 'Grouped Barplot SES & School Type',
        col=c('#e63946','#a8dadc','#1d3557'), 
        border="white",
        xlab="Type of School",
        ylab="Social Economic Status",
        beside = T)
legend('topleft',
       legend = rownames(data_hist),
       fill=c('#e63946','#a8dadc','#1d3557')
       )

#read & gender
boxplot(data2$read~data2$gender, 
        main=expression(bold("Reading Score Gender-based")), 
        xlab="", ylab="",
        frame.plot=F,
        xaxt='n',
        col=c("#003566","#ffc300"),
        col.axis='#001d3d', cex.main=2)
legend('bottomleft', legend=c("Female", "Male"), fill=c("#003566","#ffc300"))

#write & ras
boxplot(data2$write~data2$race, 
        main=expression(bold("Writing Score Race-based")), 
        xlab="", ylab="",
        frame.plot=F,
        xaxt='n',
        col=c("#003566","#ffc300",'#219ebc','#fb8500'),
        col.axis='#001d3d', cex.main=2)
legend('topleft',inset=0.005, legend=c("African-America", "Asian", "Hispanic", "White"), 
       cex=0.5,fill=c("#003566","#ffc300"))

#Prog & Gender
df2= data2 %>%
  group_by(prog,gender) %>%
  summarise(jumlah = n())

df2 = dcast(df2, prog ~ gender, value.var='jumlah')
df2

data_matrix2 = data.matrix(df2[ ,2:3])
data_matrix2

cramerV(data_matrix, digits = 4)

assocstats(data_matrix)

data_hist3 = matrix(rbind(df2$female, df2$male), nrow = 3)
rownames(data_hist3) <- c('Academic', 'General', 'Vocational')
colnames(data_hist3) <- c('Female', 'Male')
data_hist3

barplot(data_hist3,
        main = 'Stacked Barplot Program & Gender',
        col=c('#e63946','#a8dadc','#1d3557'), 
        border="white",
        xlab="Gender",
        ylab="Program")

legend('topright',
       legend = rownames(data_hist3),
       fill=c('#e63946','#a8dadc','#1d3557')
)