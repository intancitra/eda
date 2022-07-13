#Melihat dan mengatur work directory
getwd()
setwd('E:/Tugas 11 Maret')

#Import tabel
table = read.table('most_followed_ig.csv', header=TRUE, sep=',', quote='"')

#Mengubah nama kolom tabel dan melihat hasil perubahannya
colnames(table)=c('PERINGKAT','AKUN','KATEGORI 1','KATEGORI 2',
                  'PENGIKUT','ER','iPOST ON HASHTAG','MEDIA TERUNGGAH')
colnames(table)
head(table)

#Mengeksport dataframe ke csv
write.csv(table,'06211940000007_INTAN CITRA PHONSKANINGTYAS_AEDC_R.csv')
