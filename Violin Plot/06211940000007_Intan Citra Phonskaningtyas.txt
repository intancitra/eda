#Setting Work Directory
setwd('E:/AED/Tugas M10')

#Input Data
data=read.table('E:/AED/Tugas M10/countries.csv',header=TRUE,sep=',')

#Install Library ggplot2
library(ggplot2)

#Memilah dan menggabungkan data MBTI tipe Analysts (INTJ, INTP, ENTP, ENTJ) dengan function stack
df1 = stack(data[,c(13,22,25,27)])

#Menamai kolom dari dataframe di atas
colnames(df1) = c("Distribusi", "MBTI")

#Membuat baseline violin plot dan menampilkannya
analysts = ggplot(df1,aes(x=factor(MBTI),y=Distribusi))+geom_violin(trim=FALSE)
analysts

#Mengisi warna dari violin plot dan menambah legend
analysts = ggplot(df1,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI)))+
  geom_violin(trim=FALSE)
analysts

#Mengubah nama axis
analysts = ggplot(df1,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI)))+
  geom_violin(trim=FALSE) + labs(x="Turbulent Analysts Personalities Types", y="Distribution")
analysts

#Menambahkan judul dan takarir
analysts = ggplot(df1,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI))) +
  geom_violin(trim=FALSE) + labs(x="Turbulent Analysts Personalities Types",
                                 y="Distribution",
                                 title="Distribution of Personality Types",
                                 subtitle="Run on Turbulent Analysts Types")
analysts

#Mengubah judul pada legend
analysts = ggplot(df1,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI))) +
  geom_violin(trim=FALSE) + labs(x="Turbulent Analysts Personalities Types",
                                 y="Distribution",
                                 title="Distribution of Personality Types",
                                 subtitle="Run on Turbulent Analysts Types",
                                 fill="Types")
analysts

#Menghilangkan kotak background
analysts = ggplot(df1,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI))) +
  geom_violin(trim=FALSE) + labs(x="Turbulent Analysts Personalities Types",
                                 y="Distribution",
                                 title="Distribution of Personality Types",
                                 subtitle="Run on Turbulent Analysts Types",
                                 fill="Types") +
  theme(rect=element_blank())
analysts

#Mengatur margin x dan y axis
analysts = ggplot(df1,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI))) +
  geom_violin(trim=FALSE) + labs(x="Turbulent Analysts Personalities Types",
                                 y="Distribution",
                                 title="Distribution of Personality Types",
                                 subtitle="Run on Turbulent Analysts Types",
                                 fill="Types") +
  theme(rect=element_blank(), axis.title.x = element_text(margin=margin(b=5, t=10), size=10),
        axis.title.y = element_text(margin=margin(l=5, r=10), size=10))
analysts

#Menebalkan Judul
analysts = ggplot(df1,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI))) +
  geom_violin(trim=FALSE) + labs(x="Turbulent Analysts Personalities Types",
                                 y="Distribution",
                                 title="Distribution of Personality Types",
                                 subtitle="Run on Turbulent Analysts Types",
                                 fill="Types") +
  theme(rect=element_blank(), axis.title.x = element_text(margin=margin(b=5, t=10), size=10),
        axis.title.y = element_text(margin=margin(l=5, r=10), size=10),
        plot.title = element_text(face='bold'))
analysts

#Menambahkan Line pada background
analysts = ggplot(df1,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI))) +
  geom_violin(trim=FALSE) + labs(x="Turbulent Analysts Personalities Types",
                                 y="Distribution",
                                 title="Distribution of Personality Types",
                                 subtitle="Run on Turbulent Analysts Types",
                                 fill="Types") +
  theme(rect=element_blank(), axis.title.x = element_text(margin=margin(b=5, t=10), size=10),
        axis.title.y = element_text(margin=margin(l=5, r=10), size=10),
        plot.title = element_text(face='bold'),
        panel.grid=element_line(color='purple'))
analysts

#Diplomats
df2 = stack(data[,c(4,6,14,30)])
colnames(df2) = c("Distribusi", "MBTI")

diplomats = ggplot(df2,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI))) +
  geom_violin(trim=FALSE) + labs(x="Turbulent Diplomats Personalities Types",
                                 y="Distribution",
                                 title="Distribution of Personality Types",
                                 subtitle="Run on Turbulent Diplomats Types",
                                 fill="Types") +
  theme(rect=element_blank(), axis.title.x = element_text(margin=margin(b=5, t=10), size=10),
        axis.title.y = element_text(margin=margin(l=5, r=10), size=10),
        plot.title = element_text(face='bold'),
        panel.grid=element_line(color='green'))
diplomats

#Sentinels
df3 = stack(data[,c(5,8,9,18)])
colnames(df3) = c("Distribusi", "MBTI")

sentinels = ggplot(df3,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI))) +
  geom_violin(trim=FALSE) + labs(x="Turbulent Sentinels Personalities Types",
                                 y="Distribution",
                                 title="Distribution of Personality Types",
                                 subtitle="Run on Turbulent Sentinels Types",
                                 fill="Types") +
  theme(rect=element_blank(), axis.title.x = element_text(margin=margin(b=5, t=10), size=10),
        axis.title.y = element_text(margin=margin(l=5, r=10), size=10),
        plot.title = element_text(face='bold'),
        panel.grid=element_line(color='blue'))
sentinels

#Explorers
df4 = stack(data[,c(15,17,19,26)])
colnames(df4) = c("Distribusi", "MBTI")

explorers = ggplot(df4,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI))) +
  geom_violin(trim=FALSE) + labs(x="Turbulent Explorers Personalities Types",
                                 y="Distribution",
                                 title="Distribution of Personality Types",
                                 subtitle="Run on Turbulent Explorers Types",
                                 fill="Types") +
  theme(rect=element_blank(), axis.title.x = element_text(margin=margin(b=5, t=10), size=10),
        axis.title.y = element_text(margin=margin(l=5, r=10), size=10),
        plot.title = element_text(face='bold'),
        panel.grid=element_line(color='yellow'))
explorers

#All Turbulent Personalities
all=rbind(df1,df2,df3,df4)

allmbti = ggplot(all,aes(x=factor(MBTI),y=Distribusi,fill=factor(MBTI))) +
  geom_violin(trim=FALSE) + labs(x="Turbulent Personalities Types",
                                 y="Distribution",
                                 title="Distribution of Personality Types",
                                 subtitle="Run on All Turbulent Types",
                                 fill="Types") +
  theme(rect=element_blank(), axis.title.x = element_text(margin=margin(b=5, t=10), size=10),
        axis.title.y = element_text(margin=margin(l=5, r=10), size=10),
        plot.title = element_text(face='bold'),
        panel.grid=element_line(color='grey'))
allmbti
