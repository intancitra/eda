setwd('E:/AED')

data=read.table('E:/AED/bestsellers with categories.csv',header=TRUE,sep=',',quote='"')

data_2019=data[data$Year==2019,]

windowsFonts(A = windowsFont("Metropolis")) 

boxplot(data_2019$User.Rating~data_2019$Genre, 
        main=expression(bold("2019 User Rating Genre-Based")), 
        xlab="", ylab="",
        frame.plot=F,
        col=c("#003566","#ffc300"),
        col.axis='#001d3d',col.main='#e71d36', cex.main=2, family="A")

title(xlab=expression(bold("Books Genre")), ylab=expression(bold("User Rating")), line=2.5, col.lab="#001d3d", family="A")

