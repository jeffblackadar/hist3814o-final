#file is in the same directory 
#It has lines with 2 patterns and lengths:
#[1] "http://collections.banq.qc.ca:8008/jrn03/equity/src/2010/08/25/"
#[1] "http://collections.banq.qc.ca:8008/jrn03/equity/src/2010/08/25/01/"


sigDateStringProvElection=c("1886-10-14","1890-06-17","1892-03-08","1897-05-11","1900-12-07","1904-11-25","1908-06-08","1912-05-15","1916-05-22","1919-06-23","1923-02-05","1927-05-16","1931-08-24","1935-11-25","1936-08-17","1939-10-25","1944-08-08","1948-07-28","1952-07-16","1956-06-20","1960-06-22","1962-11-14","1966-06-05","1970-04-29","1981-04-13","1985-12-02","1989-09-25","1994-09-12","1998-11-30","2003-04-14","2007-03-26","2008-12-08")
sigDateProvElection=as.Date(sigDateStringProvElection,"%Y-%m-%d")

#openconnecton to read file
inputFile <- "equityurls.txt"
inputCon  <- file(inputFile, open = "r")

#open connecton to wite output file
outputFileCsv <- "equityeditions.csv"
outputCsvCon<-file(outputFileCsv, open = "w")

#open connecton to wite output file
outputFileHtml <- "equityeditions.html"
outputFileHtmlCon<-file(outputFileHtml, open = "w")

#print column headers at top of csv
writeLines('base url,year,month,day,weekday', outputCsvCon)

#set up web page at top of html
writeLines('<html><head><title></title></head><body><h1>Editions of the Shawville Equity.</h1><table>', outputFileHtmlCon)


while (length(urlLine <- readLines(inputCon, n = 1, warn = FALSE)) > 0) {
    urlLineElements = strsplit(urlLine, "/")
    #make sure it is a complete line
    if(length(urlLineElements[[1]])>8){
    
    
    #Note the %Y indicates a 4 year date
    dateOfEdition<-as.Date(paste(urlLineElements[[1]][7],'/',urlLineElements[[1]][8],'/',urlLineElements[[1]][9],sep=""),"%Y/%m/%d")
    

    sigDateProvElection
    #Credit to http://statmethods.net/input/dates.html
    #print day of the week
    #print(format(dateOfEdition,format="%a"))
    outputLine<-paste(urlLine,',',urlLineElements[[1]][7],',',urlLineElements[[1]][8],',',urlLineElements[[1]][9],',',format(dateOfEdition,format="%a"),sep="")
    
    
    
    print (outputLine)
    writeLines(outputLine, outputCsvCon)
    
    #html
    writeLines('<tr>', outputFileHtmlCon)
    
    
    
    #set file name, make it look like this: 83471_1920-06-10.pdf
    if (length(urlLineElements[[1]])==10){
      #editions with dates like these (/2010/08/25/01/) are supplements  
      writeLines(paste('<td><a href="',urlLine,'">',format(dateOfEdition,format='%Y/%m/%d'),' (sup.)','</a></td>',sep=""),outputFileHtmlCon)
      editionFileName<-paste('83471_',urlLineElements[[1]][7],'-',urlLineElements[[1]][8],'-',urlLineElements[[1]][9],'-',urlLineElements[[1]][10],sep='')  
    } else {
      writeLines(paste('<td><a href="',urlLine,'">',format(dateOfEdition,format='%Y/%m/%d'),'</a></td>',sep=""),outputFileHtmlCon)
      editionFileName<-paste('83471_',urlLineElements[[1]][7],'-',urlLineElements[[1]][8],'-',urlLineElements[[1]][9],sep='')        
    }

    writeLines(paste('<td><a href="',urlLine,editionFileName,'.pdf">.pdf</a></td>',sep=""),outputFileHtmlCon)
    writeLines(paste('<td><a href="',urlLine,editionFileName,'.txt">.txt</a></td>',sep=""),outputFileHtmlCon)
    
    #See if the edition matches significant dates.  There must be a more efficient way to do this.
    for(counter in 1:length(sigDateProvElection)){
      if(dateOfEdition-sigDateProvElection[counter]<7){
        writeLines(paste('<td>P.E. ',sigDateProvElection[counter],'</td>',sep=""),outputFileHtmlCon)
      }else{
        writeLines(paste('<td></td>',sep=""),outputFileHtmlCon)
      }
    }
    
    writeLines('</tr>', outputFileHtmlCon)
    }
} 

writeLines('</table></body></html>', outputFileHtmlCon)

close(outputFileHtmlCon)
close(inputCon)
close(outputCsvCon)




