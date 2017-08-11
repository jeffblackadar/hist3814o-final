#file is in the same directory 
#It has lines with 2 patterns and lengths:
#[1] "http://collections.banq.qc.ca:8008/jrn03/equity/src/2010/08/25/"
#[1] "http://collections.banq.qc.ca:8008/jrn03/equity/src/2010/08/25/01/"

#function to print in a cell if a significant event is in that edition.
editionWithinWeekOfDate <- function(editionDate, significantDates, note) {
  
  thisCell<-paste('<td></td>',sep="")
  thisCsValue<-","
  for(counter in 1:length(significantDates)){
    dateDiff<-difftime(editionDate,significantDates[counter], units="days")
    dateDiffDays<-as.numeric(dateDiff, units="days")
    #print(dateOfEdition)
    #print(sigDateProvElection[counter])
    #print(dateDiffDays) 
    
    if(dateDiffDays<7 & dateDiffDays>-1){
      #This edition likely contains coverage on the event that happened during the previous week
      thisCell<-paste('<td>',note,' ',significantDates[counter],'</td>',sep="")
      thisCsValue<-paste(',',note,' ',significantDates[counter],sep="")
      #Copy just these files so we can work with them with Mallet
      fileCopyFrom<-paste("c:\\a_orgs\\carleton\\hist3814\\equity\\",editionFileName,".txt",sep="")
      fileCopyTo<-paste("c:\\a_orgs\\carleton\\hist3814\\equity_subcollections\\",note,"\\",editionFileName,".txt",sep="")
      file.copy(fileCopyFrom, fileCopyTo, overwrite = FALSE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
      #no need to keep comparing after a match
      counter=length(sigDateProvElection)
    }
  }
  
  result <- c(thisCell,thisCsValue)
  return(result)
}

# Set working directory
dir <- "C:\\a_orgs\\carleton\\hist3814\\R\\hist3814o-final\\" # adjust to suit
setwd(dir)

#load dates of significant events
# sourced from: https://lop.parl.ca/About/Parliament/FederalRidingsHistory/hfer.asp?Language=E&Search=G
sigDateStringFedElection=c("1887-02-22","1891-03-05","1896-06-23","1900-11-07","1904-11-03","1908-10-26","1911-09-21","1917-12-17","1921-12-06","1925-10-29","1926-09-14","1930-07-28","1935-10-14","1940-03-26","1945-06-11","1949-06-27","1953-08-10","1957-06-10","1958-03-31","1962-06-18","1963-04-08","1965-11-08","1968-06-25","1972-10-30","1974-07-08","1979-05-22","1980-02-18","1984-09-04","1988-11-21","1993-10-25","1997-06-02","2000-11-27","2004-06-28","2006-01-23","2008-10-14")
sigDateFedElection=as.Date(sigDateStringFedElection,"%Y-%m-%d")

# sourced from: http://www.quebecpolitique.com/elections-et-referendums/circonscriptions/elections-dans-pontiac/#2008
sigDateStringProvElection=c("1886-10-14","1890-06-17","1892-03-08","1897-05-11","1900-12-07","1904-11-25","1908-06-08","1912-05-15","1916-05-22","1919-06-23","1923-02-05","1927-05-16","1931-08-24","1935-11-25","1936-08-17","1939-10-25","1944-08-08","1948-07-28","1952-07-16","1956-06-20","1960-06-22","1962-11-14","1966-06-05","1970-04-29","1981-04-13","1985-12-02","1989-09-25","1994-09-12","1998-11-30","2003-04-14","2007-03-26","2008-12-08")
sigDateProvElection=as.Date(sigDateStringProvElection,"%Y-%m-%d")
sigDateStringProvReferendum=c("1919-04-10","1980-05-20","1992-10-26","1995-10-30")
sigDateProvReferendum=as.Date(sigDateStringProvReferendum,"%Y-%m-%d")

# sourced from: Wikipedia.  Cross referenced with Shawville Equity
sigDateStringMunElection=c("1983-11-06","1985-11-03","1986-11-09","1987-11-01","1989-11-05","1990-11-04","1991-11-02","1993-11-07","1994-11-06","1995-11-05","1997-11-02","1998-11-01","2001-11-04","2002-11-03","2003-11-02","2005-11-06","2006-11-05","2009-11-01")
sigDateMunElection=as.Date(sigDateStringMunElection,"%Y-%m-%d")

#openconnecton to read file
inputFile <- "equityurls.txt"
#using a shorter test file
#inputFile <- "equityurls_test.txt"
inputCon  <- file(inputFile, open = "r")

#open connecton to wite output file
outputFileCsv <- "equityeditions.csv"
outputFileCsvCon<-file(outputFileCsv, open = "w")

#open connecton to wite output file
outputFileHtml <- "equityeditions.html"
outputFileHtmlCon<-file(outputFileHtml, open = "w")

#print column headers at top of csv
writeLines('base url,year,month,day,weekday,Federal Election, Provincial Election, Provincial Referendum, Municipal Election', outputFileCsvCon)

#set up web page at top of html
writeLines('<html><head><title></title></head><body><h1>Editions of the Shawville Equity.</h1><table>', outputFileHtmlCon)
writeLines('<tr><th>Date of<br> edition</th><th>.pdf</th><th>.txt</th><th>Federal<br>Election</th><th>Provincial<br>Election</th><th>Provincial<br>Referendum</th><th>Municipal<br>Election</th></tr>', outputFileHtmlCon)



while (length(urlLine <- readLines(inputCon, n = 1, warn = FALSE)) > 0) {
    urlLineElements = strsplit(urlLine, "/")
    #make sure it is a complete line
    if(length(urlLineElements[[1]])>8){

    #Note the %Y indicates a 4 year date
    dateOfEdition<-as.Date(paste(urlLineElements[[1]][7],'/',urlLineElements[[1]][8],'/',urlLineElements[[1]][9],sep=""),"%Y/%m/%d")
    print(dateOfEdition)

    sigDateProvElection
    #Credit to http://statmethods.net/input/dates.html
    #print day of the week
    #print(format(dateOfEdition,format="%a"))
    outputLine<-paste(urlLine,',',urlLineElements[[1]][7],',',urlLineElements[[1]][8],',',urlLineElements[[1]][9],',',format(dateOfEdition,format="%a"),sep="")
    #print (outputLine)
    
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
    thisDateColumn<-editionWithinWeekOfDate(dateOfEdition,sigDateFedElection, "Fed El")
    writeLines(thisDateColumn[1],outputFileHtmlCon)
    FedEl<-thisDateColumn[2]
    thisDateColumn<-editionWithinWeekOfDate(dateOfEdition,sigDateProvElection, "Prov El")
    writeLines(thisDateColumn[1],outputFileHtmlCon)
    ProvEl<-thisDateColumn[2]
    
    thisDateColumn<-editionWithinWeekOfDate(dateOfEdition,sigDateProvReferendum, "Prov Ref")
    writeLines(thisDateColumn[1],outputFileHtmlCon)
    ProvRef<-thisDateColumn[2]
    thisDateColumn<-editionWithinWeekOfDate(dateOfEdition,sigDateMunElection, "Mun El")
    writeLines(thisDateColumn[1],outputFileHtmlCon)
    MunEl<-thisDateColumn[2]
    
    writeLines(paste(outputLine,FedEl,ProvEl,ProvRef,MunEl,sep=""),outputFileCsvCon)
    writeLines('</tr>', outputFileHtmlCon)
    }
} 
#make sure not to run these lines until the above loop is done, it will close the connection and the files will be open
writeLines('</table></body></html>', outputFileHtmlCon)
close(inputCon)
close(outputFileHtmlCon)
close(outputFileCsvCon)








