#install.packages("stringdist")
library(stringdist)


Cleaning = function(filename){
  


text = readLines(filename)
text = paste(text, collapse = " ")
text=gsub(".*</SEC-HEADER>", "", text)
text=gsub(".*</IMS-HEADER>", "", text)
text=gsub("-----END PRIVACY-ENHANCED MESSAGE-----", "", text)
text=gsub("<XBLR>.*</XBLR>", "", text)
text=gsub("<TYPE>GRAPHIC.*</TEXT>", "", text)
text=gsub("<TYPE>ZIP.*</TEXT>", "", text)
text=gsub("<TYPE>EXCEL.*</TEXT>", "", text)
text=gsub("<TYPE>PDF.*</TEXT>", "", text)
text=gsub("<TABLE>.*</TABLE>", "", text)


doc = XML::htmlParse(text, asText = TRUE)
text = XML::xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", 
                        XML::xmlValue)
text = paste(text, collapse = " ")
text = tm::Corpus(tm::VectorSource(text))
cleantext = function(data.text.corpus) {
  data.text.corpus = tm::tm_map(data.text.corpus, 
                                tm::removePunctuation)
  data.text.corpus = tm::tm_map(data.text.corpus, 
                                tm::removeNumbers)
  data.text.corpus = tm::tm_map(data.text.corpus, 
                                tm::stripWhitespace)
  data.text.corpus = tm::tm_map(data.text.corpus, 
                                function(x) tm::removeWords(x, tm::stopwords()))
  data.text.corpus = tm::tm_map(data.text.corpus, 
                                tm::content_transformer(tolower))
  return(data.text.corpus)
}
text = cleantext(text)

test=text[[1]]$content

word.frq <- tm::termFreq(text[[1]])
wordMatrix = as.data.frame((as.matrix(word.frq)))

#sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\outfile.txt")
#test
#sink()

test1=gsub("[à]", "", test)
test1=gsub("[á]", "", test1)
test1=gsub("[â]", "", test1)
test1=gsub("[ã]", "", test1)
test1=gsub("[ä]", "", test1)
test1=gsub("[å]", "", test1)
test1=gsub("[æ]", "", test1)
test1=gsub("[ç]", "", test1)
test1=gsub("[è]", "", test1)
test1=gsub("[é]", "", test1)
test1=gsub("[ê]", "", test1)
test1=gsub("[ë]", "", test1)
test1=gsub("[ì]", "", test1)
test1=gsub("[í]", "", test1)
test1=gsub("[î]", "", test1)
test1=gsub("[ï]", "", test1)
test1=gsub("[ð]", "", test1)
test1=gsub("[ñ]", "", test1)
test1=gsub("[ò]", "", test1)
test1=gsub("[ó]", "", test1)
test1=gsub("[ô]", "", test1)
test1=gsub("[õ]", "", test1)
test1=gsub("[ö]", "", test1)
test1=gsub("[×]", "", test1)
test1=gsub("[ø]", "", test1)
test1=gsub("[ù]", "", test1)
test1=gsub("[ú]", "", test1)
test1=gsub("[û]", "", test1)
test1=gsub("[ü]", "", test1)
test1=gsub("[ý]", "", test1)
test1=gsub("[þ]", "", test1)
test1=gsub("[ß]", "", test1)
test1=gsub("[÷]", "", test1)
test1=gsub("[ÿ]", "", test1)
test1=gsub("[???]", "", test1)
test1=gsub("[,]", "", test1)
test1=gsub("[f]", "", test1)
test1=gsub("["]", "", test1)
test1=gsub("[.]", "", test1)
test1=gsub("[???]", "", test1)
test1=gsub("[???]", "", test1)
test1=gsub("[^]", "", test1)
test1=gsub("[???]", "", test1)
test1=gsub("[s]", "", test1)
test1=gsub("[<]", "", test1)
test1=gsub("[o]", "", test1)
test1=gsub("[z]", "", test1)
test1=gsub("[']", "", test1)
test1=gsub("[']", "", test1)
test1=gsub("["]", "", test1)
test1=gsub("["]", "", test1)
test1=gsub("[.]", "", test1)
test1=gsub("[-]", "", test1)
test1=gsub("[-]", "", test1)
test1=gsub("[~]", "", test1)
test1=gsub("[T]", "", test1)
test1=gsub("[>]", "", test1)
test1=gsub("[¡]", "", test1)
test1=gsub("[¢]", "", test1)
test1=gsub("[£]", "", test1)
test1=gsub("[¤]", "", test1)
test1=gsub("[¥]", "", test1)
test1=gsub("[¦]", "", test1)
test1=gsub("[§]", "", test1)
test1=gsub("[¨]", "", test1)
test1=gsub("[©]", "", test1)
test1=gsub("[ª]", "", test1)
test1=gsub("[«]", "", test1)
test1=gsub("[¬]", "", test1)
test1=gsub("[?]", "", test1)
test1=gsub("[®]", "", test1)
test1=gsub("[¯]", "", test1)
test1=gsub("[°]", "", test1)
test1=gsub("[±]", "", test1)
test1=gsub("[²]", "", test1)
test1=gsub("[³]", "", test1)
test1=gsub("[´]", "", test1)
test1=gsub("[µ]", "", test1)
test1=gsub("[¶]", "", test1)
test1=gsub("[·]", "", test1)
test1=gsub("[¸]", "", test1)
test1=gsub("[¹]", "", test1)
test1=gsub("[º]", "", test1)
test1=gsub("[»]", "", test1)
test1=gsub("[¼]", "", test1)
test1=gsub("[½]", "", test1)
test1=gsub("[¾]", "", test1)
test1=gsub("[¿]", "", test1)
test1=gsub("1", "", test1)
test1=gsub("\\[", "", test1)
test1=gsub("\\]", "", test1)

test1=gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", test1, perl=TRUE)

test1=as.character(test1)

return(test1)


}


text97 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_1997\\1002047_10-K_1997-07-23.txt")
text98 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_1998\\1002047_10-K_1998-07-22.txt")
text99 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_1999\\1002047_10-K_1999-07-12.txt")
text00 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2000\\1002047_10-K_2000-07-12.txt")
text01 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2001\\1002047_10-K_2001-07-26.txt")
text02 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2002\\1002047_10-K_2002-06-28.txt")
text03 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2003\\1002047_10-K_2003-06-25.txt")
text04 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2004\\1002047_10-K_2004-06-29.txt")
text05 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2005\\1002047_10-K_2005-07-08.txt")
text06 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2006\\1002047_10-K_2006-07-12.txt")
text07 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2007\\1002047_10-K_2007-06-26.txt")
text08 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2008\\1002047_10-K_2008-06-24.txt")
text09 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2009\\1002047_10-K_2009-06-17.txt")
text10 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2010\\1002047_10-K_2010-06-18.txt")
text11 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2011\\1002047_10-K_2011-06-23.txt")
text12 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2012\\1002047_10-K_2012-06-19.txt")
text13 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2013\\1002047_10-K_2013-06-17.txt")
text14 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2014\\1002047_10-K_2014-06-17.txt")
text15 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2015\\1002047_10-K_2015-06-12.txt")
text16 = Cleaning("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\Edgar filings\\1002047_10-K_2016\\1002047_10-K_2016-06-22.txt")








Cos97_98=stringdist(text97,text98,method="cosine")
Cos98_99=stringdist(text98,text99,method="cosine")
Cos99_00=stringdist(text99,text00,method="cosine")
Cos00_01=stringdist(text00,text01,method="cosine")
Cos01_02=stringdist(text01,text02,method="cosine")
Cos02_03=stringdist(text02,text03,method="cosine")
Cos03_04=stringdist(text03,text04,method="cosine")
Cos04_05=stringdist(text04,text05,method="cosine")
Cos05_06=stringdist(text05,text06,method="cosine")
Cos06_07=stringdist(text06,text07,method="cosine")
Cos07_08=stringdist(text07,text08,method="cosine")
Cos08_09=stringdist(text08,text09,method="cosine")
Cos09_10=stringdist(text09,text10,method="cosine")
Cos10_11=stringdist(text10,text11,method="cosine")
Cos11_12=stringdist(text11,text12,method="cosine")
Cos12_13=stringdist(text12,text13,method="cosine")
Cos13_14=stringdist(text13,text14,method="cosine")
Cos14_15=stringdist(text14,text15,method="cosine")
Cos15_16=stringdist(text15,text16,method="cosine")








sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\97.txt")
text97
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\98.txt")
text98
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\99.txt")
text99
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\00.txt")
text00
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\01.txt")
text01
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\02.txt")
text02
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\03.txt")
text03
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\04.txt")
text04
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\05.txt")
text05
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\06.txt")
text06
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\07.txt")
text07
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\08.txt")
text08
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\09.txt")
text09
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\10.txt")
text10
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\11.txt")
text11
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\12.txt")
text12
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\13.txt")
text13
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\14.txt")
text14
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\15.txt")
text15
sink()
sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\16.txt")
text16
sink()




year=c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)#,2015,2016)

Cosine=c(Cos97_98,Cos98_99,Cos99_00,Cos00_01,Cos01_02,Cos02_03,Cos03_04,Cos04_05,Cos05_06,Cos06_07,Cos07_08,Cos08_09,Cos09_10,Cos10_11,Cos11_12,Cos12_13,Cos13_14)#,Cos14_15,Cos15_16)

plot(year,Cosine,xlab="Year",ylab="CosineDist")





sink("C:\\Users\\sudhanshu\\Desktop\\Springboard\\10K\\outfile3.txt")
text
sink()
