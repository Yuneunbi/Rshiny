
# 네이버 api 이용해서 단어 검색 시, 블로그 제목 긁어오기

# query <- '유재석'
get_news <- function(id=id,secret=secret,query = query,display=display,start=start){
  
  
  
  url <- 'https://openapi.naver.com/v1/search/blog.json'
  # query <- URLencode(iconv(query,to="UTF-8"))
  query <- URLencode(enc2utf8(query))
  result <- GET(paste0(url,"?query=",query,'&display=',display,'&start=',as.character(start)), add_headers("X-Naver-Client-Id"=id,"X-Naver-Client-Secret" = secret))
  contents<- content(result, as = "parsed")
  
  
  Title <- NULL
  Date <- NULL
  # Description 
  for(i in 1:length(contents$items)){
    item <- contents$items[[i]]
    title <- item$title
    title <- gsub('<b>|</b>|&quot;|&q','',title)
    Title <- rbind(Title,title)
    date <- item$postdate
    Date <- rbind(Date,date)
  }
  
  Title <- gsub('<b>|</b>|&quot;|&q','',Title)
  Date <- substr(Date,6,11)
  
  newsdata <- data.frame(Title=Title,Date=Date,stringsAsFactors=FALSE)
  # return(newsdata)
  return(newsdata)
  
}
# start = 1 



# 단어 검색시 블로그 제목 긁어오고 전처리 후 단어 쪼개서 빈도수 테이블 만들기

get_words <- function(query){
  
  id = id # api id
  secret = secret # api password
  
  Data <- NULL
  data <- get_news(id=id,secret = secret,query=query,display='100',start=1)
  
  
  Data <- rbind(Data,data)
  
  for(j in 1:9){
    data <- get_news(id=id,secret=secret,query=query,display = '100',start =(100*j+1))
    Data <- rbind(Data,data)
  }
  # 페이지 바꿔가며 데이터 수집
  
  Data <- as.data.frame(Data,stringsAsFactors=FALSE)
  Data$Title <- gsub('\\[.*?\\]|\\(.*?\\)|\\&.*?\\;','',Data$Title)
  # 지저분한 단어들 제거 
  Data$Title <- str_replace_all(Data$Title,'\\W',' ')
  # 특수문자 제거
  Data$Title <- toupper(Data$Title) # 영어 대문자

  
  
  words <- extractNoun(Data$Title)
  # 명사인 단어만 구분해서 저장
  
  count<-as.data.frame(table(unlist(words)),stringsAsFactors = FALSE)
  # 단어 빈도 추출 
  
  count$Var1<-gsub(' ','',count$Var1)
  # 단어에 있는 불필요한 빈공간 지우기
  
  count<-count[-grep(query,count$Var1),]
  # 검색어는 제외시키기
  
  count<-count[nchar(count$Var1)>2,]
  # 단어 글자가 2개보다 많은 경우만 추출 
  
  return(count)
  
}



