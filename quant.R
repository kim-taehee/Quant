# install.packages("quantmod")
# install.packages("RJSONIO")
library(quantmod)
library(RJSONIO)
library(dplyr)
library(xts)



# 2장 거래도구 ####
A<-c(1,2,5,6,9)
B<-c(0,3,6,9,10)
A %in% B

filter_price<-function(prices){
  # 이름 : filter_prices
  # 목적 : 결측값을 값는 행 찾기
  
  valid_rows<-complete.cases(prices)
  
  missing_rows<-which(valid_rows == FALSE)
}


# 3장 데이터 작업 ####
# do, %>%를 잘쓰자
data("sample_matrix")
xts_matrix<-as.xts(sample_matrix, descr='my new xts object')

plot(xts_matrix, main='Candle plot On xts object', cex.main =0.8, type= "l")

plot(xts_matrix["2007-01-01::2007-02-12"], main='Candle plot On xts object', cex.main =0.8, type= "o")

# example1 :   xts by unit seconds
price_vector <- c(101.02, 101.03, 101.03, 101.04, 101.05, 101.03, 101.02, 101.01, 101.00, 100.99)
dates <-c("07/12/2016 08:00:00.532123",
          "07/12/2016 08:00:01.650321",
          "07/12/2016 08:00:01.982333",
          "07/12/2016 08:00:02.402321",
          "07/12/2016 08:00:02.540432",
          "07/12/2016 08:00:03.004554",
          "07/12/2016 08:00:03.900213",
          "07/12/2016 08:00:04.050323",
          "07/12/2016 08:00:04.430345",
          "07/12/2016 08:00:05.700123"
          )

# show micro seconds
options(digits.secs = 6)
time_index <-strptime(dates,format = "%d/%m/%Y %H:%M:%OS")
xts_price_vector <- xts(price_vector,time_index)

time_diff <- difftime(index(xts_price_vector)[2], index(xts_price_vector)) #기준시와의 차이

plot(time_diff)

# quantmod package
AAPL<-getSymbols("AAPL",auto.assign = FALSE)
head(AAPL)

chart_Series(AAPL, subset = '2010::2010-04') # 자세한 것은 책 뒤쪽


# 4장 기초확률 통계-datalab에 써먹자 ####
X<-rnorm(100000,2.33,0.5) # 정규분포로 생성 
mu<-mean(X)
vari<-var(X)

# 반복시행으로 정규분포 따르는것을 보기
mean_list <- list()
for(i in 1:10000){
  mean_list[[i]]<-mean(sample(X,10,replace = TRUE)) #복원추출
}
hist(unlist(mean_list),breaks = 500,
     xlab ="Mean of 10 saples from X",
     main = "distribution"
)
abline(v=mean(X),lwd=3,col='white',lty=2)

# 표본오차가 표분편차보다 정확할까? - 더 정확하다(p.124)

# Rstan - bayesian tool ###  중요함 ####
# github.com/stan-dev/rstan/wiki/Rstan-Getting-Started 에서 설치법을 참고



# 5장 중급확률통계 ####
# 주식 수익률의 정상성(stationarity),정규성(normality),자기상관(autocorrelation) 등


# 5-1. 정상성 테스트 #
## 시간에 따라 모양과 위치가 변하지 않는 확률 분포를 가져야 완전한 정상 ##
## 실제로는 잘 존재하지 않는다 ##
## random process - black box ##
# example - IWM's end price
require(quantmod)
getSymbols("SPY") #SPY ETF의 종가

# 주가를 뽑아내 통계치를 계산함
prices <- SPY$SPY.Adjusted
mean_prices <-round(mean(prices),2)
sd_prices<-round(sd(prices),2)

# 범례가 포함된 히스토그램 생성
hist(prices,breaks=100,probability =T,cex.main=0.9)
abline(v=mean_prices,lwd=2)
legend("topright",cex=0.8,border = NULL,bty="n",paste("mean=",mean_prices,"; sd=",sd_prices)  )

# 다른 기간들의  여러 분포 - 함수만들기
plot_4_ranges <- function(data,start_date,end_date,title){
  
  par(mfrow=c(2,2)) # 다중창 만들기
  
  for(i in 1:4){
    range<-paste(start_date[i],"::",end_date[i],sep="") # 적절한 날짜 기간을 갖는 문자열 생성
    time_series <- data[range] # 주가 벡터와 필요한 통계치 생성
    mean_data <- round(mean(time_series, na.rm=TRUE),3) # 결측치 제거, scaling
    sd_data <- round(sd(time_series,na.rm = TRUE),3)
    
    hist_title <- paste(title,range)
    hist(time_series,breaks = 100,probability = TRUE,xlab = "",main=hist_title, cex.main=0.8)
    legend("topright",cex=0.7,bty = 'n',paste("mean=",mean_data,"; sd=",sd_data))
    
  }
  
  par(mfrow=c(1,1))
}

# 시작날짜와 종료날짜 정의
begin_dates <-c("2007-01-01","2008-06-06","2010-10-10","2011-03-03")
end_dates <-c("2008-06-05","2010-09-09","2011-03-02","2013-01-06")

plot_4_ranges(prices,begin_dates,end_dates,"SPY prices for: ") # 그려보면 정상성을 가지지 않는다 / 그래서 로그화 시킨다

#로그 수익률 계산
returns <-diff(log(prices))
plot_4_ranges(returns,begin_dates,end_dates,"SPY log prices for: ")

# urca를 통한 정상성 테스트 
require(urca)
# 데이터 정상성 테스트 
spy<-SPY$SPY.Adjusted
test<- ur.kpss(as.numeric(spy))

class(test)
test@teststat
test@cval # 임계값(threadhold) 살펴보기, 비정상성

spy_returns <-diff(log(spy))

# 수익률에 대해 테스트
test_returns <- ur.kpss(as.numeric(spy_returns))
test_returns@teststat
test_returns@cval



# 5-2. 정규성 테스트 


# 5-2-1. qqplot 방법
# SPY data
qqnorm(as.numeric(returns),main = "SPY qqplot")
qqline(as.numeric(returns),lwd=2)

# gaussian data
normal<-rnorm(nrow(returns),mean=mu,sd=vari)
qqnorm(normal)
qqline(normal)


# 5-2-2. shapiro wilk test
shapiro.test(returns)
shapiro.test(normal)

# 5-3. 변동성(등분산성) 중 자기상관(시계열 특성)
# 자기 상관의 정확한 정의 : 앞뒤 오차항간에 같은 부호(positive) or 다른 부호(negative)를 가지려는 성질이 있음
# 

z<-rnorm(1000,0,1)

# 수익룰과 제곱 수익률의 자기 상관
par(mfrow=c(2,1))
acf(z,main="returns")
grid()
acf(z^2,main="returns squared")
grid()
par(mfrow=c(1,1))


# 6. 주식의 스프레드,베타,리스크는 추후 공부

# 7. quantstart를 이용한 백테스팅

