kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)

# Для временных рядов по месяцам ставим frequency=12, а для данных
# по кварталам frequency=4, т.е. ts(kings, frequency=12)
kingstimeseries <- ts(kings)
kingstimeseries

# Можно установить первый год, в который собирались данные, и первый интервал в
# году с помощью параметра start функции ts(). Например, если первая точка данных
# соответствует второму кварталу 1986 года, установливаем start=c(1986,2)

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries

plot.ts(kingstimeseries)

plot.ts(birthstimeseries)