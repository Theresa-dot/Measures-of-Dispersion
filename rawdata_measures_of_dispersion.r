x=c(25,67,48,53,18,39,44)
summary(x)

#to calculate range=max-min
range=67-18
range

#to calculate coefficiant of range
crange=(67-18)/(67+18)
crange

#to calculate standard deviation by actual mean method
m=mean(x)
d=x-m
var_actual=sum(d^2)/n
sd_actual=sqrt(var_actual)
sd_actual

#to calculate standard deviation by assumed mean method
am=53
d=x-am
sum_d=sum(d)
sum_d2=sum(d^2)
n=7
var_assumed=sum_d2/n-(sum_d/n)^2
sd_assumed=sqrt(var_assumed)
sd_assumed

#to calculate standard deviation by direct method
m=mean(x)
sum_x2=sum(x^2)
var_direct=sum_x2/n-m^2
sd_direct=sqrt(var_direct)
sd_direct

#to calculate standard deviation by step deviation method
am=53
cf=1
d=(x-am)/cf
sum_d=sum(d)
sum_d2=sum(d^2)
n=7
var_step=sum_d2/n-(sum_d/n)^2
sd_step=sqrt(var_step)
sd_step

#coefficiant of variation
cv=(sd_direct/mean(x))*100

#to calculate quartile deviation=(q3-q1)/2
qd=(50.5-32)/2
qd

#to calculate coefficient of quartile deviation=(q3-q1)/(q3+q1)
cqd=(50.5-32)/(50.5+32)
cqd

#mean deviation by mean
y=abs(x-mean(x))
md1=sum(y)/length(y)
md1

#coefficiant of mean deviation by mean
md1/mean(x)

#mean deviation by median
z=abs(x-median(x))
md2=sum(z)/length(z)
md2

#coefficiant of mean deviation by median
md2/median(x)

