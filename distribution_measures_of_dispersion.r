data=seq(15,55,10)
f=c(80,120,50,22,8)
fr.distr=data.frame(data,f)

#to calculate standard deviation by actual mean method
d=data-mean
var_actual=sum(f*d^2)/sum(f)
sd_actual=sqrt(var_actual)
sd_actual

#to calculate standard deviation by assumed mean method
A=35
d_assumed=(data-A)/10
var_assumed=sum(f*d_assumed^2)/sum(f)-(sum(f*d_assumed)/sum(f))^2
sd_assumed=sqrt(var_assumed)*10
sd_assumed

#to calculate standard deviation by step deviation method
cf=5
step_d=d/cf
fd2_step=sum(f*step_d^2)
fd_step=sum(f*step_d)
var_step=(fd2_step/sum(f)-(fd_step/sum(f))^2)
sd_step=sqrt(var_step)*cf
sd_step

#coefficiant of variation
cv=(sd_actual/mean)*100
cv

#to calculate mean deviation by mean
d=abs(data-mean)
fd=sum(f*d)
md1=fd/sum(f)
md1

#to calculate coefficiant of mean deviation by mean
md1/mean(x)

#to calculate mean deviation by median
cl=cumsum(f)
n=sum(f)
c=cl[ml-1]
ml=min(which(cl>=n/2))
h=10
l=data[ml]-h/2
f1=f[ml]
median=l+(((n/2)-c)/f1)*h
median
d=data-median
d=abs(d)
fd=f*d
md2=sum(fd)/n
md2

#to calculate coefficiant of mean deviation by median
md2/median

#to calculate quartile deviation
n=sum(f)
cl=cumsum(f)
h=10
q1=min(which(cl>=n/4))
l1=data[q1]-h/2
f1=f[q1]
c1=0
Q1=l1+(((n/4)-c1)/f1)*h
q3=min(which(cl>=3*n/4))
l3=data[q3]-h/2
f3=f[q3]
c3=cl[q3-1]
Q3=l3+((3*(n/4)-c3)/f3)*h
qd=(Q3-Q1)/2
qd

#to calculate coefficiant of quartile deviation
cqd=(Q3-Q1)/(Q3+Q1)
cqd

