
##Saloni Sivakumar##
#####U test#####

#Formulas-
#Null Hypothesis-U1=U2
#Alternative Hypothesis-U1!=U2
#U1=(n1*n2)+((n1*(n1+1))/2)-r1
#U2=(n1*n2)+((n1*(n1+1))/2)-r2

#U_cal2=min(U1,U2)
#U_cal2


#Questions-

#Null Hypothesis-U1=U2
#Null Hypothesis-U1!=U2
GroupA=c(7,5,6,4,12)
GroupA

GroupB=c(3,6,4,2,1)
GroupB

data.frame(GroupA,GroupB)

y=c(7,5,6,4,12,3,6,4,2,1)

rank(y)

d2=data.frame(y,rank(y))

View(d2)

r1=sum(d2$rank.y[1:5])
r1

r2=sum(d2$rank.y[6:10])
r2
nr=length(GroupA)
n2=length(GroupB)

U1=(n1*n2)+((n1*(n1+1))/2)-r1
U2=(n1*n2)+((n1*(n1+1))/2)-r2

U_cal2=min(U1,U2)
U_cal2

#U table(5,5)=2
#As Ucal is > U table,Null is rejected and there is a difference in U




####Run Test######

#Formulas-
#sigmar==sqrt((2*n1*n2)*((2*n1*n2)-n1-n2)/(((n1-n2)*(n1-n2))*(n1+n2-1)))
#Ur=((2*n1*n2)/(n1+n2)+1)
#Z=(r-Ur)/sigmar

#Questions-
#Null Hypothesis-Sequence is random
#Alternative Hypothesis- Sequence is not random
n=14
n1=8
n2=6
r=8

Ur=((2*n1*n2)/(n1+n2)+1)
Ur

sigmar=sqrt((2*n1*n2)*((2*n1*n2)-n1-n2)/(((n1-n2)*(n1-n2))*(n1+n2-1)))
sigmar

z_cal=(r-Ur)/sigmar
z_cal

#z table =1.96 at 5% LOS
#As z_cal < z_tab
#Null hypothesis is accepted and sequence is Random


####Spearman and Kendall's Test###

#Formula for no rank repetition-
#GroupA=c()
#GroupB=c()
#di=(rank(GroupA)-rank(GroupB))
#R=1-(6*sum((di)^2))/((n^3)-n)
#t_cal=R*sqrt((n-2)/(1-R^2))

#Formula for rank repetition-
#mA=1
#m=(mA^3-mA)/12
#R=1-(((6)*(m+sum(di2)))/(n^3-n))
#t_cal=R*sqrt((n-2)/(1-R^2))


#Question When rank is not repeated
#Null Hypothesis-P=0
#Alternative Hypothesis-P!=0
GroupA=c(43,48,56,61,67,70)
GroupB=c(128,120,135,143,141,152)

data.frame(GroupA,GroupB)

di=(rank(GroupA)-rank(GroupB))
d=data.frame(GroupA,rank(GroupA),GroupB,rank(GroupB),(di)^2)

View(d)
n=length(GroupA)
R=1-(6*sum((di)^2))/((n^3)-n)
R

t_cal=R*sqrt((n-2)/(1-R^2))
#degree of freedom is 6-1=5
#As LOS=5%,t table= 2.776
#As T calculated is more than T table,null Hypothesis is rejected 
#As t_cal is < t table,null is accepted and there is no significant linear relationship between A and B
#Question When rank is repeated
#Null Hypothesis-P=0
#Alternative Hypothesis-P!=0

GroupA=c(4.5,4.5,3,2,1,5)
mA=1
m=(mA^3-mA)/12
n=length(A)
GroupB=c(1,2,3,4,5,6)


di=(rank(GroupA)-rank(GroupB))

anss=data.frame(GroupA,GroupB,rank(GroupA),rank(GroupB),(di)^2)

View(anss)


R=1-(((6)*(m+sum(di^2)))/(n^3-n))

t_cal=R*sqrt((n-2)/(1-R^2))

#degree of freedom is 6-1=5
#As LOS=5%,t table= 2.776
#As t_cal is < t table,null is accepted and there is significant linear relationship between A and B


####Chi Square ####

#Formulas:
#Estimated =c()
#Observed=c()
#Calculated_Chi_square=sum(((Es-Ob)^2)/Es)
#If Calculated_Chi_square is <Tabular_Chi_Sqaure,Null is accepted
#If Calculated_Chi_square is >Tabular_Chi_Sqaure,Null is rejected
#C=sum(ans)
#S=data.frame(Es,Ob,((Es-Ob)^2),ans)


#Question from pdf-
#Null Hypothesis-Estimated and Observed support each other
#Alternative Hypothesis-Estimated and Observed do not support each other

Es=c(9/16*1600,3/16*1600,3/16*1600,1/16*1600)
Ob=c(882,313,287,118)
ans=((Es-Ob)^2)/Es
C=sum(ans)
S=data.frame(Es,Ob,((Es-Ob)^2),ans)
View(S)

#degree of freedom=4-1=4
#At 5%LOS,Chi square from table=7.8

#As Tabular Chi square is <Calculated Chi square,Null Hypothesis is accepted and we can say that Estimated and Observed support each other


