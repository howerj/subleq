#include<stdio.h>
short m[1<<16],*_=m,a,b,A;main(p,y)char**y;{
for(void*f=fopen(y[1],"r");fscanf(f,"%hd,",_++)>0;);
for(p=0;p+1;){
A=m[a=m[p++]],b=m[p],p+=2;
a+1?b+1?p=(m[b]-=A)>0?p:m[p-1]:putchar(A):(m[b]=getchar());
}}
