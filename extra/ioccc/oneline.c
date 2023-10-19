#include<stdio.h>
short m[1<<16],*_=m,a,b;
int main(int p,char**y){
	for(FILE*f=fopen(y[1],"r");fscanf(f,"%hd,",_++)>0;);
	for(p=0;p>=0;){
		a=m[p++],b=m[p++];p++;
		a+1?b+1?p=(m[b]-=m[a])>0?p:m[p-1]:putchar(m[a]):(m[b]=getchar());
	}
}
