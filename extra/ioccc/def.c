short m[1<<16]={X},a,b,A,p;main(){
for(;~p;){
A=m[a=m[p++]],b=m[p],p+=2;
~a?~b?p=(m[b]-=A)>0?p:m[p-1]:putchar(A):(m[b]=getchar());
}}
