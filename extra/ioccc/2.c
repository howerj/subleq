main(p,y)char**y;{short m[p*9],a,b,A;
for(;--p;)m[p-1]=atoi(y[p]);
for(;~p;){
A=m[a=m[p++]],b=m[p],p+=2;
~a?~b?p=(m[b]-=A)>0?p:m[p-1]:putchar(A):(m[b]=getchar());
}}
