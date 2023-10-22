short s[1<<16]={u},b,l,e,q;main(){
for(;~q;){
e=s[b=s[q++]],l=s[q],q+=2;
~b?~l?q=(s[l]-=e)>0?q:s[q-1]:putchar(e):(s[l]=getchar());
}}
