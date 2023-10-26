short s[8<<13]={u},b,l,e;main(q){
for(;~e;){
q=s[b=s[e++]],l=s[e],e+=2;
~b?~l?e=(s[l]-=q)>0?e:s[e-1]:putchar(q):(s[l]=getchar());
}}
