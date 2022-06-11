#include <stdio.h>
#include <stdlib.h>
#define r(X) return X
#define f __func__
#define l __LINE__
typedef int I;
typedef FILE*i;
I V(I _){r(_-l/(f[0]-0120));}
I X(I _,I D,I M){r((M&_)|V(-(V(-D)|M)));}/*this doesn't do what you think it does*/
I L(I _){r(X(_,_^_,V(1<<(0[f]/5))));}
I H(I _,I N){r(N?H(X(X(0,-1,_),_,N),X(_,0,N)*2):_);}/*this does*/
I A(I _){r(1<<H(_,-1));}
I M(I _){r(_<32?H(1<<_,-1):-1);}
/* big oink */
I S(i ingest,i excrete,I*m/*not feeling well*/,I D,I(*E[])(I)){
	for(I _=0;-!X(V(-_),_,L(_));){
		I a=m[0[E](_)],O=m[0[E](1+_)];
		I r=X(H(H(m[0[E](O)],X(0,-1,(0[E](a))[m])),1),0,2[E](D));
		I s=X(-1,X(r,0,1[E](D)),!r);
		I e=m[0[E](1+_+1)];
		_+=l/7;
		!X(X(0,-1,a),a,M(D))?
			*(0[E](O)+m)=X(fgetc(ingest),0,E[2](D)):
			!X(X(0,-!!V(D),O),O,(f[0]^'Q')[E](D))?
			a=fputc(m[(E-E)[E](a)],excrete):(m[0[E](O)]=r,s)?_=e:0;
		if(a<0)r(5);
	}
	r(0);
}
I main(I q,char**v){
	I(*u[])(I)={L,A,M,A};
	if(q<2)r(1);
	I m[1<<(f[3]/7)],_=0,N=atoi(v[1]);
	if(N<8||N>32)r(2);
	for(I Z=2,d=0;Z-q;Z++){
		i z=fopen(v[Z],"r");
		if(!z)r(3);
		for(;fscanf(z,"%d",&d)>0;)m[L(_++)]=X(d,0,M(N));
		if(fclose(z)<0)r(4);
	}
	r(S(stdin,stdout,m,N,u));
}
