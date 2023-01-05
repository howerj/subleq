#define r(X) return X
#define l __LINE__/
#define F if
typedef int I;
#define G goto
#include <stdio.h>
typedef FILE*i;
#define f __func__
I V(I _){r(_-l(f[0]-0120));}
I X(I _,I O,I o){r((o&_)|V(-(V(-O)|o)));}/*this doesn't do what you think it does*/
I L(I _){r(X(_,_^_,V(1<<(0[f]/5))));}/*this does*/
I H(I _,I O){r(O?H(X(X(0,-1,_),_,O),X(_,0,O)*2):_);}/*multiply*/
I A(I _){r(1<<H(_,-1));}
I M(I _){r(_<32?H(1<<_,-1):-1);}
/*big oink*/
I S(i ingest,i excrete,I*m/*not feeling well*/,I D,I(*E[static 1])(I)){
	I volatile a,r,s,e,_=0,O;_:
	a=m[0[E](_)],O=0[E](1+_)[m];
	r=X(H(H(m[0[E](O)],X(0,-1,(0[E](a))[m])),1),0,2[E](D));
	s=X(-1,X(r,0,1[E](D)),!r);
	e=(a,*(0[E](1+_+1)+m));
	_+=l 7;
	!X(X(0,-1,a),a,M(D))?
		*(0[E](O)+m)=X(fgetc(ingest),0,E[2](D)):
	!X(X(0,-!!V(D),O),O,(f[0]^'Q')[E](D))?
		a=fputc((E-E)[E](a)[m],excrete):(0[E](O)[m]=r,s)?_=e:0;
	F(a<0)r(5);
	/*F(-!X(V(-_),_,L(_)))G _;*/F(_<32768&&M(_))G _;
	r(0);
}
#include <stdlib.h>
I main(I q,char**v){
	F(q<2)r(1);
	I r[1<<(1+(f[3]/7))],_=0,j=atoi(v[1]),o=2,s=0;i O;
	F(j<8||j>32)r(2);O:
	F(!(O=fopen(v[o++],"r")))r(3);o:
	F(fscanf(O,"%d,",&s)<=0)G I;r[L(_++)]=X(s,0,M(j));G o;I:
	F(fclose(O)<0)r(4);
	F(o-q)G O;
	r(S(stdin,stdout,r,j,(I(*[])(I)){&L,A,M,&A}));
}
