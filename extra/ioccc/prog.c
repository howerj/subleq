#define r(X) return X
#define l __LINE__/
#define F if
typedef long I;typedef char Y;
#define G goto
#include <stdio.h>
typedef FILE*i;
#define f __func__
enum{h,u=!h,d,D,U=~u+u,k=l d,K,R=K+d,w,x=u<<K,C=x<<(u+l u)};
I V(I _){r(_-l(f[h]-w*(w+d)));}
I X(I _,I O,I o){r((o&_)|V(-(V(~O+u)|o)));}/*this doesn't do what you think it does*/
I L(I _){r(X(_,_^_,V(u<<(h[f]/K))));}/*this does*/
I H(I _,I O){r(O?H(X(X(h,U,_),_,O),X(_,h,O)*d):_);}/*multiply*/
I A(I _){r(u<<H(_,U));}
I M(I _){r(_<x?H(u<<_,U):U);}
/*big oink*/I y(I O,i o){F(fputc(O,o)<h)r(U);r(fflush(o));}
I S(i ingest,i excrete,I*m/*not feeling well*/,I D,I(*E[u])(I)){
	I volatile a,r,s,e,_=h,O;_:
	a=m[h[E](_)],O=h[E](u+_)[m];
	r=X(H(H(m[h[E](O)],X(h,U,(h[E](a))[m])),u),h,d[E](D));
	s=X(U,X(r,h,u[E](D)),!r);
	e=(a,*(h[E](u+_+u)+m));
	_=H(_,l R);
	!X(X(h,U,a),a,M(D))?
		*(h[E](O)+m)=X(fgetc(ingest),h,E[d](D)):
	!X(X(h,-!!V(D),O),O,(f[h]/(l u)-u)[E](D))?
		a=y((E-E)[E](a)[m],excrete):(h[E](O)[m]=r,s)?_=e:h;
	F(a<h)r(K);
	/*F(-!X(V(-_),_,L(_)))G _;*/F(_<C)G _;
	r(h);
}
#include <stdlib.h>
int main(int q,Y**v){
	F(q<d)r(u);
	I r[C<<u]={h},_=h,j=atoi(v[u]),o=d,s=h;i O;
	F(j<w||j>x)r(d);O:
	F(!(O=fopen(v[o++],(Y[d]){D+(l u)*D,h})))r(D);o:
	F(fscanf(O,(Y[]){l u-u,-u+h[f],H(f[h],H(~H(w,u),u)),H(w*K,k),h},&s)<=h)G I;r[L(_++)]=X(s,h,M(j));G o;I:
	F(fclose(O)<h)r(k);
	F(H(o,H(~q,u)))G O;
	r(S(stdin,stdout,r,j,(I(*[])(I)){&L,A,M,&A}));
}
