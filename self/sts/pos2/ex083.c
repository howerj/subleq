int *p,a,b,k;
int putchar();

int f()
{
	putchar(a);
	putchar(b);
}

int main()
{
	k = 1;
	*( k? &a: &b) = 'a';
	*(!k? &a: &b) = 'b';

	f();

	k ? putchar('c') : putchar('d');	
	!k? putchar('e') : putchar('f');

	a = k ? 'g' : 'h';
	b = !k? 'i' : 'j';

	f();

	( k? a : b ) = 'k';
	( !k? a : b ) = 'l';

	f();

	p = &( k? a : b );
	putchar(*p);

	p = &( !k? a : b );
	putchar(p[0]);

	int i;
	(k? a : i ) = 'm';
	(!k? a : i ) = 'n';

	b = i;
	f();

}
