all: subleq-embed

subleq-dec.c: ../subleq.dec
	sed 's/$$/,/' ../subleq.dec > subleq-dec.c

subleq-embed: subleq-dec.c subleq-embed.c
	gcc -o subleq-embed subleq-embed.c

test: subleq-embed
	echo "words bye" | ./subleq-embed

prep-hello: hello-world-dec.c
	cp hello-world-dec.c subleq-dec.c

hello: prep-hello subleq-embed

clean:
	rm -f subleq-embed subleq-dec.c
