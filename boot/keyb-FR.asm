	cmp al, ';'
	jne .not_semicol
	mov al, 'm'
	jmp .mapped
.not_semicol:

	cmp al, 'q'
	jne .not_q
	mov al, 'a'
	jmp .mapped
.not_q:

	cmp al, 'a'
	jne .not_a
	mov al, 'q'
	jmp .mapped
.not_a:


	cmp al, 'z'
	jne .not_z
	mov al, 'w'
	jmp .mapped
.not_z:

	cmp al, 'w'
	jne .not_w
	mov al, 'z'
	jmp .mapped
.not_w:

	cmp al, ':'
	jne .not_colon
	mov al, 'M'
	jmp .mapped
.not_colon:

	cmp al, 'Q'
	jne .not_Q
	mov al, 'A'
	jmp .mapped
.not_Q:

	cmp al, 'A'
	jne .not_A
	mov al, 'Q'
	jmp .mapped
.not_A:


	cmp al, 'Z'
	jne .not_Z
	mov al, 'W'
	jmp .mapped
.not_Z:

	cmp al, 'W'
	jne .not_W
	mov al, 'Z'
	jmp .mapped
.not_W:

	cmp al, '\'
	jne .not_antislash
	mov al, '<'
	jmp .mapped
.not_antislash:

	cmp al, '|'
	jne .not_pipe
	mov al, '>'
	jmp .mapped
.not_pipe:

	cmp al, 'm'
	jne .not_m
	mov al, ','
	jmp .mapped
.not_m:

	cmp al, 'M'
	jne .not_M
	mov al, '?'
	jmp .mapped
.not_M:

	cmp al, ','
	jne .not_comma
	mov al, ';'
	jmp .mapped
.not_comma:

	cmp al, '<'
	jne .not_less
	mov al, '.'
	jmp .mapped
.not_less:

	cmp al, '.'
	jne .not_dot
	mov al, ':'
	jmp .mapped
.not_dot:

	cmp al, '>'
	jne .not_great
	mov al, '/'
	jmp .mapped
.not_great:

	cmp al, '/'
	jne .not_slash
	mov al, '!'
	jmp .mapped
.not_slash:

	cmp al, 39
	jne .not_quote
	mov al, '*'
	jmp .mapped
.not_quote:

	cmp al, '='
	jne .not_equal
	mov al, '+'
	jmp .mapped
.not_equal:

	cmp al, '+'
	jne .not_plus
	mov al, '='
	jmp .mapped
	.not_plus:

	cmp al, '#'
	jne .not_hashtag
	mov al, 34
	jmp .mapped
.not_hashtag:

	cmp al, '$'
	jne .not_dollar
	mov al, 39
	jmp .mapped
.not_dollar:

	cmp al, '!'
	jne .not_exclam
	mov al, '&'
	jmp .mapped
.not_exclam:

	cmp al, '@'
	jne .not_at
	mov al, '~'
	jmp .mapped
.not_at:

	cmp al, '%'
	jne .not_percent
	mov al, '('
	jmp .mapped
.not_percent:

	cmp al, '_'
	jne .not_underscore
	mov al, ')'
	jmp .mapped
.not_underscore:

	cmp al, '^'
	jne .not_caret
	mov al, '|'
	jmp .mapped
.not_caret:

	cmp al, '&'
	jne .not_ampersand
	mov al, '`'
	jmp .mapped
.not_ampersand:

	cmp al, '*'
	jne .not_star
	mov al, '\'
	jmp .mapped
.not_star:

	cmp al, '('
	jne .not_op_parent
	mov al, '^'
	jmp .mapped
.not_op_parent:

	cmp al, ')'
	jne .not_cl_parent
	mov al, '@'
	jmp .mapped
.not_cl_parent:

	cmp al, '`'
	jne .not_backtick
	mov al, '_'
	jmp .mapped
.not_backtick:

cmp al, '?'
	jne .not_question
	mov al, '_'
	jmp .mapped
.not_question:

.mapped:
