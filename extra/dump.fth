' ) <ok> !
: nul? count nip 0= ; ( a -- f : is counted word empty? )
: grab begin bl word dup nul? 0= ?exit drop query again ;
: integer grab count number? nip ; 
: integer? integer 0= ( dpl @ 0>= or ) -$18 and throw ;
: bdump begin integer? dup emit 8 rshift emit again ;
bdump
