\ 'ingest', the opposite of 'dump'
: nul? count nip 0= ;
: grab begin bl word dup nul? if drop query else exit then again ;
: integer grab count number? ;
: integer? integer 0= nip ( dpl @ 0>= or ) -24 and throw ;
: >cells 2/ ;
: ingest >cells for aft integer? over ! cell+ then next drop ; ( a u -- )
: .dump ." DUMP:  " pad 10 dump cr ;
: .ingest ." INGEST: " pad 10 ingest cr ;
: test .dump .ingest .dump ;
