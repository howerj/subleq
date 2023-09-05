\ TODO: Integrate into subleq.fth as optional code

only forth definitions decimal
system +order
: page 50 for cr next ; ( -- : hacky 'page' )


system +order definitions
: .pwd dup ." PWD:" . ; ( pwd -- pwd )
: .nfa dup ."  NFA:" nfa . ; ( pwd -- pwd )
: .cfa dup ."  CFA:" cfa . ; ( pwd -- pwd )
: .blank ." --- " ; ( -- )
: .immediate dup $40 and if ." IMM " exit then .blank ;
: .compile-only dup $20 and if ." CMP " exit then .blank ;
: .hidden dup $80 and if ." HID " exit then .blank ;
: display ( pwd -- )
  dup .pwd .nfa .cfa space nfa count 
  .immediate .compile-only .hidden
  31 and type cr ;
: (w) begin ?dup while display @ repeat ; ( voc -- )
: .voc dup  ." voc: " . cr ; ( voc -- voc )
only forth definitions system +order
: w get-order for aft .voc @ (w) then next ; ( -- )

only forth definitions decimal

: 2swap >r -rot r> -rot ;       ( w x y z -- y z w x )
: 2over ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
  >r >r 2dup r> swap >r swap r> r> -rot ;
: d< rot 2dup >                    ( d -- f )
  if = nip nip if 0 exit then -1 exit then
  2drop u< ;
: dabs s>d if dnegate then ;      ( d -- ud )
: 2rot >r >r 2swap r> r> 2swap ; ( d1 d2 d3 -- d2 d3 d1 )
: d0= or 0= ; ( d -- f )
: d0< nip 0< ; ( d -- f )
: d- dnegate d+ ; ( d d -- d )
: du<  rot swap u< if 2drop -1 exit then u< ; ( ud ud -- f )
: du> 2swap du< ; ( ud -- t )
: d=  rot = -rot = and ; ( d d -- f )
: d>  2swap d< ; ( d d -- f )
: dabs 2dup 0 0 d< if dnegate then ; ( d -- ud )
: dmax 2over 2over d< if 2swap then 2drop ; ( d1 d2 -- d )
: dmin 2over 2over d> if 2swap then 2drop ; ( d1 d2 -- d )

: d. dup -rot dabs <# #s sign #> type space ; ( d -- )
: d.r >r dup -rot dabs <# #s sign #> r> (pad) type ; ( d n -- )

system +order
: roll ?dup if swap >r 1- recurse r> swap then ; 
: -roll ?dup if rot >r 1- recurse r> then ; 
: reverse for aft r@ -roll then next ; ( x0...xn n -- xn...x0 ) 

