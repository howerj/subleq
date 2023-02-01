\ http://lars.nocrew.org/forth2012/core/IS.html
\ http://lars.nocrew.org/forth2012/core/DEFER.html

: is
   state @ if
     postpone ['] postpone defer!
   else
     ' defer!
   then ; immediate

: ['] postpone ' postpone literal ; immediate
: ?null ?dup 0= -2 and throw ;
: defer create ' abort , does> @ ?null execute ;
\ : is state @ if else then ; immediate
: defer! >body ! ;
: is postpone ' defer! ;
