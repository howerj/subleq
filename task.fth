\ ' nop <ok> !
\ task: status
\ task: ahoy
task: rx
task: tx1
task: tx2

\ : .stats begin 100 ms single cr cycles @ . multi again ;
\ ' .stats status activate
\ : .ahoy begin 100 ms single cr ." HELLO" multi again ;
\ ' .ahoy ahoy activate

: .tx1 begin [char] X rx send 100 ms again ;
: .tx2 begin [char] Y rx send 200 ms again ;
: .rx begin multi receive single . space emit cr again ;

single
' .tx1 tx1 activate
' .tx2 tx2 activate
' .rx rx activate
: schedule begin pause again ; 
multi schedule
