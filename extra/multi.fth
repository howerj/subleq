' ( <ok> !
system +order
task: rx
task: tx1
task: tx2
: .tx1 begin [char] X rx send 100 ms again ;
: .tx2 begin [char] Y rx send 200 ms again ;
: .rx begin
  multi receive single . space emit cr again ;
single
' .tx1 tx1 activate
' .tx2 tx2 activate
' .rx rx activate
: schedule begin pause again ;
multi schedule
