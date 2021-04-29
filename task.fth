\ ' nop <ok> !
task: status
task: ahoy
: .stats begin 100 ms single cr cycles @ . multi again ;
' .stats status activate
: .ahoy begin 100 ms single cr ." HELLO" multi again ;
' .ahoy ahoy activate
: schedule begin pause again ; 
schedule
