\ ' nop <ok> !
task: status
: .stats begin 100 ms single cr cycles @ . multi again ;
' .stats status task-set
status activate

: ahoy begin 100 ms single cr ." HELLO" multi again ;
ahoy
