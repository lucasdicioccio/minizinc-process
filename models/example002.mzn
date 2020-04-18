0..100: x;
enum BAR;
var int: y;
set of int: FOO;
array[FOO] of BAR: zs1;
array[FOO,FOO] of BAR: zs2;
array[FOO] of set of BAR: zss;
array[FOO] of string: ws1;
array[FOO] of set of string: wss;
var set of BAR: bars;
constraint x < y;

solve satisfy;

output ["coco rico"]
