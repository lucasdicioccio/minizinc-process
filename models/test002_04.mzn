% model useful for testing codegeneration

int: x1;
bool: x2;
float: x3;
array[1..2] of int: xs1;
array[1..2] of bool: xs2;
array[1..2] of float: xs3;
array[1..2,1..2] of int: xs4;
set of int: xz1;
set of bool: xz2;
set of float: xz3;


var int: y1;
var bool: y2;
var float: y3;
array[1..2] of var int: ys1;
array[1..2] of var bool: ys2;
array[1..2] of var float: ys3;
array[1..2,1..2] of var int: ys4;
var set of 1..10: yz1;

constraint y1 = 1;
constraint y2 = true;
constraint y3 = 1.0;

constraint ys1 = [1,2];
constraint ys2 = [true, false];
constraint ys3 = [1.0, 2.0];
constraint ys4 = array2d(1..2,1..2, [1,2,3,4]);

% force 1-3, .... , 10
constraint 1 in yz1;
constraint 2 in yz1;
constraint 3 in yz1;
constraint 10 in yz1;
constraint card(yz1) = card(xz1);
