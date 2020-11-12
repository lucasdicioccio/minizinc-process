% model useful for testing codegeneration

int: x1;
bool: x2;
float: x3;
array[1..2] of int: xs1;
array[1..2] of bool: xs2;
array[1..2] of float: xs3;

var int: y1;
var bool: y2;
var float: y3;
array[1..2] of var int: ys1;
array[1..2] of var bool: ys2;
array[1..2] of var float: ys3;

constraint y1 = 1;
constraint y2 = true;
constraint y3 = 1.0;

constraint ys1 = [1,2];
constraint ys2 = [true, false];
constraint ys3 = [1.0, 2.0];

