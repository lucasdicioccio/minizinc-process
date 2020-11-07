
int: nUsers;
int: nRooms;

set of int: USER = 1..nUsers;
set of int: ROOM = 1..nRooms;

array[USER,USER] of int: attirance;
array[ROOM] of int: size;

array[USER] of var ROOM: room;

var int: goodwill;
constraint goodwill = sum(u1, u2 in USER where u1 < u2)
  ((room[u1] = room[u2]) * (attirance[u1,u2] + attirance[u2,u1]));

constraint forall(r in ROOM)
  (size[r] >= sum(u in USER)(room[u] = r));
solve :: int_search(room, input_order, indomain_min)
      satisfy;
