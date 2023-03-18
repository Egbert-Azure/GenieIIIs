(*
 * pascal
 * creates and draws pascal triangles of any size
 *)

program pascal(input, output);

var
   ch  : char;    (* used to answer questions *)

(*
 * triangle
 * creates the triangle and prints it out.
 * note that only one row is created. To make
 * a node, all you need to know is the value of
 * the node to its left, and the value of the node
 * directly above it. as a result, i only keep the
 * previous row.
 *)

procedure triangle;

type
   nodeptr = ^node;
   node    = record
      value : integer;    (* value of this node *)
      next  : nodeptr;    (* next node (may be nil) *)
   end;

var
   start   : nodeptr;   (* beginning of the last line *)
   curnode : nodeptr;   (* current node *)
   size    : integer;   (* how big the triangle is to be *)
   mult    : integer;   (* the multiples to be removed *)
   last    : integer;   (* value of the last node *)
   heap    : ^integer;  (* pointer to the heap *)
   which   : integer;   (* which node we are printing on the line *)
   left    : integer;   (* how many rows of the triangle left *)

(*
 * print
 * prints out the current node
 *)

procedure print(value: integer);

begin
   if mult = 0 then     (* if mult = 0 => user wants real triangle *)
      write(value:4)
   else
      if (value mod mult) = 0 then
         write('  ')
      else
         write('* ')
end;

begin
   mark(heap);        (* used to free up space *)

   write('Which multiples do you want printed as spaces? ');
   readln(mult);
   write('Size of the triangle? ');
   readln(size);

(*
 * create the first row.
 * note that it is being created from
 * the left side toward the right.
 *)

   for which := 1 to size do
   begin
      print(1);
      new(curnode);
      curnode^.value := 1;
      curnode^.next := start;
      start := curnode
   end;
   writeln;

   for left := 1 to size do
   begin
      last := 0;
      curnode := start;
      for which := 1 to (size - left) do
      begin
         print(last+curnode^.value);
         curnode^.value := curnode^.value + last;
         last := curnode^.value;
         curnode := curnode^.next
      end;
      writeln
   end;
   release(heap)
end;

begin
   repeat
      triangle;
      writeln;
      write('Continue? ');
      readln(ch)
   until (ch <> 'y') and (ch <> 'Y')

end.
