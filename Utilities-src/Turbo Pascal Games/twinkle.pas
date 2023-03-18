(*
 * twinkle
 * randomly fill the screen with
 * '*' and then randomly remove them
 *)

program twinkle(output);

const
   scrsize = 1920;      (* number of points on the screen *)

   xmax    = 79;        (* maximum x location (0 -> 79) *)
   ymax    = 23;        (* maximum y location *)

type
   pos = record
      xloc : 0..xmax;
      yloc : 0..ymax
   end;

   index = 1..scrsize;

var
   screen : array[index] of pos;

   (*
    * init
    * do a primitive clear screen,
    * then init the screen array
    *)

procedure init;

var
   scrindex : integer;  (* index into screen array *)
   x : 0..xmax;
   y : 0..ymax;

begin
   (* do a simple clear screen *)
   for y := 0 to ymax do
      writeln;

   randomize;

   scrindex := 1;
   for x := 0 to xmax do
      for y := 0 to ymax do
      begin
         screen[scrindex].xloc := x;
         screen[scrindex].yloc := y;
         scrindex := scrindex + 1
      end
end;  (* init *)

(*
 * shuffle
 * shuffle the screen index
 *)

procedure shuffle;

var
   i : index;
   tmp : pos;
   rnd : index;

begin
   for i := 1 to scrsize do
   begin
      rnd := random(scrsize) + 1;
      tmp := screen[rnd];
      screen[rnd] := screen[i];
      screen[i] := tmp
   end
end;  (* shuffle *)

(*
 * fill
 * fill up the screen with a character
 *)

procedure fill(ch:char);

var
   i : index;

begin
   for i := 1 to scrsize do
   begin
      gotoxy(screen[i].xloc,screen[i].yloc);
      write(ch)
   end;
end;  (* fill *)


    (* main  *)

begin
   init;
   shuffle;
   fill ('*');
   shuffle;
   fill (' ')
end.  (* twinkle *)
