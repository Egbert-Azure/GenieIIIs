Program LIFE;
{$I-}

{
('** "Life", APPLE PASCAL GAMES, Hergert & Kalash, pp. 223 et seq.,');
('           Sybex, 1981.  Modified for TURBO Pascal 1/10/84 d.c.o.');
}

const height = 24;
      width  = 80;
      minbound = -1;
      lively = '+';
      deadly = ' ';

type  state = (alive, dead);
      cell = record
               lookslikeitis: state;
               nearby: integer;
             end;
      edges = record left, right, top, bottom: integer end;
      screenline = string[80];

var   board: array[minbound..height] of array[minbound..width] of cell;
      population, births, deaths: integer;
      ch: char;
      quit: boolean;
      pause: integer;
      edge: edges;

procedure kill_typeahead;
begin
  if keypressed then read(kbd,ch)
end;  {kill_typeahead}

function yes(s:screenline):boolean;
var ch:char;
begin
  write(s,' Y/N ');
  kill_typeahead;
  repeat
    read(kbd,ch)
  until ch in ['y','Y','n','N'];
  yes:=ch in ['y','Y']
end;  {yes}

function min(a,b: integer):integer;
begin
  if a<=b then min:=a else min:=b
end;  {min}

function max(a,b: integer):integer;
begin
  if a>=b then max:=a else max:=b
end;  {max}

procedure resetedges;
begin
  edge.top:=height-1;
  edge.right:=minbound+1;
  edge.left:=width-1;
  edge.bottom:=minbound+1
end;  {resetedges}

procedure instructions;
var ch: char;

  procedure lecture_on_life;
  begin
    clrscr;
    write('LIFE simulates the growth of a colony of animalcules in a "');
    writeln(width-1:1,' by ',height-1:1,' World".');
    writeln;
    writeln('Whether a cell is born, lives or dies depends on the number of living');
    writeln('animalcules near by.  If a cell is empty and has exactly 3 neighbors, it');
    writeln('will be born in the next generation.  If it is alive and has 2 or 3');
    writeln('neighbors, it will stay alive.  Otherwise, it either dies of loneliness');
    writeln('or suffocates from overcrowding.');
    writeln;
    writeln('You type in the starting pattern, giving the XY location of each cell.');
    writeln('When you enter X Y be sure to leave a SPACE between the numbers.  When');
    writeln('you are through seeding a colony, enter a -1 to begin the generations.');
    writeln('The < key speeds things up a bit, the > key slows things down.  In the');
    writeln('good old days at M.I.T., this game was played with pencil & graph paper.');
    gotoxy(1,22);
    write('(* any key continues *) ');
    kill_typeahead;
    read(kbd,ch)
  end;  {lecture_on_life}

begin
  writeln; writeln; writeln; writeln; writeln;
  if yes('Would you like instructions for Life?') then
     lecture_on_life;
  clrscr
end;  {instructions}

procedure title;
begin
  clrscr;
  writeln('** "Life", APPLE PASCAL GAMES, Hergert & Kalash, pp. 223 et seq.,');
  writeln('           Sybex, 1981.  Modified for TURBO Pascal 1/10/84 d.c.o.');
end;  {title}

procedure initialize;
var down, across: integer;
begin
  for down:=minbound to height do
      for across:=minbound to width do
          begin
            board[down,across].lookslikeitis := dead;
            board[down,across].nearby := 0
          end;
  resetedges
end;  {initialize}

procedure limits(x,y: integer);
begin
  with edge do
    begin
      left:=min(left,x);
      right:=max(right,x);
      top:=min(top,y);
      bottom:=max(bottom,y)
    end
end; {limits}

procedure clearnearby;
var down, across: integer;
begin
  for down:=edge.top-1 to edge.bottom+1 do
      for across:=edge.left-1 to edge.right+1 do
          board[down,across].nearby := 0
end;  {clearnearby}

procedure countneighbors;
var down, across, deltadown, deltacross: integer;
begin
  clearnearby;
  for down:=edge.top-1 to edge.bottom+1 do
      for across:=edge.left-1 to edge.right+1 do
          if board[down][across].lookslikeitis = alive then
             for deltadown:=-1 to 1 do
                 for deltacross:=-1 to 1 do

           board[down+deltadown][across+deltacross].nearby :=
           board[down+deltadown][across+deltacross].nearby + 1
end;  {countneighbors}

procedure update;
var down, across: integer;
    localedge: edges;

begin
  births:=0;
  deaths:=0;
  localedge:=edge;
  resetedges;

  for down:=max(minbound+1,localedge.top-1) to
            min(height-1,localedge.bottom+1) do
  for across:=max(minbound+1,localedge.left-1) to
              min(width-1,localedge.right+1) do
      with board[down][across] do
        case lookslikeitis of
          dead:
            if nearby=3 then
              begin
                lookslikeitis:=alive;
                gotoxy(across+1,down+1);
                write(lively);
                limits(across,down);
                births:=births+1
              end;
          alive:
            if (nearby=3) or (nearby=4) then limits(across,down)
            else
              begin
                lookslikeitis:=dead;
                gotoxy(across+1,down+1);
                write(deadly);
                deaths:=deaths+1
              end
          end; {case}
  population:=population+births-deaths;
  gotoxy(1,1)
end;  {update}

procedure getpositions;
var down,across: integer;
    finished: boolean;

  procedure reprinttopline;
  var across:integer;
  begin
    gotoxy(1,1);
    for across:=minbound+1 to width-1 do
      if board[minbound+1][across].lookslikeitis = dead
        then write(deadly)
        else write(lively)
  end;  {reprinttopline}

begin
  finished:=false;
  population:=0;
  gotoxy(1,1);
  write('Position of Cell #',population+1:1,' is: ');
  while not finished do
    begin
      readln(across,down);  {works if you leave a space between x y}
      if ioresult<>0 then
        begin
          clrscr;
          writeln('*** INPUT ERROR ***');
          write('A Demonstration of LIFE with 9 cells from 32 11 to 41 11:');
          for across:=31 to 31+9 do begin
              limits(across,10);
              board[10][across].lookslikeitis:=alive;
              gotoxy(across+1,11); write(lively)
              end;
          for across:=1 to 128 do delay(64);
          finished:=true;
          population:=9
        end
      else
        begin
      if (down<=minbound) or
         (down>=height) or
         (across<=minbound) or
         (across>=width) then
         finished:=true
      else with board[down][across] do
        begin
          limits(across,down);
          gotoxy(across+1,down+1);
          if lookslikeitis = alive then
            begin
              write(deadly);
              lookslikeitis:=dead;
              population:=population-1
            end
          else
            begin
              write(lively);
              lookslikeitis:=alive;
              population:=population+1
            end;
          gotoxy(1,1);
          write('Position of Cell #',population+1:1,' is: ');
        end
      end
    end;
  reprinttopline
end;  {getpositions}

BEGIN  {Main}
  repeat
    initialize;
    title;
    instructions;
    getpositions;
    pause:=32;
    quit:=false;
    while not quit do
      begin
        countneighbors;
        update;
        for ch:='A' to 'Z' do delay(pause);
        quit := (population=0) or ((births=0) and (deaths=0));
        if keypressed then
          begin
            read(kbd,ch);
            if ch in ['>','.'] then pause:=min(pause+16,255)
            else if ch in ['<',','] then pause:=max(pause-16,0)
            else quit:=true
          end
      end;
    gotoxy(1,22);
    if population=0
      then writeln('This colony has died out.')
      else writeln
  until not yes('Would you like to run LIFE again?')
END.
