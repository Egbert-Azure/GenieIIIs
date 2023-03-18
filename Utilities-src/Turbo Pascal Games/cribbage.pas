PROGRAM Cribbage;

(*  TURBO PASCAL 1.0
    Morrow Micro Decision MD-2
    David C. Oshel, Jan 15, 1984, 1219 Harding Ave, Ames, Iowa 50010
    *)


{ TITLE PAGE:
(':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::');
(':: Adapted from "Cribbage" in APPLE PASCAL GAMES, by Douglas Hergert ::');
(':: and Joseph T. Kalash, pages 301-349.  Sybex, 1981.                ::');
('::                                                                   ::');
(':: January 8, 1984                                            d.c.o. ::');
(':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::');
}

Label 1;

Const
  decksize   =   52;
  dealsize   =    6;
  scribsize  =    6;
  playsize   =    4;
  ranksize   =   13;
  winpoints  =  121;

Type
  charset    =  set of char;
  str80      =  string[80];
  suitype    =  (hearts,diamonds,clubs,spades);
  ranktype   =  0..ranksize;

  card     = record
               rank: ranktype;
               suit: suitype
             end;

  handtype = array[1..dealsize] of card;

{Typed} Const

  alpha:charset   = [' '..'}'];

Var
  deck: array[1..decksize] of card;
  comp,
  human,
  crib: handtype;
  common: card;
  i,
  hscore,
  cscore: integer;
  ch: char;
  xplayx: integer;

(*
This is the code for simulating an Exit with TURBO Pascal 1.0

--> Include this instead of Exit(Procname) in the procedure which
    actually invokes the exit:

         inline($2A/save/        { LD HL,(save)  ; EXIT PROC }
                $F9);            { LD SP,HL                  }
         goto procend;

--> Include this as the FIRST instruction in the Procedure you wish
    to eventually exit from:

         inline($21/0/0/         { LD HL,0000h   ; MARK PROC }
                $39/             { ADD HL,SP                 }
                $22/save);       { LD (save),HL              }

David C. Oshel, 15 January 1984, Ames, Iowa
*)


function getchar:char;
var ch,cr,bs: char;
    goodset: charset;
    bailout: boolean;
begin
  cr:=chr(13); bs:=chr(8); goodset:=alpha+[cr,bs];
  repeat
    read(kbd,ch);
    ch:=upcase(ch);
    bailout:=(ch=chr(3)) or (ch=chr(27));
    if eoln then ch:=cr
  until bailout or (ch in goodset);
  getchar:=ch;
  if bailout then
         inline($2A/xplayx/      { LD HL,(xplayx) ;EXIT PROC }
                $F9)             { LD SP,HL                  }
end;  {getchar}

procedure getln(VAR s:str80);
var ch: char;
    done: boolean;
begin
  done:=false;
  s:='';
  repeat
    ch:=getchar;
    if (*  bailout or *) (ch=chr(13)) then
      begin
        done:=true;
        writeln
      end
    else if ch=chr(8) then
      begin
        if length(s) > 0 then
          begin
            write(chr(8),' ',chr(8));
            s:=copy(s,1,length(s)-1)
          end
        else s:=''
      end
    else
      begin
        s:=concat(s,ch);
        if ch in alpha then write(ch)
      end
  until done;
end;  {getln}


procedure addpoints(who:boolean; amount:integer);
var winner: boolean;
begin
  if who then
    begin
      hscore:=hscore+amount;
      writeln('You''ve pegged ',hscore,' points.');
      winner := (hscore >= winpoints)
    end
  else
    begin
      cscore:=cscore+amount;
      writeln('I''ve pegged ',cscore,' points.');
      winner := (cscore >= winpoints)
    end;
  if winner then
         inline($2A/xplayx/      { LD HL,(xplayx) ;EXIT PROC }
                $F9)             { LD SP,HL                  }
end; {addpoints}

{$I Cribbage.PS2}


function getelement:integer;
label retry;
var irank,isuit: char;
    rank: ranktype;
    suit: suitype;
    which: integer;
    index: 1..dealsize;
    many: -5..4;

    procedure getcard(VAR rankchar:char; VAR suitchar:char);
    var ch: char;
        s: str80;
        i: integer;
    begin
      repeat
        write('__',chr(8),chr(8));
        getln(s);
        rankchar:=' '; suitchar:=' ';
        for i:=1 to length(s) do
          begin
            ch:=s[i];
            if (ch in ['A','2'..'9','T','J','Q','K']) then rankchar:=ch;
            if (ch in ['S','H','D','C']) then suitchar:=ch
          end;
        if (rankchar=' ') or (suitchar=' ') then
          begin
            writeln(s,'?');
            writeln('Suits = S,H,D,C (Spades,Hearts,Diamonds,Clubs)');
            writeln('Ranks = A,2,3,4,5,6,7,8,9,T,J,Q,K  (Ace is A, 10 is T!)');
            writeln('Example: 8D (eight of Diamonds) or TH (ten of Hearts)');
            writeln;
            write('Try again from the start.  Which card? ')
          end
      until (rankchar<>' ') and (suitchar<>' ');
      writeln('{{{ ',rankchar,suitchar,' }}}');
    end;  {getcard}

begin
  retry:
  getcard(irank,isuit);
  case irank of
    'A': rank:=1;
    '2','3','4','5','6','7','8','9': rank:=ord(irank)-ord('0');
    'T': rank:=10;
    'J': rank:=11;
    'Q': rank:=12;
    'K': rank:=13
    end; {case}
  case isuit of
    'S': suit := spades;
    'H': suit := hearts;
    'D': suit := diamonds;
    'C': suit := clubs
    end; {case}
  many:=0;
  which:=0;
  for index:=1 to dealsize do
    begin
      if human[index].rank = rank then
        begin
          many:=many+1;
          if many>0 then which:=index;
          if isuit<>' ' then
          if human[index].suit = suit then
          many:=-5
        end
    end;
  if many=0 then
    begin
      writeln('What?!  No such card exists.');
      write('Which card? ');
      goto retry
    end;
  if many>1 then
    begin
      writeln('There is more than one ',irank);
      write('Please be more specific:  ');
      goto retry
    end;
  if (many=1) or (many<0) then getelement:=which;
end;  {getelement}

procedure tocrib;
var cardnum: 1..dealsize;
    numgone: 0..1;

begin
  for numgone:=0 to 1 do
    begin
      write('Throw which card? [ ');
      for cardnum:=1 to (dealsize-numgone) do showcard(human[cardnum]);
      write(' ] ');
      cardnum:=getelement;
      crib[numgone+1]:=human[cardnum];
      while cardnum <= (dealsize-1) do
        begin
          human[cardnum]:=human[cardnum+1];
          cardnum:=cardnum+1
        end;
      human[cardnum].rank:=0
    end;
end; {tocrib}

procedure sort(n:integer; var hand:handtype);
var
  touched: boolean;
  index: 1..dealsize;
  tmp: card;

begin
  repeat
    touched:=false;
    for index:=1 to (n-1) do
      if hand[index].rank > hand[index+1].rank then
        begin
          tmp:=hand[index];
          hand[index]:=hand[index+1];
          hand[index+1]:=tmp;
          touched:=true
          end
  until not touched;
end;  {sort}

{$I Cribbage.PS3}

procedure compcrib;
type
  bestrec = record
              points: integer;
              first, second: 1..dealsize
            end;
var
  tmp: handtype;
  best: bestrec;
  i,j,points: integer;

  function compscore:integer;
  var
    index,points: integer;
    num: 1..dealsize;
  begin
    num:=1;
    for index:=1 to (i-1) do
      begin
        tmp[num]:=comp[index];
        num:=num+1
      end;
    for index:=(i+1) to (j-1) do
      begin
        tmp[num]:=comp[index];
        num:=num+1
      end;
    for index:=(j+1) to dealsize do
      begin
        tmp[num]:=comp[index];
        num:=num+1
      end;
    tmp[5].rank:=0;
    compscore:=score(tmp);
  end; {function compscore}

begin {compcrib}
  best.points:=-1;
  sort(6,comp);
  for i:=1 to (dealsize-1) do
    for j:=i+1 to dealsize do
      begin
        points:=compscore;
        if points > best.points then
          begin
            best.points:=points;
            best.first:=i;
            best.second:=j
          end
      end;
  j:=1;
  for i:=1 to (best.first-1) do
    begin
      tmp[j]:=comp[i];
      j:=j+1
    end;
  for i:=(best.first+1) to (best.second-1) do
    begin
      tmp[j]:=comp[i];
      j:=j+1
    end;
  for i:=(best.second+1) to dealsize do
    begin
      tmp[j]:=comp[i];
      j:=j+1
    end;
  crib[3]:=comp[best.first];
  crib[4]:=comp[best.second];
  for i:=1 to playsize do comp[i]:=tmp[i];
end;  {compcrib}


procedure count(who: boolean);
var
  oldhuman: array[1..4] of card;
  curcount: integer;
  humcant,
  compcant: boolean;
  cnthand: array[1..8] of card;
  last: 0..2;
  cntnum: 1..8;
  lastcnt: integer;
  humleft,
  comleft: 0..playsize;
  i: -1..playsize;
  points: integer;

  function countscore(newcard: card):integer;
  var
    return: integer;
    matched, index: 0..8;
  begin
    return:=0; matched:=0;
    cnthand[cntnum]:=newcard;
    if cnthand[cntnum].rank > 10
      then curcount:=curcount+10
      else curcount:=curcount+cnthand[cntnum].rank;
    if cntnum=1 then
      begin
        cntnum:=cntnum+1;
        countscore:=0
      end
    else
      begin
        if (curcount=15) or (curcount=31) then return:=2;
        index:=cntnum;
        while index >= 2 do
          begin
            if cnthand[index].rank=cnthand[index-1].rank then
              matched:=matched+1
            else index:=1;
            index:=index-1
          end;
        case matched of
          0: ;
          1: return:=return+2;
          2: return:=return+6;
          3: return:=return+12
          end; {case}
        matched:=0;
        index:=cntnum;
        while index >= 2 do
          begin
            if cnthand[index].rank=(cnthand[index-1].rank -1) then
              matched:=matched+1
            else index:=1;
            index:=index-1
          end;
        cntnum:=cntnum+1;
        if matched > 2 then return:=return+matched+1;
        countscore:=return
    end;
  end;  {countscore}

  function humplay:integer;
  var i,j: integer;
  begin
    if human[1].rank > 10 then i:=10
    else i:=human[1].rank;
    if (humleft <= 0) or ((i+curcount) > 31) then
      begin
        humcant:=true;
        humplay:=-1
      end
    else
      begin
        last:=1;
        humcant:=false;
        if human[2].rank > 10 then i:=10
        else i:=human[2].rank;
        if (humleft=1) or ((i+curcount) > 31) then
          humplay:=1
        else
          begin
            j:=0;
            while j=0 do
              begin
                write('Play which card? [ ');
                for i:=1 to playsize do
                  if human[i].rank <> 0 then showcard(human[i]);
                  write(' ] ');
                  i:=getelement;
                  if human[i].rank > 10 then j:=10
                  else j:=human[i].rank;
                  if (j+curcount) > 31 then
                    begin
                      writeln('Sorry, that''s more than 31');
                      j:=0
                    end
              end;
            humplay:=i
          end
      end
  end;  {humplay}

  function complay:integer;
  var
    index: 1..playsize;
    points, best: integer;
    tmp: 0..10;
    return: 1..playsize;
  begin
    best:=-1;
    if comp[1].rank > 10 then tmp:=10
    else tmp:=comp[1].rank;
    if (comleft=0) or ((tmp+curcount) > 31) then
      begin
        compcant:=true;
        complay:=-1
      end
    else
      begin
        compcant:=false;
        last:=2;
        for index:=1 to comleft do
          begin
            if comp[index].rank>10 then tmp:=10
            else tmp:=comp[index].rank;
            if (tmp<>0) and ((tmp+curcount) <= 31) then
              begin
                points:=countscore(comp[index]);
                cntnum:=cntnum-1;
                curcount:=curcount-tmp;
                if points>best then
                  begin
                    best:=points;
                    return:=index
                  end
              end
          end;
        complay:=return
      end;
  end;  {complay}

begin {count -- at last!}
  humleft:=playsize;
  comleft:=playsize;
  humcant:=false;
  compcant:=false;
  last:=0;
  cntnum:=1;
  if common.rank=11 then
    begin
      if who then
        begin
          writeln('I get a point for His Nibs!');
          addpoints(false,1);
        end
      else
        begin
          writeln('YOU get a point for His Nibs!!');
          addpoints(true,1);
        end
    end;
  for curcount:=1 to playsize do
    oldhuman[curcount]:=human[curcount];
  curcount:=0;
  while (humleft > 0) or (comleft > 0) do
    begin
      if who then
        begin
          who:=false;
          i:=humplay;
          if i>0 then
            begin
              write('You played a ');
              showcard(human[i]);
              writeln('.');
              points:=countscore(human[i]);
              if points>0 then
                begin
                  writeln('You got ',points,' points');
                  addpoints(true,points);
                end;
              while i<=(playsize-1) do
                begin
                  human[i]:=human[i+1];
                  i:=i+1
                end;
              human[humleft].rank:=0;
              humleft:=humleft-1
            end
        end;
      if not who then
        begin
          who:=true;
          i:=complay;
          if i>0 then
            begin
              write('I play a ');
              showcard(comp[i]);
              writeln('.');
              points:=countscore(comp[i]);
              if points>0 then
                begin
                  writeln('I got ',points,' points.');
                  addpoints(false,points);
                end;
              while i <= (playsize-1) do
                begin
                  comp[i]:=comp[i+1];
                  i:=i+1
                end;
              comp[comleft].rank:=0;
              comleft:=comleft-1
            end
        end;
      if lastcnt<>curcount then writeln('Total is ',curcount,'.');
      lastcnt:=curcount;
      if (humcant and compcant) or ((humleft=0) and (comleft=0)) then
        begin
          case last of
            0: ;
            1: begin
                 writeln('You got a point for last card.');
                 addpoints(true,1);
                 who:=false
               end;
            2: begin
                 writeln('I got a point for last card.');
                 addpoints(false,1);
                 who:=true
               end
            end; {case}
          writeln;
          writeln('Total is now 0.');
          writeln;
          humcant:=false;
          compcant:=false;
          last:=0;
          cntnum:=1;
          curcount:=0
        end;
    end;
  for curcount:=1 to playsize do
    human[curcount]:=oldhuman[curcount];
end;  {count}


procedure play(who: boolean);
var
  cpoints,
  hpoints,
  crbpnts: integer;
  user:str80;
  usernum,code: integer;

  procedure check(num: integer; question: str80);
  begin
    repeat
      write(question);
      getln(user);
      val(user,usernum,code);
      if code<>0
        then writeln(user,'??')
        else writeln;
    until code=0;
    if usernum<>num then
      begin
        writeln('Unless there''s a bug in my program, you should have taken ',num,' points!');
        writeln('I get ',abs(num-usernum),', regardless!');
        addpoints(false,abs(num-usernum));
        if usernum>num then usernum:=num
      end;
    if (usernum > 0) then addpoints(true,usernum);
    writeln;
  end; {check}

begin {play}
  inline($21/0/0/         { LD HL,0000h   ; MARK PROC }
         $39/             { ADD HL,SP     ; FOR EXIT  }
         $22/xplayx);     { LD (xplayx),HL            }

  repeat { forever -- we get out by simulating an Exit(Play) }
      shuffle;
      deal;
      if who then writeln('It''s my crib.')
      else writeln('It''s your crib.');
      sort(6,human);
      tocrib;
      compcrib;
      sort(4,crib);
      if who then
        begin
          repeat
            write('Cut which card [1-40] ? ');
            getln(user);
            val(user,usernum,code);
            if code<>0
              then writeln(user,'??')
              else writeln
          until (code=0) and (usernum in [1..40]);
          common:=deck[usernum+12]
        end
      else
        begin
          common:=deck[13+random(40)]
        end;
      write('The UPCARD is ');
      showcard(common);
      writeln;
      cpoints:=score(comp);
      hpoints:=score(human);
      crbpnts:=score(crib);
      writeln;
      count(who);
      writeln;
      if who then
        begin
          write('[ ');
          for usernum:=1 to playsize do showcard(human[usernum]);
          write(' ]  [ ');
          showcard(common);
          writeln(' ]');
          check(hpoints,'How many points you got? ');
          writeln('I''ve got ',cpoints,' points in my hand');
          addpoints(false,cpoints);
          writeln('I have ',crbpnts,' points in my crib');
          addpoints(false,crbpnts);
        end
      else
        begin
          writeln('I''ve got ',cpoints,' in my hand');
          addpoints(false,cpoints);
          writeln;
          write('[ ');
          for usernum := 1 to playsize do showcard(human[usernum]);
          write(' ]  [ ');
          showcard(common);
          writeln(' ]');
          check(hpoints,'How many points in YOUR hand? ');
          writeln;
          write('[ ');
          for usernum:=1 to playsize do showcard(crib[usernum]);
          write(' ]  [ ');
          showcard(common);
          writeln(' ]');
          check(crbpnts,'How much in the crib? ');
        end;
      who:=not who
  until false
end; {play}


BEGIN  {MAIN}
  1:
  clrscr;
  writeln(':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::');
  writeln('::       W E L C O M E   TO  K I D D I E   K R I B B A G E !         ::');
  writeln('::                                                                   ::');
  writeln(':: Adapted from "Cribbage" in APPLE PASCAL GAMES, by Douglas Hergert ::');
  writeln(':: and Joseph T. Kalash, pages 301-349.  Sybex, 1981.                ::');
  writeln('::                                                                   ::');
  writeln(':: TURBO Pascal 1.0, Copyright 1983 by Borland Intl.    1/17/84  DCO ::');
  writeln(':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::');
  writeln;
  writeln('It''s you against me, kid!  Whoever pegs 121 points first wins!');
  writeln;

  randomize;
  makedeck;
  hscore:=0;
  cscore:=0;
  play(random(2)=0);

  writeln;
  if cscore>hscore
    then writeln('Ho Ho!!  I peg out and win this game!')
    else writeln('You pegged out and won the game!  Congratulations!');

  writeln; writeln; writeln;
  write('Do you want another game? ');
  read(kbd,ch); writeln(ch);
  if ch in ['n','N']
    then writeln('OK, see you later!')
    else goto 1
END.
