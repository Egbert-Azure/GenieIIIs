#
# Small c Interpreter command shell
#
# The library functions:
#
putchar(c){return sys(c,1)}
getchar(){return sys(2)}
puts(b){sys(b,1,3)}
gets(b){return sys(b,0,4)}
fputs(b,u){sys(b,u,3)}
fgets(b,u){return sys(b,u,4)}
sprintf(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
   {return sys(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,5)}
printf(a0,a1,a2,a3,a4,a5,a6,a7,a8)
   {char b[128];sys(b,a0,a1,a2,a3,a4,a5,a6,a7,a8,5);puts(b)}
sscanf(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
   {return sys(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,6)}
atoi(b){int v[1];sys(b,"%d",v,6);return v[0]}
fopen(f,m){return sys(f,m,7)}
fread(s,l,u){return sys(s,l,u,8)}
fwrite(s,l,u){return sys(s,l,u,9)}
fclose(u){return sys(u,10)}
exit(){sys(11)}
stmt(l,p,s){return sys(l,p,s,12)}
totok(s,b){return sys(s,b,13)}
untok(b,s){return sys(b,s,14)}
edit(l,p){return sys(l,p,15)}
strcmp(s,t){return sys(s,t,16)}
strncmp(s,t,n){return sys(s,t,n,16)}
coreleft(){return sys(17)}
malloc(n){return sys(n,18)}
free(p){sys(p,19)}
load(f,p){return sys(f,p,20)}
save(f,p){return sys(f,p,21)}
list(p){return sys(p,22)}
trace(n){sys(n,23)}

#
# Entry point to the shell. All globals defined before the "entry" keyword
# are in the "library" and are accessible by user programs.
#
entry

int size,top;
char line[80];
char program[16000];

main()
{
   int from, to;

   top=16000;
   program[0] = 90;   # This is an "End of program" token - required
   size=1;

   # print sign-on message
   printf( "%s\nShell V1.1, 23 OCT 1985\n", sys(0) );

   while(1)
   {
      puts("> ");
      if(gets(line))
      {
         if (!strncmp(line,"edit",4))
            size = sys(atoi(line+5),program,15); # envoke the editor
         else if (!strncmp(line,"save ",5))
            sys(line+5,program,21);              # save the program buffer
         else if (!strncmp(line,"load ",5))
            size = sys(line+5,program,20);       # load the program buffer
         else if (!strncmp(line,"list",4))
         {
            if(line[4])
               sscanf(line+4,"%d %d",&from,&to);
            else
            {
               from=1;
               to=32765;
            }
            sys(program,from,to,22);           # list the program buffer
         }
         else if (!strncmp(line,"exit",4))
            return;                              # return to previous shell
         else if (!strncmp(line,"free",4))
            printf("%d\n",top-size);             # show amount of free space
         else
            #
            # attempt to parse the line as a small c statement. Note that
            # we will always display the results of the statement, so you
            # could enter something like: 2+2 and a 4 would be printed.
            #
            printf("%d\n", sys(line,program,size,12) );
      }
   }
}
