char *Lineptr;
int Stack[ 10 ], Stackptr, Stacktop;

calc()
{
   char line[ 80 ];

   Stacktop = 10;
   while(1)
   {
      puts("-> ");
      if ( gets(Lineptr=line) )
      {
         if( *Lineptr=='x' )
            return;
         addition();
         printf( "%d\n",pop() );
      }
   }
}

number()
{
   if( isdigit( *Lineptr ) )
   {
      push( atoi( Lineptr ) );
      while( isdigit( *Lineptr ) )
         Lineptr = Lineptr+1;
   }
}

addition()
{
   int num;

   while(1)
   {
      multiplication();
      if( *Lineptr=='+' )
      {
         Lineptr = Lineptr+1;
         multiplication();
         push( pop() + pop() );
      }
      else if( *Lineptr=='-' )
      {
         Lineptr = Lineptr+1;
         multiplication();
         num = pop();
         push( pop() - num );
      }
      else
         return;
   }
}

multiplication()
{
   int num;

   while(1)
   {
      number();
      if( *Lineptr=='*' )
      {
         Lineptr = Lineptr+1;
         number();
         push( pop() * pop() );
      }
      else if( *Lineptr=='/' )
      {
         Lineptr = Lineptr+1;
         number();
         num = pop();
         push( pop() / num );
      }
      else
         return;
   }
}

push( n )
{
   if( Stackptr<Stacktop )
   {
      Stack[ Stackptr ] = n;
      Stackptr = Stackptr+1;
      return n;
   }
   puts( "stack overflow\n" );
}

pop()
{
   if ( Stackptr>0 )
   {
      Stackptr = Stackptr-1;
      return Stack[ Stackptr ];
   }
   puts( "stack underflow\n" );
}

isdigit( c )
char c;
{
   return '0'<=c && c<='9';
}
tion