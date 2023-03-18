/* program bench1(output);
var i,j:integer;
begin
     writeln(^Z);
     writeln('Benchmarktest');
     writeln('1 Million Schleifendurchl{ufe !!!!');
     for i:= 0 to 1000 do
         for j:= 0 to 1000 do;
     writeln('Fertig !!!');
end.   */

int i,j;
main()
{
	putch(0x1a);
	printf("Benchmarktest\n ");
	printf("1 Million Integerschleifendurchlaufe !!!!!!\n\a");
	for ( i = 0; i < 1000; i++)
	{
		for ( j = 0; j<1000; j++)
		{};
	}
	printf("\a Fertig !!");
}
			 