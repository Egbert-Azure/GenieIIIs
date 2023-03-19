#include <stdio.h>
#include <time.h>
#include <string.h>
extern void exit (int);

main (int argc, char ** argv)
{
#define PGM "manchmal" 
	FILE * fp;
	long clock,count,newcount,limit;
	int use_days,i,exit_status=0;
	char text [500], *filename;
	
	if ((argc>1) && (strcmp (argv[1],"-t")==0))
	{
		use_days = 1;
		-- argc;
		++ argv;
	}
	else use_days = 0;	
	if (argc<3)
	{
		fprintf (stderr,"usage: %s [-t] Zahl Datei Text...\n",PGM);
		exit (0);
	}
	
	text [0] = '\0';
	for (i=3; (i<argc) && (strlen(text)+
	     strlen(argv[i])+2<sizeof(text)); i++)
	{
		strcat (text,argv[i]);
		strcat (text," ");
	}
	
	sscanf (argv[1],"%ld",&limit);
	filename = argv[2];
	count = 0L;
	if (fp = fopen (filename,"r"))
	{
		fscanf (fp,"%ld",&count);
		fclose (fp);
	}
	if (!use_days)
	{
		if (count<=1L || count>limit)
		{
			newcount=limit;
			printf ("%s: %s\n",PGM,text);
			exit_status = 0;
	} else {
			newcount = count-1;
			exit_status = 1;
		}
	}
	else
	{
		clock = time (&clock);
		newcount = clock / (24L*60L*60L);
		if (newcount>=count+limit || newcount < count)
	{
		printf ("%s: %s\n",PGM,text);
		exit_status = 0;
	} else {
		newcount = count;
		exit_status = 1;
		}
	}
	if ((fp = fopen (filename,"w")))
	{
		fprintf (fp,"%ld\n%s\n",newcount,text);
		fclose (fp);
	}
	else fprintf (stderr,"%s: Kann '%s' nicht schreiben\n",
	              PGM,filename);
	
	exit (exit_status); return (exit_status);
}
