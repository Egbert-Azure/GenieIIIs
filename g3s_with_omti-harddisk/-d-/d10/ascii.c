/*  ASCII.C  -->  Programm zur Ausgabe der ASCII-Code-Tabelle.  */

# include  <stdio.h>
main()
{
     char answer;  int upper, ac = 32;    /* ab ASCII-Code 32   */
                                          /* ohne Steuerzeichen */
     while(1)
     {
        printf("\n\nZeichen    Dezimal    Hexadezimal\n\n");
        for( upper = ac + 20 ; ac < upper && ac < 256 ; ++ac)
           printf("   %c  %10d  %10X\n", ac, ac, ac);

        if( upper >= 256)  break;

        printf("\nWeiter --> <Return>,   Ende --> <q>+<Return> ");
        scanf("%c", &answer);
        if( answer == 'q' || answer == 'Q')  break;

        fflush( stdin);                /* Eingabepuffer l|schen */
     }
}
