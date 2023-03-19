
/* Sektor mit der Belegung der Funktionstasten ins RAM laden */

#include <bios3.h>

#define DISK 'C'
#define TRK  0
#define SECT 2

#define SEC_SIZE 512

char systab[3 * SEC_SIZE];  /* table of system constants   */

int lies(char disk, int track, int sector)
{
  if(bios(SELDSK,disk)==0)
  {
    printf("Laufwerk nicht vorhanden !\n");
    exit();
    }
  else
  {
    bios(SETTRK,track);
    bios(SETSEC,sector);
    bios(SETDMA,systab);
    return(bios(READ));
    }
}

main()
{
  if(lies(DISK-65,TRK,SECT))
  {
    printf("\nFehler beim Lesen von Laufwerk %c: !!\n",DISK);
    exit();
    }
  else
  {
    printf("Spur %d Sektor %d des Laufwerks %c: wurde in\n",TRK,SECT,DISK);
    printf("den Hauptspeicher an die Adresse %x geladen\n",systab);
    }
}	
