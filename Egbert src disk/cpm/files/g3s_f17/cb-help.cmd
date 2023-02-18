* Program.: CB-HELP.CMD (.PRG)
* Author..: Stephen Kurasch
* Date....: February 9, 1984
* Notice..: Copyright 1984, Ashton-Tate and RSP, Inc.
* Notes...: Help screens called from CB-MENU.CMD

ERASE
SET COLON OFF
?
TEXT
     Welcome to the Checkbook Management System.  This system is designed to
keep track of your deposits, your checks, balancing your checkbook and print-
ing reports of several combinations.  Listed below are the menu selections
and a description of each.

     (1) Enter new checks:
         This selection will allow the user to enter checks.  The special
         features of this entry are checking for duplicate check numbers,
         checking for a valid date, not allowing a blank check, making sure
         that a positive, non-zero amount exists, and categorizing the check
         into 8 different tax deductible fields (if needed).

     (2) Enter deposits:
         This selection is for entering a deposit slip.  The special features
         included here are keeping track of the individual deposits as well as
         the entire deposit slip itself.  It checks for positive, non-zero
         amounts and provides a place to deduct cash withdrawals.
ENDTEXT

STORE " " TO continue
@ 23,25 SAY "Press any key to continue  " GET continue
READ

ERASE
?
TEXT
     (3) Enter cancelled checks:
         This selection allows you to cancel the checks you received with
         your bank statement.  It checks to make sure that the check has
         already been cancelled or that the check is on file.

     (4) Entered cleared deposits:
         This selection allows you to clear the deposits that are listed on
         the bank statement.

     (5) Reconcile bank statement:
         This selection is a bank reconciliation.  Preparing a bank recon-
         ciliation means determining those items which make up the difference
         between the balance on the bank statement and the balance in the
         checkbook.  We must deduct from our records any charges that been
         made by the bank.  We must add to our records any amount that
         the bank has credited to our account.  This includes interest
         earned, or if you are lucky enough, a bank note that has come
         due to you.  As in (3) and (4) above, these must be taken into
         account on the banks records.
ENDTEXT

STORE " " TO continue
@ 23,25 SAY "Press any key to continue " GET continue
READ

ERASE
?
TEXT
     (6) Reports:
         This selection will print several combinations of reports. These
         include listings of all checks, of all taxable checks, and of all
         deposits between two dates.  All reports can be listed on the
         screen or to the printer.


                               Have fun!!!
ENDTEXT

STORE " " TO continue
@ 23,17 SAY "Press any key to return to the main menu " GET continue
READ

SET COLON ON
RELEASE continue
RETURN

