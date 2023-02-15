# Windows MAKEFILE Tutorial

## Introduction

As you may know make is a UNIX program that's commonly used to automate build systems. Sadly it is not available on windows.

Luckily there have been ports made for windows and today I will walk you through how to install make in just 5 minutes. All I ask is that if this video helps you please leave a like and subscribe for more educational programming content. I have left all related links in the description down below as well as a step by step guide if you want to follow that instead.

 So lets get started by opening our browser and searching for mingw-get.



1) Google MinGW-GET

   1) Click first link
   2) https://sourceforge.net/projects/mingw/files/Installer/mingw-get/

2) Then click the big green Download Latest Version Button

   1) https://sourceforge.net/projects/mingw/files/latest/download

3) Run the installer program

4) Click Install 

5) Make sure the installation folder has no spaces and that you remember where it will be installed.

6) Uncheck the GUI option if you please

7) After the installer finishes go to the installation directory in File Explorer

8) Copy the bin folder's location to your clipboard and add it to the systems PATH.

9) If all is successful open a new Terminal/PowerShell:

   1) ```bash
      mingw-get
      ```

   2) If that shows a popup window then all is good.

   3) Next install mingw32-make 

   4) ```bash
      mingw-get install mingw32-make
      ```

10) Next lets navigate back to the folder and change the name of the executable to make
11) ALL FINISHED