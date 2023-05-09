Lazarus Keyboard Layout
=======================

What is it
----------

I once tried to create a convenient login window for users who have more than one keyboard layout. This is a very common situation when an user forgets to switch the layout before entering a login or password. The OS-wide keyboard layout indicator is usually located in the corner of the screen, and the user must look here before typing. This is inconvenient, so I wanted to add the indicator to the login window itself so that it is in the front of the user's eyes.

Also, this was cross-platform application (Windows + GNU/Linux) written in FreePascal / Lazarus and the indicator should work on both OSes.

A core of it's algorithm is a cross-platform (Windows + GNU/Linux, I think, it should also work under all OSes with X11 support like FreeBSD) function to retrieve the current keyboard locale from the OS in human-readable form ('US', 'RU', etc.):

``` delphi
function GetKeyboardLayoutAbbr: string;
```

Files
-----
The module `keyboardlayout.pas` contains this function and the auxiliary routines necessary for its operation. I tested it under Windows 10 and ALT Linux.

Folder `demo` belong to the demo project that illustrates the use of this procedure.

![Demo Windows](demo_windows.png) ![Demo AltLinux](demo_altlinux.png)

Dependencies
------------
Nothing special, just the default LCL package. Tested on Lazarus 2.2.4 and 2.2.6, should work on earlier (more or less modern) and later versions.

Delphi compatibility
--------------------
It is also compatible with Delphi. To use it with Delphi on Windows, remove the {$mode ObjFPC}{$H+} line, all {$IF defined(WINDOWS)} lines, and all lines between the associated {$ELSE} and {$ENDIF}, including themselves. You may also need to fix the module names in the section 'uses' (depending on the version of Delphi).

How to make a visual keyboard layout indicator
----------------------------------------------
It's nearly impossible to keep track of all the ways a user can switch keyboard layouts. He can use a keyboard shortcut, click on the indicator, or use a third-party software. And requirement of cross-platform makes the task unsolvable.

So I used a standard TLabel component and a TTimer. Once per second, the timer checks the current layout and writes it to the label caption.

Author
------
Copyright (c) 2023, Michael Demidov

Visit my GitHub page to check for updates, report issues, etc.: https://github.com/MichaelDemidov

Drop me an e-mail at: michael.v.demidov@gmail.com
