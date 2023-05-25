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

**Note.** *It would be nice to add support for macOS, but I just don't have a Mac.*

Files
-----
The module `keyboardlayout.pas` contains this function and the auxiliary routines necessary for its operation. I tested it under Windows 10 and ALT Linux 10.

Folder `demo` belong to the demo project that illustrates the use of this procedure.

Dependencies
------------
Nothing special, just the default LCL package. Tested on Lazarus 2.2.4 and 2.2.6, should work on earlier (more or less modern) and later versions.

Demo
----
The demo project can be compiled under both Windows and GNU/Linux (the left screenshot is from Windows 10, the right one if from ALT Linux 10 with GNOME shell). Unfortunately I can't test it on macOS.

![Demo Windows](demo_windows.png) ![Demo AltLinux](demo_altlinux.png)

On Windows, it uses system hooks to intercept keyboard layout changes.

But there are no system-wide messaging mechanisms in GNU/Linux (or I don't know about them), and the need to work in different desktop environments makes the task unsolvable. Therefore, I used the standard TTimer component. Once per second, it checks the current layout and writes it to the label caption.

**Note** *I understand that using a timer might be a bad practice here, but I don't know of a better way because my Linux programming experience is relatively limited. Let me know if you have a better solution.*

Delphi compatibility
--------------------
The `keyboardlayout.pas` file is also compatible with Delphi. To use it with Delphi on Windows, remove the {$mode ObjFPC}{$H+} line, all {$IF defined(WINDOWS)} lines, and all lines between the associated {$ELSE} and {$ENDIF}, including themselves. You may also need to fix the module names in the section 'uses' (depending on the version of Delphi).

The demo project is incompatible with Delphi due to different form formats (`dfm` / `lfm`). You can recreate the form and use the source code.

Author
------
Copyright (c) 2023, Michael Demidov

Visit my GitHub page to check for updates, report issues, etc.: https://github.com/MichaelDemidov

Drop me an e-mail at: michael.v.demidov@gmail.com
