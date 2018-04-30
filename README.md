# console


So here is the rundown of the Windows console character encoding disasterous situation.

Here we assume that Windows default locale is English (United States), but it could just as well be
any other locale.

## From the console

First of all I'll try to `echo` my name in Russian in both `Powershell` and `cmd`:

* Powershell

```
PS C:\Users\lehins> chcp
Active code page: 437
PS C:\Users\lehins> echo ???????
???????
```

So, first though is to change the code page:

```
PS C:\Users\lehins> chcp 65001
Active code page: 65001
PS C:\Users\lehins> echo
out-lineoutput : The Win32 internal error "A device attached to the system is not functioning" 0x1F occurred while
writing to the console output buffer at the current cursor position. Contact Microsoft Customer Support Services.
    + CategoryInfo          : WriteError: (:) [out-lineoutput], HostException
    + FullyQualifiedErrorId : WriteConsole,Microsoft.PowerShell.Commands.OutLineOutputCommand
```

Nope, the problem is with the "Raster Fonts" that can't cope with Unicode characters and everything
just blows up in your face. There is no way around it and font must be changed to "Lucida Console"
manually if anything but CP437 to be displayed. Can be easily done by right clicking on the console
window header and further clicking Properties.

Let's change the code page back and try to echo unicode with new font set:

```
PS C:\Users\lehins> chcp 437
Active code page: 437
PS C:\Users\lehins> echo Алексей
Алексей
```

Cool, that worked. But if we are to pipe it into anything it will break again, regardless of the
code page.

```
PS C:\Users\lehins> echo Алексей | sed s/й/i/
???????
PS C:\Users\lehins> chcp 65001
Active code page: 65001
PS C:\Users\lehins> echo Алексей | sed s/й/i/
???????
```

Situation is slightly different with `cmd`, namely setting code page to utf-8, fixes the issue with
piping:

```
C:\Users\lehins> chcp 437
Active code page: 437
C:\Users\lehins> echo Алексей | sed s/й/i/
???????
C:\Users\lehins> chcp 65001
Active code page: 65001
C:\Users\lehins> echo Алексей | sed s/й/i/
Алексеi
```

This little experiment tells us that there is some different that happens between printing to
console and into a handle that is also a pipe.

## From Haskell


Let's continue with experiment and try to print my name from `ghci`:

```
PS C:\phab\console> chcp
Active code page: 437
PS C:\phab\console> ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude> putStrLn "Алексей"
*** Exception: <stdout>: hPutChar: invalid argument (invalid character)
```

Clearly handle encoding is the issue, let's fix that:

```
Prelude> import System.IO
Prelude System.IO> hSetEncoding stdout utf8
Prelude System.IO> putStrLn "Алексей"
╨É╨╗╨╡╨║╤ü╨╡╨╣
```

Well, that removes the error, but we still get gibberish. How about something like `chcp`, but from
Haskell:

```
Prelude System.IO> import System.Win32.Console
Prelude System.IO System.Win32.Console> setConsoleOutputCP 65001
Prelude System.IO System.Win32.Console> putStrLn "Алексей"
А�л�е�к�с�е�й�
```

There is some progress, but still not quite right. How about compiling and running it.

```
main :: IO ()
main = do
  oldCP <- getConsoleOutputCP
  setConsoleOutputCP 65001
  hSetEncoding stdout utf8
  IO.putStrLn "Алексей"
  setConsoleOutputCP oldCP
```

Note that we need to restore the code page before exiting, since it is a global setting for the
cosole session. That results in even more mysterious output:

```
PS C:\phab\console> stack --resolver nightly-2018-04-29 exec -- console
Алексей
�ей
```

As it turns out this issue has been known for millenia and there is no explanation for it. From one
of [blogposts online](https://www.windowsperl.com/2014/02/20/dealing-with-code-pages/) on the
subject (in that case it was with Perl):

> I suspect this is a bug in the Window’s console handling of UTF-8, and that CP-65001 is part of
> the issue. It’s not just Command Prompt that has this issue; WinBash shows the same
> behavior. Various people have made vague references to issues with CP-65001 but never got down to
> the actual issue, which I think is very deep in the architecture. Part of this is the console’s
> inability to use a proper font.

Other places that mention this issue and the upcoming solution:

* https://stackoverflow.com/questions/33308276/issues-with-windows-command-line-using-utf-8-code-page
* https://stackoverflow.com/questions/1259084/what-encoding-code-page-is-cmd-exe-using

Luckily for us this behavior is only present when printing to console, but not when it is being
piped:

```
PS C:\phab\console> stack --resolver nightly-2018-04-29 exec -- console | echo
Алексей
```

I say luckily because there is a Windows API Call `WriteConsoleW` that we can use to print directly
into the console. So it seems that the proper workaround for this "beautiful" Microsoft feature is
to:

* Attempt to write the buffer into the handle using `WriteConsoleW`, which will succeed if it is a
  console.
* If above fails, it means the handle wasn't a console in the first place so we can safely write it
  in our usual way while setting the encoding on the handle to utf8.


## Implemented solution

So, by using Windows API now we can consistently print to console without worrying about the code
page.


* In Powershell:

```
PS C:\phab\windows-console> chcp
Active code page: 437
PS C:\phab\windows-console> stack exec -- console
Алексей Кулешевич
```

* In `cmd`:

```
C:\phab\windows-console> chcp
Active code page: 437

C:\phab\windows-console> stack exec -- console
Алексей Кулешевич
```
