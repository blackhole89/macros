== Windows installation instructions ==

The following guide is due to Reddit's [/u/zom-ponks](https://www.reddit.com/r/cpp/comments/8i3ya3/a_more_powerful_macro_preprocessor_for_cc/dyoyizq/).

* Have git installed (https://git-scm.com/)
* You need either Visual Studio or mingw/cygwin installed to build 
    * MinGW: https://nuwen.net/mingw.html
    * Visual Studio: https://www.visualstudio.com/ (Community Edition is free)
* Test on the command line: `nmake` for Visual Studio or `make` for MinGW
    * for VS use "Tools &gt; Visual Studio Command Prompt" for this
* Install Haskell, "Full" installer from here https://www.haskell.org/platform/windows.html
    * In the "full" install, the prerequisites are already installed, otherwise use cabal-install to fetch those (mtl and parsec)
    * Use defaults unless you know what you're doing
        * Test: run `ghc` on the command line (restart console for environment changes to take effect)
* Now clone the repository somewhere convenient 
    * For instance: 
        * `cd c:\\dev`
        * `git clone https://github.com/blackhole89/macros.git`
    * Build:
        * Visual Studio (again, from the "Visual Studio Command Prompt")
            * `cd \\dev\\macros`
            * `nmake -f Makefile`
        * MinGW
            * `cd \\dev\\macros`
            * `make`
* Now you should have a "macros.exe" in the root of the project(in this example `c:\\dev\\macros\\`)
    * Copy it somewhere that's in PATH
        * `mkdir \tools`
        * `copy macros.exe \tools`
        * `set PATH=%PATH%;C:\tools` (this is not permanent, use computer properties to set this always)
* Run `macros`, read the documentation at https://github.com/blackhole89/macros


