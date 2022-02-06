% Installation

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Next: [2. Compilation](A2_Compilation.html)


# 1.1 How to install

Installation is done by compiling the source code with a C compiler.
There are three ways to compile the source code: by script, by autoconf, and by hand.
Another page shows how to compile manually.
If the method on this page doesn't work, please see that page, 
[2. Compilation](A2_Compilation.html).


First, download the source from github.

```
$ git clone https://github.com/nptcl/npt.git
$ cd npt
```


# 1.2 Compilation by Script

Compilation scripts for FreeBSD and Linux are available.  
To compile on FreeBSD, execute the following steps

```
$ ./freebsd_release.sh
```

To compile on Linux, install `gcc` and `gmake` beforehand.  
Execute the following steps

```
$ ./linux_release.sh
```

In both cases, the executable file `npt` is created.


# 1.3 Compilation by Batch File

Compilation batch file for Windows are available.
It has been tested to work with Visual Studio 2017.  
In order to compile, the following commands must be available.

- `CL.EXE`
- `LINK.EXE`
- `NMAKE.EXE`

Execute the following steps

```
> windows_release.bat
```

The executable file `npt` is created.
In some cases it may not work.  
If this fails, check [2. Compilation](A2_Compilation.html) and compile manually.


# 1.4 Compilation by autoconf

To compile with `autoconf`, run `bootstrap.sh` to generate file `configure`.


```
$ ./bootstrap.sh
```

After the file is generated, run `configure`.

```
$ ./configure
```

If you want to set the installation location, specify `--prefix`.

```
$ ./configure --prefix=/usr/local
```

Run `make` to compile.

```
$ make
$ make install
```

An executable file named `npt` is created when `make` is executed.  
Execute `make install` and the executable file `npt` is copied to `--prefix`.


# 1.5 Installing the Executable File

If you have scripted compilation, copy the executable file `npt`
to the location of your choice and the installation is complete.
For example

```
$ ./freebsd_release.sh
...
$ cp -i npt /usr/local/bin/.
```

In the case of `configure`, `make install` copies `npt`
to the location specified by `--prefix`.

The installation copies only one file, the executable `npt`.  
If you want to uninstall, just delete the `npt` file.


# 1.6 `npt` example

When `npt` is launched, the prompt changes to `*`.

```
$ npt
*
```

Enter a Lisp expression and you will get the answer.

```
* (+ 10 20 30)
60
*
```

To exit, press Ctrl+D or execute `(exit)`.

```
* (exit)
$
```

To load a file, specify `--load` or `--script` as an argument.

```
$ cat > aaa.lisp
(format t "Hello~%")
^D
$ npt --script aaa.lisp
Hello
$ npt --load aaa.lisp
Hello
* (quit)
$
```

See `--help` for details.

```
$ npt --help
```
