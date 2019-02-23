# Npt

Npt is a small Lisp Programming Language.

## Compile

FreeBSD
```sh
$ tar zxf npt-x.x.x.tar.gz
$ cd npt-x.x.x
$ ./bsd_release.sh
...
$
```
Linux
```sh
$ tar zxf npt-x.x.x.tar.gz
$ cd npt-x.x.x
$ ./linux_release.sh
...
$
```

## Install

```sh
$ su -
# cp -i npt /usr/local/bin/.
```

## Example

Prompt
```sh
$ ./npt
* (format nil "Hello~%")
Hello
* ^D
$
```

Command Line
```sh
$ ./npt --quit --eval '(format t "Hello~%")'
Hello
$
```

## License

[The Unlicense](LICENSE)

## Distribution

https://github.com/nptcl/npt

