min-lift
=====

[![Build Status](https://travis-ci.org/prg-titech/min-lift.svg?branch=master)](https://travis-ci.org/prg-titech/min-lift)

a subset implementation of [LIFT](https://github.com/lift-project/lift)

```shell
$ docker build -t min-lift .
$ make
$ sudo nvidia-docker run --rm -v (pwd):/min-lift -it min-lift /min-lift/runtime FILE INPUT_DATA
```
