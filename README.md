# cl-ontology - Library to operate ontology written by hozo

[![Build Status](https://travis-ci.com/dbym4820/cl-ontology.svg?branch=develop)](https://travis-ci.com/dbym4820/cl-ontology)

## Status

In progress

## What is this library

[法造](http://www.hozo.jp/hozo/)(オントロジー構築ツール)を用いて作成したオントロジーをCommonLispで活用するためのユーティリティ

## Requirement

- roswell
- quicklisp
- hozo

## Installation

- cl-ontology

```
$ ros install dbym4820/cl-ontology
```

- 法造について
[こちら(http://support.hozo.jp/modules/tinyd2/index.php?id=3)](http://support.hozo.jp/modules/tinyd2/index.php?id=3)を参照ください(大阪大学知識システム研究室の法造HPへのリンクです)

## Usage

- As Common Lisp library

```cl
CL-USER> (ql:quickload :cl-ontology :silent t)
T

CL-USER (cl-ontology:convert-ontology "/absolute/path/to/ontology.xml")
(<concept-name-list> ...)

CL-USER> (mapcar #'(lambda (d)
              	(show-attribute :role-name d))
                 (show-attribute :proper (dusque:find-concept "アニメ")))
```

- As roswell script on shell

```
? clon アニメ --proper
```


## Author

* TomokiAburatani

## Copyright

Copyright (c) 2018 TomokiAburatani

## License

Licensed under the MIT License License.
