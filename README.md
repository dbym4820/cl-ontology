# Cl-Ontology - Library to operate ontology written by hozo

未完成
## TODO


- 基本概念が持つデータを全てCLOSに落とし込む
- part, attributeを持てる陽にクラスを改造
- データ利用，解析に使いやすいようなマクロを整備
- 全てができればRoswellでコマンドラインから処理できるツールとしてリリースしたい



## What is this library

[法造](http://www.hozo.jp/hozo/)(オントロジー構築ツール)を用いて作成したオントロジーをCommonLispで活用するためのユーティリティ

## Installation

事前にquicklispのインストールが必要です


- cl-ontologyについて

```sh
mkdir /path/to/directory
cd /path/to/directory
git clone https://github.com/TomokiAburatani/cl-ontology.git
sudo add-project.sh
```


- 法造について
[こちら(http://support.hozo.jp/modules/tinyd2/index.php?id=3)](http://support.hozo.jp/modules/tinyd2/index.php?id=3)を参照ください(大阪大学知識システム研究室の法造HPへのリンクです)

## Usage

```cl
(ql:quickload :cl-ontology)

(cl-ontology:set-ontology-file "/absolute/path/to/ontology.xml")
(cl-ontology:convert-ontology)
```


## Author

* TomokiAburatani

## Copyright

Copyright (c) 2016 TomokiAburatani

## License

Licensed under the MIT License License.
