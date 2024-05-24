evalML2まではブランチにあります。

# copl
CoPL のオンライン演習問題の解答を出力するプログラムです。中身は主にインタプリタと導出の出力です。
## 仕組み
1. 字句解析
2. 構文解析
3. 評価（インタプリタ）と導出
4. 出力

字句解析と構文解析は、これらのプログラムを自動生成するOCamlのプログラムを使います。

参考：https://kenichi-asai.github.io/lex-yacc/#%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%81%AE%E6%A7%8B%E6%88%90

## ファイル構成
```
┬ .gitignore
├ Makefile 
├ eval.ml // 構文木を評価し導出を推論する
├ lexer.mll // 字句の定義
├ main.ml // メインのプログラム
├ parser.mly // 文法の定義
└ syntax.ml // 構文木の定義
```

## 使い方
```
make
```
でビルド

```
cat test.txt | ./ml3.o
```
でテスト入力`test.txt`をプログラム`ml3.o`に渡して実行できます。

プログラムの名前は`Makefile`の
```
RESULT = ml3.o
```
の右辺を変更してください。

### 入力
`<式>`の形の入力を受け取ります。それ以外は
```
Fatal error: exception Stdlib.Parsing.Parse_error
```
となります。\\
式は、[教科書](https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/index.cgi)の定義に対応しています。
#### 入力例
```
1 - 10
```
#### 出力例
```
|- (1-10) evalto -9 by E-Minus {
  |- 1 evalto 1 by E-Int {};
  |- 10 evalto 10 by E-Int {};
  1 minus 10 is -9 by B-Minus {};
};
```
