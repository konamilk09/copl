# copl
CoPL のオンライン演習問題の解答を出力するプログラムです。中身は主にインタプリタと導出の出力です。
## 仕組み
1. 字句解析
2. 構文解析
3. 評価（インタプリタ）
4. 導出の出力

字句解析と構文解析は、これらのプログラムを自動生成するOCamlのプログラムを使います。

参考：https://kenichi-asai.github.io/lex-yacc/#%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%81%AE%E6%A7%8B%E6%88%90

## ファイル構成
```
┬ .gitignore
├ Makefile 
├ eval.ml // 構文木を評価し、評価結果を木に記録
├ lexer.mll // 字句の定義
├ main.ml // メインのプログラム
├ parser.mly // 文法の定義
├ syntax.ml // 構文木の定義
└ tree.ml // 導出を演習システムの形式に沿って出力
```

## 使い方
```
make
```
でビルド

```
cat t4.txt | ./ml0.o
```
でテスト入力`t4.txt`をプログラム`ml0.o`に渡して実行できます。

プログラムの名前は`Makefile`の
```
RESULT = ml0.o
```
の右辺を変更してください。

### 入力
`<式> evalto <数字>`の形の入力を受け取ります。現在対応しているものは、加減乗算の式のみです。
#### 入力例
```
(4+5)*(1-10) evalto -81
```
#### 出力例
```
Parsed : ((4+5)*(1-10)) evalto -81
Evaluated : ((4+5[9])*(1-10[-9])[-81]) evalto -81
Tree :
((4+5)*(1-10)) evalto -81 by E-Times {
  (4+5) evalto 9 by E-Plus {
    4 evalto 4 by E-Int {};
    5 evalto 5 by E-Int {};
    4 plus 5 is 9 by B-Plus {};
  };
  (1-10) evalto -9 by E-Minus {
    1 evalto 1 by E-Int {};
    10 evalto 10 by E-Int {};
    1 minus 10 is -9 by B-Minus{};
  };
  9 times -9 is -81 by B-Times{};
};
```
