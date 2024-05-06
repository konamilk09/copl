# copl
## 使い方
```
make
```
でビルド

```
cat test.txt | ./namelessml3.o
```
でテスト入力`test.txt`をプログラム`namelessml3.o`に渡して実行できます。

プログラムの名前は`Makefile`の
```
RESULT = namelessml3.o
```
の右辺を変更してください。

### 入力
`<式>`の形の入力を受け取ります。それ以外は
```
Fatal error: exception Stdlib.Parsing.Parse_error
```
となります。\\
式は、[教科書](https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/index.cgi)の`EvalNamelessML3`に対応しています。
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
