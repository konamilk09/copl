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
let . = true in let . = 4 in if #2 then #1 + 1 else #1 - 1
```
#### 出力例
```
|- let . =true in let . =4 in if #2 then (#1 + 1) else (#1 - 1) evalto 5 by E-Let {
  |- true evalto true by E-Bool {};
  true |- let . =4 in if #2 then (#1 + 1) else (#1 - 1) evalto 5 by E-Let {
    true |- 4 evalto 4 by E-Int {};
    true, 4 |- if #2 then (#1 + 1) else (#1 - 1) evalto 5 by E-IfT {
      true, 4 |- #2 evalto true by E-Var {};
      true, 4 |- (#1 + 1) evalto 5 by E-Plus {
        true, 4 |- #1 evalto 4 by E-Var {};
        true, 4 |- 1 evalto 1 by E-Int {};
        4 plus 1 is 5 by B-Plus {};
      };
    };
  };
};
```
