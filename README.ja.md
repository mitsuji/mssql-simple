# mssql-simple: SQL Server client library implemented in Haskell

このライブラリは Microsoft SQL Server のクライアント機能の
Haskellによる実装です。


## 使用例

https://github.com/mitsuji/mssql-simple-example/blob/master/app/Main.hs


## 関連プロジェクト

* [mssql-simple-example](https://github.com/mitsuji/mssql-simple-example)
  : Usage example of mssql-simple  
  https://github.com/mitsuji/mssql-simple-example
  
* [ms-tds](https://github.com/mitsuji/ms-tds)
  : TDS Protocol implemented in Haskell  
  https://github.com/mitsuji/ms-tds
  

## 特徴

* ODBC 非依存

* Haskell だけで実装 (他の言語・環境のライブラリ 非依存)

* ログイン時の暗号化に対応

* 複数のレコードセットの同時取得に対応

* ストアドプロシージャのRPCに対応

* トランザクション処理に対応

* 7.1 Revision 1 対応 (SQL Server 2000 SP1 以降)

* SQL Server 2008 R2, SQL Server 2014 でテスト



## Todo

* Write tests
  * Write general tests
  

* ETC
  * Encrypt entire connection support
  * Mars support
  * SSPI support
  * FedAuth support
  * TDS protocol versions other than 7.1

  * Bulk Load  
    https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/88176081-df75-4b24-bcfb-4c16ff03cbfa

  * Distributed Transaction  
    https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/b4b78564-5440-4fc0-b5ef-c9e1925aaefe

