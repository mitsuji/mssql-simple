# mssql-simple: SQL Server client library implemented in Haskell

This is a Haskell implementation of client library for Microsoft SQL Server.


## Usage Example

https://github.com/mitsuji/mssql-simple-example/blob/master/app/Main.hs


## Related projects

* [mssql-simple-example](https://github.com/mitsuji/mssql-simple-example)
  : Usage example of mssql-simple  
  https://github.com/mitsuji/mssql-simple-example
  
* [ms-tds](https://github.com/mitsuji/ms-tds)
  : TDS Protocol implemented in Haskell  
  https://github.com/mitsuji/ms-tds
  

## Advantage

* ODBC independent

* Implemented only with Haskell (Independent of other languages ​​and environments)

* Supports encryption at login

* Supports simultaneous acquisition of multiple record sets

* Supports stored procedure RPC

* Supports Transaction

* 7.1 Revision 1 (SQL Server 2000 SP1 and later)

* Tested with SQL Server 2008 R2, SQL Server 2014



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

