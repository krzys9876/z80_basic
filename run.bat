echo off
cls
java.exe -XX:+UseSerialGC -classpath C:\data\code\scala\z80_basic\target\scala-2.13\z80_basic-assembly-0.1.jar;C:\Users\rutak\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.8\scala-library-2.13.8.jar org.kr.scala.z80.Main input-files\tictactoe10.txt
rem java.exe -XX:+UseSerialGC -classpath C:\data\code\scala\z80_basic\target\scala-2.13\z80_basic-assembly-0.1.jar;C:\Users\rutak\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.8\scala-library-2.13.8.jar org.kr.scala.z80.Main input-files\arithmetic.txt
