--Tahel Nadav
--Tzviya Laniado
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List
--import Data.IORef
--import System.Directory
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}


echoFile = do
          writeFile "C:\\Users\\tahel\\Desktop\\files_0\\output.asm" ""
          ia <- openFile "C:\\Users\\tahel\\Downloads\\InputA.vm" ReadMode
          ib <- openFile "C:\\Users\\tahel\\Downloads\\InputB.vm" ReadMode
          let filenamea=(split "C:\\Users\\tahel\\Downloads\\InputA.vm")!!4
          let filenameb=(split "C:\\Users\\tahel\\Downloads\\InputB.vm")!!4
          contenta<-hGetContents ia
          let allinesa=lines contenta
         -- let count=length allinesa 
          contentb<-hGetContents ib
          let allinesb=lines contentb
          let a=[filenamea]++allinesa
          let b=[filenameb]++allinesb
          --let count=length allinesb
         -- let result=[somefunc(x)|x<-allines]
         {-} let linefromfile="BUY breed 2 13.0" 
          let l=words linefromfile
          let productname=l!!1
          let temp1=(l!!2)
          let temp2=(l!!3)
          --let amount=read temp1
          --let price =read temp2
          --handleBuy productname amount price-}
          let totallist=[0,0]
          let end=a++b
          linesinfile end totallist
          --print (stam)
          
split :: String -> [String]
split [] = [""]
split (c:cs) | c == '\\' = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

                         
                           



checkfunc::String -> Double->[Double]->[Double]
checkfunc n mult listt=
  if(n=="buy")
    then do
    let b=listt!!0
    let s=listt!!1
    [b+mult,s]
  else
  do
    let b=listt!!0
    let s=listt!!1
    [b,s+mult]
   

--handleBuy func 
handleBuy ::[Char ] -> Int ->Double->IO ()
handleBuy productName amount price = do
    
    let sum = fromIntegral( amount) * price
    --let s=totallist!!1
    --let b=totallist!!0
    --let totallist=[b+sum,s]
    let st="### BUY "++productName++" ###"++"\n"
    let s=show(sum)
   -- f <- openFile "C:\\Users\\tahel\\Downloads\\InputA.vm" ReadMode
    --content<-hGetContents f
   {- let g= words content
    let v= g!!0
    let n=read v
    let p=sum+n
    let l=show(p)
    writeFile "C:\\Users\\tahel\\Desktop\\files_0\\buys.asm" l-}
    let s1=s++"\n"
    appendFile "C:\\Users\\tahel\\Desktop\\files_0\\output.asm" st
    appendFile "C:\\Users\\tahel\\Desktop\\files_0\\output.asm" s1

--handleSell func
handleSell ::[Char ] -> Int ->Double->IO ()
handleSell productName amount price= do
    let sum = fromIntegral( amount) * price
    let st="$$$ CELL "++productName++" $$$"++"\n"
    let s=show  sum

   -- writeFile "C:\\Users\\tahel\\Desktop\\files_0\\sells.asm" s
    let s1=s++"\n"
    appendFile "C:\\Users\\tahel\\Desktop\\files_0\\output.asm" st
    appendFile "C:\\Users\\tahel\\Desktop\\files_0\\output.asm" s1

somefunc :: [Char] -> IO ()
somefunc linList   = do 
    let l=words linList
    
    let productname=l!!1
    let temp1=(l!!2)
    let temp2=(l!!3)
    let amount=read temp1
    let price =read temp2
    let sumbuys=[]::[Double]
    let sumsells=[]::[Double]
    let mult=fromIntegral(amount)*price
   -- let temp= totallist!!0
    --let t=[temp+mult,0]
   -- totallist<-t
   -- Data.IORef.writeIORef total [0]
    if(l!!0=="buy")
      then
       -- func mult sumbuys
        handleBuy productname amount price 
      
        --modifyIORef sumbuys+mult
      else
        handleSell productname amount price

func num xs = xs ++ [num]
--linesinfile:: [String] -> IO 
linesinfile :: [[Char]]->[Double] -> IO [Double]
linesinfile [] totallist=
  do
     let b="TOTAL BUY: "++show(totallist!!0)++"\n"
     let c="TOTAL CELL: "++show(totallist!!1)
     appendFile "C:\\Users\\tahel\\Desktop\\files_0\\output.asm" b
     appendFile "C:\\Users\\tahel\\Desktop\\files_0\\output.asm" c
     return totallist
linesinfile (linList:xs) totallist = do
                       --somefunc x 
                        
                         let l=words linList
                         let bolr=l!!0 
                         if(bolr/="cell"&&bolr/="buy")
                           then do
                           let n=bolr++"\n"
                           appendFile "C:\\Users\\tahel\\Desktop\\files_0\\output.asm" n
                           linesinfile xs totallist
                         else 
                           do
                            let productname=l!!1
                            let temp1=(l!!2)
                            let temp2=(l!!3)
                            let amount=read temp1
                            let price =read temp2
                            let sumbuys=[]::[Double]
                            let sumsells=[]::[Double]
                            let mult=fromIntegral(amount)*price
                            let tempbuy= totallist!!0
                            let tempsell= totallist!!1
                            let t=checkfunc bolr mult totallist 
                            let totallist=[t!!0,t!!1]
                            if(l!!0=="buy")
                             then do
                               handleBuy productname amount price
                               linesinfile xs totallist  
                            else do
                               handleSell productname amount price
                               linesinfile xs totallist
                         
