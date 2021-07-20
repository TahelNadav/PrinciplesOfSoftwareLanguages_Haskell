import System.IO 
import Data.List  
import System.FilePath
    ( takeBaseName, takeExtension, takeFileName )
import Data.Maybe () 
import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )
----
import Control.Applicative ()
import System.Directory ( getDirectoryContents, removeFile )
import Control.Monad
import System.Environment ()

readFilesDirectory = do putStr "Enter directory path:"
                        directoryPath<-getLine 
                        let directoryfiles = getDirectoryContents directoryPath
                        vmFilesList<-filter(\currfile->takeExtension currfile==".vm")<$>directoryfiles
                        let asmFileName = directoryPath++"/"++takeFileName directoryPath++".asm"
                        writeFile asmFileName ""
                        writeFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\ex_1\\staticVar.txt" "0"
                        mapM (\currFile-> vmToAsm (directoryPath++"/"++currFile) asmFileName) vmFilesList

                        
                        print vmFilesList
                        

staticVar :: IO Integer
staticVar = do statVar<-openFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\ex_1\\staticVar.txt" ReadMode
               statVarContent<-hGetContents statVar
               let varLines=lines statVarContent
               let l=words (varLines!!0)
               let t=read (l!!0)::Integer 
               removeFile  "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\ex_1\\staticVar.txt"
               writeFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\ex_1\\staticVar.txt" (show (t+1))
             
               return t
               

    

vmToAsm vmPath asmPath= do vmFile <- openFile vmPath ReadMode
                           vmContenta<-hGetContents vmFile
                           let vmLines=lines vmContenta
                           linesinfile(removeirrelevant vmLines) (takeBaseName vmPath) asmPath
                           print "vmToAsm"
                           



data Counter = Counter { x :: IORef Int }
makeCounter :: Int -> IO Counter        
makeCounter i = do iref <- newIORef i   
                   return (Counter iref)   
incCounter :: Int -> Counter -> IO ()            
incCounter i (Counter c) = do modifyIORef c (+ i)
showCounter :: Counter -> IO ()               
showCounter (Counter c) = do c' <- readIORef c
                             print(c')   
c =makeCounter 1     
{-mainFunc= do
    
    
     ia <- openFile "C:\\Users\\tahel\\Downloads\\Exercises\\Targil1\\project 07\\MemoryAccess\\PointerTest\\PointerTest.vm" ReadMode
     writeFile asmPath ""
     contenta<-hGetContents ia
     let allinesa=lines contenta
     let vmFileName=split "C:\\Users\\tahel\\Downloads\\Exercises\\Targil1\\project 07\\MemoryAccess\\StaticTest\\StaticTest"
     --linesinfile allinesa
     --let t=getDirectoryContents "C:\\Users\\tahel\\Desktop\\files_1"
    -- let asmCode = map (\thisLine -> convertFunc (words thisLine) filename (fromJust (elemIndex thisLine listVm))) listVm
     --let a=elemIndex  allinesa
     let h=vmFileName!!((length vmFileName)-1)
     --linesinfile (removeirrelevant allinesa) h 
     print h-}
    
     --print c
removeirrelevant :: [String] -> [[Char]]
--removeirrelevant ((xs):arr)=["push"++xs]++arr
removeirrelevant ((linList):arr)=do
                         let l=words linList
                         if(length l>0) then do
                          let word=l!!0
                          if(word/="push"&& word/="pop"&&word/="add"&&word/="sub")
                              then do
                                 removeirrelevant arr
                          else
                              return linList++arr
                          else
                              removeirrelevant arr
                   


split :: String -> [String]
split [] = [""]
split (c:cs) | c == '\\' = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs


push :: (Eq a, Show a, Num a) => [Char] -> a -> String ->String-> IO ()
push segment index namevm asmPath=do
    let a="//push "++segment++" "++show(index)++"\n"
    appendFile asmPath a
    if(segment=="constant") then do
         let temp="@"++show(index)++"\n"++"D=A"++"\n"++"@SP\n"++"A=M\nM=D\n@SP\nM=M+1\n"
         appendFile asmPath temp
    else if(segment=="local") then do
          let temp="@"++show(index)++"\n"++"D=A"++"\n"++"@LCL\n"++"A=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
          appendFile asmPath temp
    else if(segment=="argument") then do
                let temp="@"++show(index)++"\n"++"D=A"++"\n"++"@ARG\n"++"A=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
                appendFile asmPath temp
    else if(segment=="this") then do
                let temp="@"++show(index)++"\n"++"D=A"++"\n"++"@THIS\n"++"A=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
                appendFile asmPath temp
    else if(segment=="that") then do
                let temp="@"++show(index)++"\n"++"D=A"++"\n"++"@THAT\n"++"A=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
                appendFile asmPath temp
    else if(segment=="temp") then do
                let temp="@"++show(index+5)++"\n"++"D=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
                appendFile asmPath temp
    else if(segment=="static") then do
        
                let temp="@"++namevm++"."++show(index)++"\n"++"D=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
                appendFile asmPath temp
    else if(segment=="pointer") then do
                if(index==0) then do
                 let temp="@"++"3"++"\n"++"D=M"++"\n"++"@SP\n"++"A=M\nM=D\n@SP\nM=M+1\n"
                 appendFile asmPath temp
                else do
                 let temp="@"++"4"++"\n"++"D=M"++"\n"++"@SP\n"++"A=M\nM=D\n@SP\nM=M+1\n"
                 appendFile asmPath temp

    else
        appendFile asmPath ""
        
 
pop segment index namevm asmPath=do
    let a="//pop "++segment++" "++show(index)++"\n"
    appendFile asmPath a

    if(segment=="local") then do
          let temp="@SP"++"\nA=M-1"++"\n"++"D=M"++"\n"++"@LCL"++"\n"
          let add=addingFunc index
          appendFile asmPath (temp++"A=M\n"++add++"M=D\n@SP\nM=M-1\n")
    else if(segment=="argument") then do
          let temp="@SP"++"\nA=M-1"++"\n"++"D=M"++"\n"++"@ARG"++"\n"
          let add=addingFunc index
          appendFile asmPath (temp++"A=M\n"++add++"M=D\n@SP\nM=M-1\n")
    else if(segment=="this") then do
          let temp="@SP"++"\nA=M-1"++"\n"++"D=M"++"\n"++"@THIS"++"\n"
          let add=addingFunc index
          appendFile asmPath (temp++"A=M\n"++add++"M=D\n@SP\nM=M-1\n")
    else if(segment=="that") then do
          let temp="@SP"++"\nA=M-1"++"\n"++"D=M"++"\n"++"@THAT"++"\n"
          let add=addingFunc index
          appendFile asmPath (temp++"A=M\n"++add++"M=D\n@SP\nM=M-1\n")
    else if(segment=="temp") then do
                let temp="@SP"++"\nA=M-1"++"\n"++"D=M"++"\n"++"@"++show(5+index)++"\nM=D\n@SP\nM=M-1\n"
                appendFile asmPath temp
    else if(segment=="static") then do
                let temp="@SP\n"++"M=M-1"++"\n"++"A=M"++"\n"++"D=M"++"\n@"++namevm++"."++show(index)++"\nM=D\n"
                appendFile asmPath temp
    else if(segment=="pointer") then do
                if(index==0) then do
                 let temp="@SP"++"\nA=M-1"++"\n"++"D=M"++"\n"++"@3"++"\nM=D\n@SP\nM=M-1\n"
                 appendFile asmPath temp
                else do
                 let temp="@SP"++"\nA=M-1"++"\n"++"D=M"++"\n"++"@4"++"\nM=D\n@SP\nM=M-1\n"
                 appendFile asmPath temp

    else
        appendFile asmPath ""
        


add1 asmPath =do
    let addcomment="\n//add\n@SP\nA=M-1\nD=M\nA=A-1\nM=M+D\n@SP\nM=M-1\n"
    appendFile asmPath addcomment

sub asmPath=do
    let subcomment="\n//sub\n@SP\nA=M-1\nD=M\nA=A-1\nM=M-D\n@SP\nM=M-1\n"
    appendFile asmPath subcomment

neg :: FilePath -> IO ()
neg asmPath=do
    let negcomment="\n//neg\n@SP\nA=M-1\nM=-M\n"
    appendFile asmPath negcomment

notc asmPath=do
    -----------------------------------------------------------------------
    let notcomment="\n//not\n@SP\nA=M-1\nD=M\nM=Not D\n"
    appendFile asmPath notcomment

eq asmPath=do
      
    c<-staticVar
    --let v=(show c)
    let eqcomment="\n//eq\n@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\n@IF_TRUE"++show c++"\nD;JEQ\nD=0\n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE"++show c++"\n0;JMP\n(IF_TRUE"++show c++")\nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE"++show c++")\n@SP\nM=M-1"
    appendFile asmPath eqcomment


gt asmPath=do
    c<-staticVar
    let gtcomment="//gt\n@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\n@IF_TRUE"++show c++"\nD;JGT\nD=0 \n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE"++show c++"\n0;JMP\n(IF_TRUE"++show c++") \nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE"++show c++")\n@SP\nM=M-1\n"
    appendFile asmPath gtcomment
    


andc asmPath=
    do 
    let andcomment="//and\n@SP\nA=M-1\nD=M\nA=A-1\nM=D&M\n@SP\nM=M-1\n"
    appendFile asmPath andcomment


orc :: FilePath -> IO ()
orc asmPath=
     do 

    let orcomment="//or\n@SP\nA=M-1\nD=M\nA=A-1\nM=D|M\n@SP\nM=M-1\n"
    appendFile asmPath orcomment

lt asmPath= do 
    c<-staticVar
    let ltcomment="\n//lt\n@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\n@IF_TRUE"++show c++"\nD;JLT\nD=0\n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE"++show c++"\n0;JMP\n(IF_TRUE"++show c++")\nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE"++show c++")\n@SP\nM=M-1"
    appendFile asmPath ltcomment
    

  

addingFunc :: (Eq t, Num t) => t -> [Char]
addingFunc 0=""
addingFunc x="A=A+1\n"++addingFunc (x-1)

 
    













linesinfile :: [[Char]]->String->String->IO [Double]
linesinfile [] x asmPath= return []
    
linesinfile (linList:xs) vmFileName asmPath = do
                        c <- makeCounter 0
                        let l=words linList
                        let command=l!!0
                        if(command=="push")
                            then do
                                let segment=l!!1
                                let index=read(l!!2)
                                push segment index vmFileName asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="pop")
                            then do
                                let segment=l!!1
                                let index=read(l!!2)
                                pop segment index vmFileName asmPath
                                linesinfile xs vmFileName asmPath
                               
                        else if(command=="add")
                            then do
                                add1 asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="sub")
                            then do
                                sub asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="neg")
                            then do
                                neg asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="eq")
                            then do
                                eq asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="gt")
                            then do
                                gt asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="lt")
                            then do
                                lt asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="and")
                            then do
                                andc asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="or")
                            then do
                                orc asmPath
                                linesinfile xs vmFileName asmPath
                         else if(command=="not")
                            then do
                                notc asmPath
                                linesinfile xs vmFileName asmPath
                        else 
                           
                           linesinfile xs vmFileName asmPath
----