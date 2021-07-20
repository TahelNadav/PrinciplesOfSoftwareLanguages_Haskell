
import System.IO ( openFile, hGetContents, IOMode(ReadMode) ) 
import Data.List ()  
import System.FilePath
    ( takeBaseName, takeExtension, takeFileName )
import Data.Maybe () 
import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )
----
import Control.Applicative ()
import System.Directory ( getDirectoryContents, removeFile )
--import Control.Monad
import System.Environment ()

readFilesDirectory :: IO ()
readFilesDirectory = do putStr "Enter directory path:"
                        directoryPath<-getLine 
                        let directoryfiles = getDirectoryContents directoryPath
                        vmFilesList<-filter(\currfile->takeExtension currfile==".vm")<$>directoryfiles
                        let asmFileName = directoryPath++"/"++takeFileName directoryPath++".asm"
                        writeFile asmFileName ""
                        writeFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\ex_1\\staticVar.txt" "0"
                        let a=length vmFilesList
                        bootstrapping asmFileName a
                        mapM (\currFile-> vmToAsm (directoryPath++"/"++currFile) asmFileName) vmFilesList
                        print vmFilesList
                                 
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

staticVar :: IO Integer
staticVar = do statVar<-openFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\ex_1\\staticVar.txt" ReadMode
               statVarContent<-hGetContents statVar
               let varLines=lines statVarContent
               let l=words (varLines!!0)
               let t=read (l!!0)::Integer 
               removeFile  "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\ex_1\\staticVar.txt"
               writeFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\ex_1\\staticVar.txt" (show (t+1))
             
               return t
               


bootstrapping asmPath a= do
                        if(a>1)
                            then do
                                 appendFile asmPath "// Initialize the SP to 256\n@256\nD=A\n@SP\nM=D\n"
                                 call "Sys.init" "0" "Sys.init" asmPath
                        else
                          appendFile asmPath ""

                            


 

vmToAsm vmPath asmPath= do
                           
                           vmFile <- openFile vmPath ReadMode
                           vmContenta<-hGetContents vmFile
                           let vmLines=lines vmContenta
                           let a=removeirrelevant vmLines
                           let b=removeItem "" a
                           linesinfile(b) (takeBaseName vmPath) asmPath
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
                          if(word/="push"&& word/="pop"&&word/="add"&&word/="sub"&&word/="label"&&word/="goto"&&word/="if-goto"&&word/="function"&&word/="call")
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
    let a="//push "++segment++" "++"\n"
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
    else if(segment=="LCL") then do
        
                let temp="@LCL"++"\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
                appendFile asmPath temp
    else if(segment=="ARG") then do
        
                let temp="@ARG"++"\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
                appendFile asmPath temp
    else if(segment=="THIS") then do
        
                let temp="@THIS"++"\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
                appendFile asmPath temp
    else if(segment=="THAT") then do
        
                let temp="@THAT"++"\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
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
    let notcomment="\n//not\n@SP\nA=M-1\nD=M\nM=!D\n"
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


functionhelp 0 =""
functionhelp x ="@0\n"++"D=A"++"\n"++"@SP\n"++"A=M\nM=D\n@SP\nM=M+1\n"++functionhelp (x-1)
    



label  vmFileName namelable asmPath=do
    let lablecomment="//label\n"++"("++vmFileName++"."++namelable++")\n"
    appendFile asmPath lablecomment

goto vmFileName namelable asmPath=do
    let gotocomment="//goto\n"++"@"++vmFileName++"."++namelable++"\n0;JMP\n"
    appendFile asmPath gotocomment

ifgoto vmFileName namelable asmPath=do
    let ifgotocomment="//if-goto\n"++"@SP\nM=M-1\nA=M\nD=M\n@"++vmFileName++"."++ namelable++"\nD;JNE\n"
    appendFile asmPath ifgotocomment

functiona :: (Eq t, Num t) => [Char] -> t -> FilePath -> IO ()
functiona funcname k asmPath  =do
    let kpush=functionhelp k 
    --let a=[push "constant" 0 "" ""| m <- [1..k]]
    let functioncomment="//function\n"++"("++funcname++")\n"++kpush
    appendFile asmPath functioncomment

call funcname arguments currentfunc asmPath=do
    c<-staticVar
    let callcomment="//call\n@"++currentfunc++".ReturnAddress"++show (c)++"\n"++"D=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
    appendFile asmPath callcomment
    push "LCL" 0 "" asmPath
    push "ARG" 0 "" asmPath
    push "THIS" 0 "" asmPath
    push "THAT" 0 "" asmPath
    let repositionarg="//reposition ARG\n@"++arguments++"\nD=A\n@5\nD=D+A\n@SP\nD=M-D\n@ARG\nM=D\n"
    let repositionlcl="//reposition LCL\n@SP\nD=M\n@LCL\nM=D\n"
    let transfer="//Transfer control\n@"++funcname++"\n0;JMP\n("++currentfunc++".ReturnAddress"++show (c)++")"
    appendFile asmPath (repositionarg++repositionlcl++transfer)


restore who="@LCL\nM=M-1\nA=M\nD=M\n@"++who++"\nM=D\n"

returnc  asmPath=do
    appendFile asmPath ("//return\n//save the return address\n@LCL\nD=M\n@5\nA=D-A\nD=M\n@13\nM=D\n"++"//save return value in top\n@SP\nM=M-1\nA=M\nD=M\n@ARG\nA=M\nM=D\n//update sp\n@ARG\nD=M+1\n@SP\nM=D\n")
    let restore_comments=(restore "THAT"++restore "THIS"++restore "ARG"++restore "LCL")
    appendFile asmPath (restore_comments++"//jump to return address\n@13\nA=M\n0;JMP")

linesinfile :: [[Char]]->String->String->IO [Double]
linesinfile [] x asmPath= return []
    
linesinfile (linList:xs) vmFileName asmPath = do
                        --appendFile asmPath ("\n\n\n\n\n"++vmFileName++"\n\n\n\n\n")
                        
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
                        else if(command=="label")
                            then do
                                label  vmFileName (l!!1) asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="goto")
                            then do
                                goto vmFileName (l!!1) asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="if-goto")
                            then do
                                ifgoto vmFileName (l!!1) asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="function")
                            then do
                                writeFile "C:\\Users\\tahel\\Desktop\\func.txt" (l!!1)
                                let h=read(l!!2)
                                functiona (l!!1) h asmPath  
                                linesinfile xs vmFileName asmPath
                        else if(command=="call")
                            then do
                                statVar<-openFile "C:\\Users\\tahel\\Desktop\\func.txt" ReadMode
                                statVarContent<-hGetContents statVar
                                let varLines=lines statVarContent
                                let hh=words (varLines!!0)
                                let currentfunc=hh!!0 
                                let funcname=(l!!1)
                                let argnum=l!!2
                                call funcname argnum currentfunc asmPath
                                linesinfile xs vmFileName asmPath
                        else if(command=="return")
                            then do
                                
                                returnc asmPath
                                linesinfile xs vmFileName asmPath
                        else 
                           
                           linesinfile xs vmFileName asmPath
----