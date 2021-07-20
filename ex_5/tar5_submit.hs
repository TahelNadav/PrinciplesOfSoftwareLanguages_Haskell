--Tahel Nadav 207816612
--Tzviya Laniado 315121798


import Control.Applicative ()
import System.Directory ( getDirectoryContents, removeFile )
import System.IO
import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.FilePath
import Data.Maybe




readFilesDirectory = do putStr "Enter directory path:"
                        directoryPath<-getLine
                        let directoryfiles = getDirectoryContents directoryPath
                        jackFilesList<-filter(\currfile->takeExtension currfile==".jack")<$>directoryfiles
                        mapM_ (\currFile-> jackToxmlTokenizing (directoryPath++"/"++currFile) (directoryPath++"/"++takeBaseName currFile++"T.xml")) jackFilesList
                        sendFilesToParser directoryPath
                        print jackFilesList


--part 1

--pass jack file to xml
jackToxmlTokenizing :: FilePath -> FilePath -> IO ()
jackToxmlTokenizing jackPath xmlPath= do
                            let keyword=["class","constructor","function","method","field","static","var","int","char","boolean","void","true","false","null","this","let","do","if","else","while","return"]
                            let symbolList=["{","}","(",")","[","]",".",",",";","+","-","*","/","&","|","<",">","=","~"]
                            jackFile <- openFile jackPath ReadMode
                            jackContenta<-hGetContents jackFile
                            let xml=makeTokens jackContenta keyword symbolList--xml code of the jack file

                            writeFile xmlPath  (unlines (["<token>"]++xml++["</token>"]))




makeTokens :: String ->[String]->[String]->[String]
makeTokens jCode keywords symbols
  | null jCode = []
  | (jCode!!0 == '/' ) && (jCode!!1 == '/' ) = makeTokens (remove1LineComment jCode) keywords symbols--remove 1 line
  | (jCode!!0 == '/' ) && (jCode!!1 == '*' ) = makeTokens (removeLinesComment jCode) keywords symbols--remove /**/
  | length jCode>0&& isDigit(jCode!!0) = ["<integerConstant> " ++ ((mintegerConstant jCode)!!0)] ++ makeTokens ((mintegerConstant jCode)!!1) keywords symbols
  | length jCode>0&&jCode!!0 == '"' = ["<stringConstant> " ++ ( (mstringConstant (removeFirst jCode))!!0)] ++ makeTokens ((mstringConstant (removeFirst jCode))!!1) keywords symbols
  | length jCode>0&&  [jCode!!0] `elem` symbols = ("<symbol> " ++ ( (whichSymbol jCode)!!0)) : (makeTokens ((whichSymbol jCode)!!1) keywords symbols )
  | (jCode!!0 == ' ' ) || (jCode!!0 == '\n' ) || (jCode!!0 == '\t' ) = makeTokens (removeFirst jCode) keywords symbols
  | otherwise = [(keywordOrIdentifier jCode keywords)!!0] ++ makeTokens ((keywordOrIdentifier jCode keywords)!!1) keywords symbols--or keyword or id



--send xml files to parser
sendFilesToParser::String->IO()
sendFilesToParser directoryPath=
        do
                let directoryfiles = getDirectoryContents directoryPath
                xmlFilesList<-filter(\currfile->takeExtension currfile==".xml")<$>directoryfiles
                mapM_(\currFile-> mparser (directoryPath++"/"++currFile) (directoryPath++"/"++removeLastChar (takeBaseName  currFile)++".vm")) xmlFilesList


--add argument or local vars to the table

getCurTableRoutine :: String -> String -> String -> String -> String
getCurTableRoutine varsRoutineTable kind varType name = if kind == "local"
                                                    then
                                                            do
                                                                    let newVar=[name ++ " " ++ varType ++ " " ++ kind ++" "++  show (countLocal (lines varsRoutineTable)) ]
                                                                    unlines ((lines varsRoutineTable) ++ newVar)
                                                         else
                                                                 do
                                                                       let newVar=[name ++ " " ++ varType ++ " " ++ kind ++" "++ show (countArg (lines varsRoutineTable))  ]
                                                                       unlines ((lines varsRoutineTable) ++ newVar)



 --varsClassTable


checktatements :: [String]->String->[Char]->String->Int->[String]
checktatements xmlTokLinesCode varsRoutineTable varsClassTable className n
  | (words (xmlTokLinesCode!!0) !!1) == "}" = 
          [show n]
  | (words (xmlTokLinesCode!!0 )!!1) == "let"= do
                   let l = letStatement xmlTokLinesCode varsRoutineTable varsClassTable className
                   let curChar=read ((getLast l)!!0)
                   removeLastElem l++ checktatements (removeNFirstLines xmlTokLinesCode curChar ) varsRoutineTable varsClassTable className (n +curChar)
  | (words(xmlTokLinesCode!!0)!!1) == "if" = do
                          let l = ifStatement xmlTokLinesCode varsRoutineTable varsClassTable className
                          removeLastElem l++ checktatements (removeNFirstLines xmlTokLinesCode((read ((getLast l)!!0)) )) varsRoutineTable varsClassTable className (n + (read ((getLast l)!!0)))
  | (words (xmlTokLinesCode!!0)!!1) == "while" = do
                           let l = whileStatement xmlTokLinesCode varsRoutineTable varsClassTable className
                           (removeLastElem l) ++ (checktatements (removeNFirstLines xmlTokLinesCode((read ((getLast l)!!0)) )) varsRoutineTable varsClassTable className (n + (read ((getLast l)!!0))))
  | (((words (xmlTokLinesCode!!0) )!!1) == "do") = do
                   let l = (doStatement xmlTokLinesCode varsRoutineTable varsClassTable className)
                   (removeLastElem l) ++ (checktatements (removeNFirstLines xmlTokLinesCode((read ((getLast l)!!0)) )) varsRoutineTable varsClassTable className (n +(read ((getLast l)!!0))))
  | (((words (xmlTokLinesCode!!0) )!!1) == "return") = do
           let l = (returnStatement xmlTokLinesCode varsRoutineTable varsClassTable className)
           (removeLastElem l) ++ (checktatements (removeNFirstLines xmlTokLinesCode((read (getLast l!!0)) )) varsRoutineTable varsClassTable className (n + (read ((getLast l)!!0))))
  | otherwise = []

letStatement::[String]->String->String->String->[String]
letStatement xmlTokLinesCode varsRoutineTable varsClassTable className =
                                if ((words (xmlTokLinesCode!!2) )!!1) == "["--assighn to arr
                                        then do
                                                        
                                                        let leftVarIdx = expression (removeNFirstLines xmlTokLinesCode 3) varsRoutineTable varsClassTable className--push left var index
                                                        let endExp = 3 + ((read ((getLast leftVarIdx)!!0)) )
                                                        let rightVar = expression (removeNFirstLines xmlTokLinesCode (endExp + 2)) varsRoutineTable varsClassTable className
                                                        let name= ((words(xmlTokLinesCode!!1))!!1)
                                                        let varKind=kindOf (lines varsClassTable) name ++ kindOf (lines varsRoutineTable) name
                                                        let indexVar=getVarIdx (lines varsClassTable) name ++ getVarIdx (lines varsRoutineTable) name
                                                        removeLastElem leftVarIdx ++ --push idx OF LEFT VAR
                                                         ["push "++ varKind ++" "++ indexVar] ++ --push left var
                                                          ["add"] ++ --add var and index
                                                          removeLastElem rightVar ++ --push right var
                                                           ["pop temp 0"] ++ --pop the right to temp 
                                                           ["pop pointer 1" ] ++ --pop arr+idc to pointer 1/that
                                                            ["push temp 0"] ++ --push right var that saved in temp
                                                             ["pop that 0"] ++  --enter right to left
                                                             [show (endExp + 3 + ((read ((getLast rightVar)!!0)) ))]
                                else do
                                                
                                                let rightVar = expression (removeNFirstLines xmlTokLinesCode 3) varsRoutineTable varsClassTable className --after =
                                                let endrightVar = 3 + read ((getLast rightVar)!!0)
                                                let name= words(xmlTokLinesCode!!1)!!1
                                                let leftVar=kindOf (lines varsClassTable) name ++ kindOf (lines varsRoutineTable) name
                                                let varIdx=getVarIdx (lines varsClassTable) name ++ getVarIdx (lines varsRoutineTable) name
                                                removeLastElem rightVar ++ --push right var
                                                 ["pop " ++ leftVar ++ " " ++varIdx] ++ --enter right to left
                                                 [show (endrightVar + 1)]
-- 

ifStatement:: [String]->String->String->String->[String]
ifStatement xmlTokLinesCode varsRoutineTable varsClassTable className = do --if(expr){statement} (else{statement})?
                                                let conditionExpr = expression (removeNFirstLines xmlTokLinesCode 2) varsRoutineTable varsClassTable className
                                                let endCondIdx = read ((getLast conditionExpr)!!0) + 2
                                                let trueStatement = mstatements (removeNFirstLines xmlTokLinesCode (endCondIdx + 2)) varsRoutineTable varsClassTable className
                                                let endTrueStIdx = endCondIdx + 2 + read ((getLast trueStatement)!!0)
                                                if ((words (xmlTokLinesCode!!(endTrueStIdx+1)))!!1) == "else"-- if else exist  
                                                        then do
                                                                        let elseStatement =  mstatements (removeNFirstLines xmlTokLinesCode (endTrueStIdx + 3))varsRoutineTable varsClassTable className
                                                                        let endElseIdx = endTrueStIdx + 3 + read ((getLast elseStatement)!!0)
                                                                        let def= ["if-goto IF_TRUE" ++ className++ show (length xmlTokLinesCode) ] ++ ["goto IF_FALSE" ++ className++ show (length xmlTokLinesCode) ] 
                                                                        let trueLableSt= ["label IF_TRUE" ++ className++ show (length xmlTokLinesCode) ] ++ removeLastElem trueStatement++ ["goto IF_END" ++ className++ show (length xmlTokLinesCode) ]
                                                                        let elseLableSt= ["label IF_FALSE" ++ className++ show (length xmlTokLinesCode)]++removeLastElem elseStatement
                                                                        removeLastElem conditionExpr++def++ trueLableSt ++elseLableSt++ ["label IF_END" ++ className++ show (length xmlTokLinesCode)  ]++ [show(endElseIdx + 1)]
                                                else  
                                                        do
                                                                let trueLableSt= ["label IF_TRUE" ++className++ show (length xmlTokLinesCode)] ++ removeLastElem trueStatement
                                                                let def=["if-goto IF_TRUE" ++ className++ show (length xmlTokLinesCode)] ++ ["goto IF_FALSE" ++ className++ show (length xmlTokLinesCode)] 
                                                                removeLastElem conditionExpr++ def++ trueLableSt ++ ["label IF_FALSE" ++ className++ show (length xmlTokLinesCode)] ++ [show(endTrueStIdx + 1)]


whileStatement :: [String]->[Char]->String->String->[String]
whileStatement xmlTokLinesCode varsRoutineTable varsClassTable className = do
                                                let conditionExpr = expression (removeNFirstLines xmlTokLinesCode 2) varsRoutineTable varsClassTable className
                                                let endCondIdx = read ((getLast conditionExpr)!!0) + 2
                                                let currentStatement = mstatements (removeNFirstLines xmlTokLinesCode (endCondIdx + 2)) varsRoutineTable varsClassTable className
                                                let endSt =  endCondIdx     + 2 +read ((getLast currentStatement)!!0)
                                                let def=["label WHILE_EXP"++ className ++ show (length xmlTokLinesCode) ]
                                                let trueCond=["if-goto WHILE_END"++ className ++ show (length xmlTokLinesCode)] ++removeLastElem currentStatement ++ ["goto WHILE_EXP" ++ className ++ show (length xmlTokLinesCode)]
                                                let falseCond= ["label WHILE_END" ++ className ++ show (length xmlTokLinesCode)] 
                                                def ++ removeLastElem conditionExpr ++ ["not"] ++trueCond++falseCond++ [show(endSt+1)]

doStatement :: [String]->String->String->String->[String]
doStatement xmlTokLinesCode varsRoutineTable varsClassTable className = do
                                                        let src = subRoutineCall (removeNFirstLines xmlTokLinesCode 1) varsRoutineTable varsClassTable className
                                                        let endSrc = ((read ((getLast src)!!0)) ) + 1
                                                        removeLastElem src ++
                                                         ["pop temp 0"] ++ --enter the result to temp
                                                         [show (endSrc + 1)] 

returnStatement :: [String]->String->String->String->[String]
returnStatement xmlTokLinesCode varsRoutineTable varsClassTable className =
                                        do
                                                let afterReturn=(words (xmlTokLinesCode!!1))!!1
                                                if afterReturn == ";" -- nothing returned
                                                        then["push constant 0"] ++ 
                                                        ["return"] ++
                                                         ["2"]
                                                else do
                                                        let curCode=removeNFirstLines xmlTokLinesCode 1
                                                        let funcResult = expression curCode varsRoutineTable varsClassTable className
                                                        let end = ((read ((getLast funcResult)!!0)) ) + 1
                                                        (removeLastElem funcResult) ++
                                                         ["return"] ++[(show (end + 1))]



translateConstant::String->String->String-> [String]
translateConstant varsRoutineTable varsClassTable term
  | ((words term)!!0) == "<integerConstant>" = ["push constant " ++ ((words term)!!1)]
  | ((words term)!!0) == "<stringConstant>" = ["push constant " ++ (show (length (removeNLastLines (removeNFirstLines term 17) 18 )))] ++ ["call String.new 1"] ++ (pushString (removeNLastLines (removeNFirstLines term 17) 18 ))
  | (words term)!!1=="true"||(words term)!!1=="false"||(words term)!!1=="null"||(words term)!!1=="this"=
          do
                  let word=((words term)!!1)
                  if(word == "true") then  ["push constant 0"] ++ ["not"]
                    else if(word == "false"||word == "null") then  ["push constant 0"] --null/false
                      else if(word == "this") then ["push pointer 0"]--pointer to class
                        else []
  | otherwise = 
  do
          ---------------------------------------what if there istwo same name in both table
        let name=((words term)!!1)
        let varKind=kindOf (lines varsClassTable) name ++ kindOf (lines varsRoutineTable) name--var from one of the table
        let inndexVar=getVarIdx (lines varsClassTable) name ++ getVarIdx (lines varsRoutineTable) name
        ["push " ++ varKind ++ " " ++ inndexVar ]

expression::[String]->String->String->String->[String]
expression xmlTokLinesCode varsRoutineTable varsClassTable className = do
                                        let term1 = term xmlTokLinesCode varsRoutineTable varsClassTable className--first term
                                        let endTerm = read (head(getLast term1)) 
                                        let a=moreTerms (removeNFirstLines xmlTokLinesCode endTerm) varsRoutineTable varsClassTable className 0
                                        let endGetTerm = read (head (getLast a)) 
                                        removeLastElem term1 ++ removeLastElem(a)++[show (endTerm + endGetTerm)]



moreTerms :: [String]->String->String->String-> Int ->[String]
moreTerms code varsRoutineTable varsClassTable className  n =
                        if ((words (code!!0))!!1) == "]" || ((words (code!!0))!!1) == ";"|| ((words (code!!0))!!1) == ")" ||((        words (code!!0))!!1) == ","
                                then [show n]
                                else do
                                        let term1 = term (removeNFirstLines code 1) varsRoutineTable varsClassTable className--remove op
                                        let endTerm = 1 + read (head (getLast term1))
                                        removeLastElem term1 ++
                                         [translateOp ((words (code!!0))!!1)] ++
                                          moreTerms (removeNFirstLines code endTerm) varsRoutineTable varsClassTable className (n + endTerm)

translateOp x=if (x=="*") then "call Math.multiply 2"
      else if(x=="+") then "add"
      else if(x=="&amp;") then "and"
      else if(x=="|") then "or"
      else if(x=="-") then "sub"
      else if(x=="~") then "not"
      else if (x=="!") then "neg"
      else if(x=="&lt;") then "lt"
      else if(x=="&gt;") then "gt"
      else if(x=="=") then "eq"
      else if(x=="/") then "call Math.divide 2"
      else ""


--varsRoutineTable varsClassTable

term :: [String]->String->String->String->[String]
term xmlTokLinesCode varsRoutineTable varsClassTable className
  | (words (xmlTokLinesCode !! 0)) !! 0== "<identifier>"
    = if (length xmlTokLinesCode > 1) && ((words (xmlTokLinesCode !! 1) !! 1) == "[") then  --if it like a[...] 
        do 
                let varIdx= expression(removeNFirstLines xmlTokLinesCode 2) varsRoutineTable varsClassTable className
                let endExp = read (getLast varIdx !! 0 )+ 2
                let name = words (xmlTokLinesCode !! 0) !! 1
                let varKind= kindOf (lines varsClassTable) name++ kindOf (lines varsRoutineTable) name --get name from the table
                let indexVar= getVarIdx (lines varsClassTable) name ++ getVarIdx (lines varsRoutineTable) name

                (removeLastElem varIdx)--push idx
                  ++
                  ["push "++ varKind++" "++ indexVar]--push var
                  ++ ["add"]--add idx to var
                     ++ ["pop pointer 1"] 
                      ++["push that 0"] --push the value
                      ++ [(show (endExp + 1))]
    else
        if (((length xmlTokLinesCode) > 1) &&((((words (xmlTokLinesCode !! 1)) !! 1) == "(")|| (((words (xmlTokLinesCode !! 1)) !! 1) == ".")))----subroutinecall
                 then
                         do
                             let sub = (subRoutineCall xmlTokLinesCode varsRoutineTable varsClassTable className)
                             let aaa = (getLast sub) !! 0
                             if (((words (xmlTokLinesCode !! 0)) !! 1) == "(") then
                              ["push pointer 0"] ++ (removeLastElem sub) ++ [aaa]
                             else
                                 (removeLastElem sub) ++ [aaa]
	       else
                   (translateConstant varsRoutineTable varsClassTable (xmlTokLinesCode !! 0)) ++ ["1"]
  | (words (xmlTokLinesCode !! 0) !! 1) == "("
  = do let exp2 = expression (removeNFirstLines xmlTokLinesCode 1) varsRoutineTable varsClassTable className
       let endExp2 = read ((getLast exp2) !! 0) + 1
       (removeLastElem exp2) ++ [(show (endExp2 + 1))]

  | (words (xmlTokLinesCode !! 0) !! 1 == "-")|| ((words (xmlTokLinesCode !! 0) !! 1) == "~") --unary op
  = do 
       let term1 = term (removeNFirstLines xmlTokLinesCode 1) varsRoutineTable varsClassTable className --term after unaryop
       let endT = read ((getLast term1) !! 0) + 1
       if (words (head xmlTokLinesCode ) !! 1) == "~"
               then
                  (removeLastElem term1) ++ ["not"] ++ [(show (endT))]
       else
                  (removeLastElem term1) ++ ["neg"] ++ [(show (endT))]
  | otherwise
  = (translateConstant varsRoutineTable varsClassTable (xmlTokLinesCode !! 0)) ++ ["1"]

getTypeVar:: [String]->String -> String
getTypeVar [] name = ""
getTypeVar (var1:table) name = if ((words var1)!!0) == name then (words var1)!!1  else getTypeVar table name


checkIfKnown:: [String]->[Char] -> Bool
checkIfKnown [] name = False
checkIfKnown (var1:table) name = (((words var1)!!0) == name) || (checkIfKnown table name)

callIsMember :: [String]-> String-> String-> [String]
callIsMember code varsRoutineTable varsClassTable 
  |  ((words(code!!1)!!1) == ".") && (checkIfKnown (lines varsClassTable) ((words(head code))!!1) || (checkIfKnown (lines varsRoutineTable) ((words(head code))!!1))) = 
          do
                  let name= ((words(code!!0))!!1)
                  let varKind=kindOf (lines varsClassTable) name ++ kindOf (lines varsRoutineTable) name
                  let indexVar=getVarIdx (lines varsClassTable) name ++ getVarIdx (lines varsRoutineTable) name
                  ["push " ++ varKind++ " " ++ indexVar]
  | ((words(code!!1))!!1) /= "." = ["push pointer 0"] --if its diff from className.func
  | otherwise = []


subRoutineCall :: [String]->String->String->String->[String]
subRoutineCall xmlTokLinesCode varsRoutineTable varsClassTable className =
                                                if((words (xmlTokLinesCode!!1))!!1) == "(" --funcname()
                                                        then do
                                                                let paramToSendList = expressionList (removeNFirstLines xmlTokLinesCode 2) varsRoutineTable varsClassTable className--push args
                                                                let endExpL = 2 + read (head (getLast paramToSendList)) 
                                                                callIsMember xmlTokLinesCode varsRoutineTable varsClassTable ++ removeLastElem (removeLastElem paramToSendList) ++
                                                                 ["call " ++ className ++ "." ++ (words (xmlTokLinesCode!!0))!!1 ++ " "++ 
                                                                 show ((read(getElemBeforeLast paramToSendList))+1)] ++ 
                                                                 [show (endExpL+1)]
                                                else do 
                                                                let paramToSendList = expressionList (removeNFirstLines xmlTokLinesCode 4) varsRoutineTable varsClassTable className--push args
                                                                let endExpL2 = 4 + read (head (getLast paramToSendList))  
                                                                if checkIfKnown (lines varsClassTable) ((words(head xmlTokLinesCode))!!1) || (checkIfKnown (lines varsRoutineTable) ((words(head xmlTokLinesCode))!!1))--classobject.func()
                                                                     then 
                                                                             do
                                                                                     let name=((words (xmlTokLinesCode!!0))!!1)
                                                                                     let typeV=(getTypeVar (lines varsClassTable) name) ++ (getTypeVar (lines varsRoutineTable) name)
                                                                                     let args=removeLastElem (removeLastElem paramToSendList)
                                                                                     let numArgs=show(1+(read(getElemBeforeLast paramToSendList)))
                                                                                     callIsMember xmlTokLinesCode varsRoutineTable varsClassTable ++ args++["call " ++ typeV ++(words (xmlTokLinesCode!!1))!!1++(words (xmlTokLinesCode!!2))!!1 ++
                                                                                      " " ++numArgs]++[show (endExpL2 + 1)]
                                                                else removeLastElem (removeLastElem paramToSendList) ++  --it is static -function----classname.func()
                                                                                ["call " ++ words (head xmlTokLinesCode)!!1 ++words (xmlTokLinesCode!!1)!!1++words (xmlTokLinesCode!!2)!!1 ++ " " ++
                                                                                getElemBeforeLast paramToSendList]
                                                                                ++[show (endExpL2 + 1)]


parseClass xmlTokLinesCode=
        do
            --build the class table
            let varsClassTable = parseClassVarDec (removeNFirstLines xmlTokLinesCode 3) "" 0--remove class name {
            let onlyTable = getElemBeforeLast varsClassTable
            let currentChar=read (getLast varsClassTable!!0)
            let className=(words(xmlTokLinesCode!!1))!!1
            let currCode=removeNFirstLines xmlTokLinesCode (3 + currentChar)
            let b = parseSubRoutineDec currCode onlyTable "" className  0
            removeLastElem (removeLastElem varsClassTable) ++ removeLastElem (removeLastElem b)

--params of func (int a,b,...)

argumentsList::[String]->String->[String]
argumentsList xmlTokLinesCode varsRoutineTable
  | (((words (xmlTokLinesCode!!0) )!!1)  == ")") =
           [varsRoutineTable]++["0"]
  | ((words (xmlTokLinesCode!!1))!!1  == ")") =do
           let type1=words(head xmlTokLinesCode)!!1
           let kind=words(xmlTokLinesCode!!1)!!1
           let addvar = getCurTableRoutine varsRoutineTable "argument" type1 kind
           addvar:["2"]
  | otherwise = do--                                                                  type                              name                                                        
           let type1=words(head xmlTokLinesCode)!!1
           let kind=words(xmlTokLinesCode!!1)!!1
           let tableWithNewVar = getCurTableRoutine varsRoutineTable "argument" type1 kind
           let tableWithArguments = moreArguments (removeNFirstLines xmlTokLinesCode 2) tableWithNewVar 0
           let currentChar=read ((getLast tableWithArguments)!!0)
           removeLastElem tableWithArguments++[(show (2 + currentChar))]


--add local(var) to the table
onevarDec::[String]->String->[String]
onevarDec xmlTokLinesCode varsRoutineTable =do
                                                        let vartype=words(xmlTokLinesCode!!1)!!1
                                                        let name=(words (xmlTokLinesCode!!2))!!1
                                                        let varsRoutineTableUp = getCurTableRoutine varsRoutineTable "local" vartype name
                                                        let varsRoutineTableUp1 = moreVars (removeNFirstLines xmlTokLinesCode 3) "var local"  (xmlTokLinesCode!!1) "" varsRoutineTableUp 0
                                                        let onlyvarsRoutineTable = getElemBeforeLast varsRoutineTableUp1
                                                        let curChar=read ((getLast varsRoutineTableUp1)!!0)
                                                        removeLastElem (removeLastElem varsRoutineTableUp1) ++ [onlyvarsRoutineTable] ++[show (4 +curChar)]



varDec::[String]->String->String->Int->[String]
varDec xmlTokLinesCode varsRoutineTable varsClassTable n =
    do
        let firstTok=words (xmlTokLinesCode!!0)!!1
        if(firstTok == "let" || firstTok == "if" || firstTok == "while" || firstTok == "do" || firstTok == "return" || firstTok == "}")--end of local
         then varsRoutineTable : [(show n)]
             else do
                let varsRoutineTableUp= getElemBeforeLast (onevarDec xmlTokLinesCode varsRoutineTable)
                let a=removeLastElem(removeLastElem (onevarDec xmlTokLinesCode varsRoutineTable))
                let currChar=read (getLast (onevarDec xmlTokLinesCode varsRoutineTable)!!0)
                let currCode=removeNFirstLines xmlTokLinesCode currChar
                a ++ varDec currCode varsRoutineTableUp varsClassTable (n+currChar )


subroutineBody:: [String]->String->String->String->[String]
subroutineBody xmlTokLinesCode varsRoutineTable varsClassTable className = do
                                                let varsRoutineTableAll = varDec(removeNFirstLines xmlTokLinesCode 1) varsRoutineTable varsClassTable 0--add local (remove{ from the code)
                                                let varsRoutineTableAllOnly = getElemBeforeLast varsRoutineTableAll
                                                let currCharV=read ((getLast varsRoutineTableAll)!!0)
                                                let st = mstatements (removeNFirstLines xmlTokLinesCode (1+currCharV)) varsRoutineTableAllOnly varsClassTable className
                                                let curCharS=read ((getLast st)!!0)
                                                removeLastElem(removeLastElem varsRoutineTableAll)++removeLastElem st++[varsRoutineTableAllOnly]++[show (2+(currCharV ) + curCharS)]



mstatements::[String]->String->String->String->[String]
mstatements xmlTokLinesCode varsRoutineTable varsClassTable className = do
                                        let statenents1 = checktatements xmlTokLinesCode varsRoutineTable varsClassTable className 0
                                        removeLastElem statenents1 ++ [head (getLast statenents1)]


subRoutineDec::[String]->String->String->String->[String] -- parameterList  + Body
subRoutineDec xmlTokLinesCode varsClassTable varsRoutineTable className =
                                        do
                                                let rotineType=(words(xmlTokLinesCode!!0))!!1
                                                if rotineType == "constructor"
                                                then do
                                                                let curCode=removeNFirstLines xmlTokLinesCode 4
                                                                let paramsTable = argumentsList curCode varsRoutineTable
                                                                let onlyparamsTable= getElemBeforeLast paramsTable--new record
                                                                let curCode1=removeNFirstLines xmlTokLinesCode (5 + ((read ((getLast paramsTable)!!0)) ))
                                                                let body = subroutineBody curCode1 onlyparamsTable varsClassTable className
                                                                let newMs2 = getElemBeforeLast body
                                                                --classname.funcname num of locals in the func
                                                                let funcname=className ++ "." ++ words(xmlTokLinesCode!!2)!!1++" " ++show (countLocal (lines newMs2)) 
                                                                ["function" ++ " " ++funcname ] ++
                                                                 ["push constant " ++ show (countField (lines varsClassTable)) ]++["call Memory.alloc 1"]++["pop pointer 0"] ++ 
                                                                 removeLastElem(removeLastElem paramsTable)++ --not need
                                                                 removeLastElem (removeLastElem body)++
                                                                 [show(5+read ((getLast paramsTable)!!0)+read ((getLast body)!!0))]
                                                else if(rotineType == "method")
                                                        then do 
                                                                let tableWithThis = getCurTableRoutine varsRoutineTable "argument" className "this"--this-pointer to class
                                                                let tableWithAllArgs = argumentsList (removeNFirstLines xmlTokLinesCode 4) tableWithThis
                                                                let onlytableWithAllArgs = getElemBeforeLast tableWithAllArgs--table with all args
                                                                let currChar=read ((getLast tableWithAllArgs)!!0)
                                                                let currCode=removeNFirstLines xmlTokLinesCode (5 +currChar )
                                                                let body = subroutineBody currCode onlytableWithAllArgs varsClassTable className
                                                                let allTable = getElemBeforeLast body
                                                                --classname.funcname num of locals in the func
                                                                let funcname=className ++ "." ++ (words(xmlTokLinesCode!!2))!!1 ++ " " ++ show (countLocal (lines allTable)) 
                                                                ["function " ++funcname ]
                                                                 ++["push argument 0"] ++["pop pointer 0"] ++ --get this
                                                                 removeLastElem (removeLastElem tableWithAllArgs) ++
                                                                 removeLastElem (removeLastElem body) ++ [show (5 + read ((getLast tableWithAllArgs)!!0) + read ((getLast body)!!0) )]
                                                else do --function
                                                        let tableWithAllArgs = argumentsList (removeNFirstLines xmlTokLinesCode 4) varsRoutineTable
                                                        let tableWithArgs = getElemBeforeLast tableWithAllArgs
                                                        let body = subroutineBody (removeNFirstLines xmlTokLinesCode (5 + ((read ((getLast tableWithAllArgs)!!0)) ))) tableWithArgs varsClassTable className
                                                        let curChar=show (5 + read ((getLast tableWithAllArgs)!!0) + read ((getLast body)!!0) )
                                                        let allTable = getElemBeforeLast body
                                                        let funcName= (words(xmlTokLinesCode!!2))!!1 
                                                        ["function " ++ className ++ "." ++ funcName ++ " "
                                                         ++ show (countLocal (lines allTable))  ] ++ removeLastElem (removeLastElem tableWithAllArgs) ++ 
                                                         removeLastElem (removeLastElem body)
                                                         ++ [curChar]





parseSubRoutineDec:: [String]->[Char ]->String->String->Int->[String]-- c-tor||func||method
parseSubRoutineDec xmlTokLinesCode varsClassTable varsRoutineTable className n =
                                                if ((words(xmlTokLinesCode!!0))!!1 == "}")-- if subRoutineDec is exist or no more routine
                                                        then varsRoutineTable : [show(n)]
                                                else do
                                                                let s= subRoutineDec xmlTokLinesCode varsClassTable varsRoutineTable className
                                                                let curChar=(read ((getLast s)!!0))
                                                                let curCode=removeNFirstLines xmlTokLinesCode curChar
                                                                removeLastElem s ++
                                                                 parseSubRoutineDec curCode varsClassTable varsRoutineTable className (n+curChar)


--build the class vars table
parseClassVarDec xmlTokLinesCode classVarTable n=
  --n is the num of lines of var declaration 
  do
    let firstTok=words (head xmlTokLinesCode)!!1--get word 2 of the first line     
    if(firstTok=="constructor" || firstTok == "function" || firstTok == "method"||firstTok=="}")--if there is no variables.
        then [classVarTable]++[show n]
             --classVarTable    current char idx
        else
            do
                let classVarTableUp1=mclassVarDec xmlTokLinesCode classVarTable-- insert one var to table
                let onlyTable =getElemBeforeLast classVarTableUp1
                let currentChar=read ((getLast classVarTableUp1)!!0)
                removeLastElem(removeLastElem classVarTableUp1) ++
                 parseClassVarDec (removeNFirstLines xmlTokLinesCode (currentChar)) onlyTable (n+currentChar )
                --recursive


mclassVarDec :: [String] -> String -> [String]
mclassVarDec code classVarTable =
    do --                                                 field/sta             type                  name
         let currTable = getCurTableClass classVarTable (words(code!!0)!!1) (words(code!!1)!!1) (words(code!!2)!!1)--add var to the table

         let varList = moreVars (removeNFirstLines code 3) (code!!0) (code!!1) currTable "" 0--field int a,b,c...
         let currTableUp = getElemBeforeLast varList
         [currTableUp ]++ [show (4 + ((read ((getLast varList)!!0)) ))]


getCurTableClass ::  String -> String -> String -> String -> String
getCurTableClass classVarTable storfi vartype name = if storfi == "static"
                                                     then
                                                             do
                                                                let c= staticvarclassnum
                                                                let newVar=[name ++ " " ++ vartype ++ " " ++ storfi++" "++  show (countStatic (lines classVarTable))  ]
                                                                unlines ((lines classVarTable) ++ newVar)
                                                      else
                                                             do
                                                                let c= staticvarclassnum
                                                                let newVar=[name ++ " " ++ vartype ++ " " ++ storfi++" "++  show (countField (lines classVarTable)) ]
                                                                unlines ( (lines classVarTable) ++newVar )
--



moreArguments ::[String]->String->Int->[String]
moreArguments xmlTokLinesCode varsRoutineTable n = 
        do
                 let ch=(words (head xmlTokLinesCode) )!!1
                 if ch == ")"
                                                then varsRoutineTable : [(show n)]
                  else do
                                                let vartype=words (xmlTokLinesCode!!1)!!1
                                                let name=words(xmlTokLinesCode!!2)!!1
                                                let tableWithNewVar = getCurTableRoutine varsRoutineTable "argument" vartype name
                                                let updateCode= removeNFirstLines xmlTokLinesCode 3 --code without type name ,                                                    
                                                moreArguments updateCode  tableWithNewVar (n+3)



moreVars ::[String]->String->String->String->String->Int->[String]-- if more then 1 ;
moreVars xmlTokLinesCode kind varType classVarTable varsRoutineTable n
  | words (xmlTokLinesCode !! 0)!! 1== ";"
  = if ((words kind) !! 1) == "field" || ((words kind) !! 1) == "static" then
        classVarTable : [show n]--field int a;
    else
        varsRoutineTable : [show n]
  | ((words kind) !! 1) == "field" || ((words kind) !! 1) == "static"
  = do 
          let classVarTableUp = getCurTableClass classVarTable ((words kind) !! 1) ((words varType) !! 1) ((words (xmlTokLinesCode !! 1)) !! 1)
          moreVars (removeNFirstLines xmlTokLinesCode 2) kind varType classVarTableUp varsRoutineTable (n + 2)
  | otherwise
  = do let a --routine
             = getCurTableRoutine varsRoutineTable ((words kind) !! 1) ((words varType) !! 1) ((words (xmlTokLinesCode !! 1)) !! 1)
       moreVars (removeNFirstLines xmlTokLinesCode 2) kind varType classVarTable a (n + 2)




--expr(,expr)*

expressionList :: [String] ->String->String->String->[String]
expressionList xmlTokLinesCode varsRoutineTable varsClassTable className = if ((words (xmlTokLinesCode!!0))!!1) == ")"
                                                then "0" : ["0"]
                                                else do
                                                        let firstExpr = expression xmlTokLinesCode varsRoutineTable varsClassTable className
                                                        let endExprIndex = read ((getLast firstExpr)!!0)   
                                                        let exprList = moreExpressions (removeNFirstLines xmlTokLinesCode (endExprIndex)) varsRoutineTable varsClassTable className 0 0 --after op
                                                        let endExpListIndex = read ((getLast exprList)!!0)     + endExprIndex
                                                        let argNum=show(1 + (read (getElemBeforeLast exprList))) 
                                                        removeLastElem firstExpr ++
                                                         removeLastElem (removeLastElem exprList) ++ 
                                                         [argNum] ++ 
                                                         [show endExpListIndex]--count paramerets in list


moreExpressions :: [String]-> String->String->String->Int -> Int->[String]
moreExpressions xmlTokLinesCode varsRoutineTable varsClassTable className num n =
                       do
                                let a=(words (xmlTokLinesCode!!0))!!1 
                                if a/= ")"
                                        then do
                                                let curCode=removeNFirstLines xmlTokLinesCode 1
                                                let  exp = expression curCode varsRoutineTable varsClassTable className--remove ,
                                                let end = 1+ read ((getLast exp)!!0)  
                                                let curCode1=removeNFirstLines xmlTokLinesCode end
                                                removeLastElem exp ++
                                                 moreExpressions curCode1 varsRoutineTable varsClassTable className (num+1)  (n + end)
                                else (show num) : [(show n)]


--helperFunctions


pushString :: [Char ] ->[String]
pushString "" = []
pushString (x:y) = 
        do
                let asciiChar=show (ord x)
                ["push constant " ++asciiChar] ++ 
                 ["call String.appendChar 2"] ++
                  pushString y

getVarIdx:: [String]->String -> String
getVarIdx [] name = ""
getVarIdx (x:y) name = if ((words x)!!0) == name then  (words x)!!3 else getVarIdx y name

removeNLastLines:: String->Int->String
removeNLastLines y n = if n == 1 then removeLastChar y else removeNLastLines (removeLastChar y) (n-1)


--local/static/arg/static
kindOf:: [String]->[Char ] -> [Char ]
kindOf [] name = ""
kindOf (var1:table) name= 
        do
                let kind=(words var1)!!2
                if ((words var1)!!0) == name
                        then if kind == "field" then "this"--push this 1
                           else kind --local/static/arg
                     else kindOf table name



removeLastChar :: [a] -> [a]
removeLastChar [] = []
removeLastChar (ch:str) = if(length str) == 0 then [] else [ch] ++ (removeLastChar str)

getLast :: [String] -> [String]
getLast []= []
getLast (x:y) = if(length y) == 0 then [x] else getLast y


countStatic:: [String]-> Int
countStatic [] = 0
countStatic (var1:table) = 
        do
                let kind=(words var1)!!2
                if (kind) == "static"
                                then 1+ (countStatic table)
                                else countStatic table


countField :: [[Char ]] -> Int
countField [] = 0
countField (var1:table) = 
        do
                let kind=(words var1)!!2
                if (kind) == "field" then 
                        1 + (countField table)
                         else countField table


getElemBeforeLast :: [String]->String
getElemBeforeLast []=""
getElemBeforeLast(x:y)=if(length y)==1 then x else getElemBeforeLast y

countLocal:: [String]-> Int
countLocal [] = 0
countLocal (x:y) = if ((words x)!!2) == "local"
                                        then 1 + (countLocal y)
                                        else countLocal y


countArg:: [String]-> Int
countArg [] = 0
countArg (x:y) = 
        do
                let kind=(words x)!!2
                if kind == "argument"
                                then 1+ (countArg y)
                                else countArg y


mparser  xmlTokenPath xmlPathDst=
        do      --send to parser
                --writeFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\staticvarclass.txt" "0"
                --writeFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\fieldvarclass.txt" "0"
                xmlTokFile<-openFile xmlTokenPath ReadMode
                xmlTokCode<-hGetContents xmlTokFile
                let resultCodeList= parseClass (removeLastElem (removeNFirstLines (lines xmlTokCode) 1))  --remove "<token>" "</token>"
                writeFile  xmlPathDst (unlines resultCodeList)
                print "n"


--count static var class
staticvarclassnum =
    do
        statVar<-openFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\staticvarclass.txt" ReadMode
        statVarContent<-hGetContents statVar
        let varLines=lines statVarContent
        let l=words (head varLines)
        let t=read (head l)::Integer
        removeFile  "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\staticvarclass.txt"

        writeFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\staticvarclass.txt" (show (t+1))
        return (head l)




--count field var class
fieldvarclassnum :: IO Integer
fieldvarclassnum =
    do
        statVar<-openFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\fieldvarclass.txt" ReadMode
        statVarContent<-hGetContents statVar
        let varLines=lines statVarContent
        let l=words (head varLines)
        let t=read (head l)::Integer
        removeFile  "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\fieldvarclass.txt"
        writeFile "C:\\Users\\tahel\\Desktop\\degree\\ekronot\\fieldvarclass.txt" (show (t+1))

        return t


removeNFirstLines [] n =[]
removeNFirstLines (line:linesList) n = if n==1 then linesList else removeNFirstLines linesList (n-1)

removeLastElem :: [String] -> [String]
removeLastElem [] = []
removeLastElem (str:strlist) = if(null) strlist then [] else str : (removeLastElem strlist)



--remove the first charactor from the string
removeFirst :: [a] -> [a]
removeFirst (ch:str) = str

--remove // comment
remove1LineComment :: String->String
remove1LineComment str = if head str  == '\n' then removeFirst str else remove1LineComment (removeFirst str)

--remove /**/ comment

removeLinesComment:: String->String
removeLinesComment str = if str!!0 == '*'&&str!!1 == '/'
                                   then removeFirst (removeFirst str)--remove */
                               else removeLinesComment(removeFirst str)


--stringConstant		
mstringConstant :: String->[String]
mstringConstant ('"':y) = [" </stringConstant>", y]--the string over
mstringConstant (x:y) = [x : (mstringConstant y)!!0, mstringConstant y!!1]--the string continue

--integerConstant
mintegerConstant :: String->[String]
mintegerConstant (x:y) = if isDigit x then
     [x : (mintegerConstant y)!!0,  mintegerConstant y!!1]--there is more digits
      else [" </integerConstant>", x : y]--the int over



-- find the current symbol
whichSymbol :: String->[String]
whichSymbol ('&':y) = ["&amp;" ++ " </symbol>" ,y]
whichSymbol ('<':y) = ["&lt;" ++ " </symbol>" ,y]
whichSymbol ('>':y) = ["&gt;" ++ " </symbol>" ,y]
whichSymbol (x:y) = [ x : " </symbol>" ,y]


--return word
returnToken:: String -> [String]
returnToken (ch:str) = if isLetter ch || ch == '_' || isDigit ch then
                           [ch : ( head (returnToken str)) ,returnToken str!!1]
                                           else --the word over
                                                                                           ["", ch : str ]

keywordOrIdentifier:: String -> [String]  -> [String]
keywordOrIdentifier word keywords= do
                                let keyOrId= returnToken word
                                if keyOrId!!0 `elem` keywords
                                        then ["<keyword> " ++ head keyOrId ++ " </keyword>", keyOrId!!1]--the last elem is the next code
                                        else ["<identifier> " ++ head keyOrId ++ " </identifier>", keyOrId!!1]

