--Tahel Nadav 207816612
--Tzviya Laniado 315121798

import Control.Applicative()
import System.Directory
import System.IO
import Data.List()
import Data.Char ( isLetter, isDigit )
import Control.Monad()
import System.Environment()
import System.FilePath
import Data.Maybe ()



readFilesDirectory = do putStr "Enter directory path:"
                        directoryPath<-getLine
                        let directoryfiles = getDirectoryContents directoryPath
                        jackFilesList<-filter(\currfile->takeExtension currfile==".jack")<$>directoryfiles
                        mapM_ (\currFile-> jackToxmlTokenizing (directoryPath++"/"++currFile) (directoryPath++"/"++takeBaseName currFile++"T.xml")) jackFilesList
                        sendFilesToParser directoryPath
                        print jackFilesList




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
  | length jCode ==0 = []
  | ((jCode!!0 == '/' ) && (jCode!!1 == '/' )) = (makeTokens (remove1LineComment jCode) keywords symbols)--remove 1 line
  | ((jCode!!0 == '/' ) && (jCode!!1 == '*' )) = (makeTokens (removeLinesComment jCode) keywords symbols)--remove /**/
  | (length jCode>0&& isDigit(jCode!!0)) = (  ["<integerConstant> " ++ ((integerConstant jCode)!!0)]) ++ (makeTokens ((integerConstant jCode)!!1) keywords symbols)
  | (length jCode>0&&jCode!!0 == '"') = (["<stringConstant> " ++ ( (stringConstant (removeFirst jCode))!!0)]) ++ (makeTokens ((stringConstant (removeFirst jCode))!!1) keywords symbols)
  | (length jCode>0&&  [jCode!!0] `elem` symbols) = ["<symbol> " ++ ( (whichSymbol jCode)!!0)] ++ (makeTokens ((whichSymbol jCode)!!1) keywords symbols )
  | ((jCode!!0 == ' ' ) || (jCode!!0 == '\n' ) || (jCode!!0 == '\t' )) = (makeTokens (removeFirst jCode) keywords symbols)
  | otherwise = ([(keywordOrIdentifier jCode keywords)!!0]) ++ (makeTokens ((keywordOrIdentifier jCode keywords)!!1) keywords symbols)--or keyword or id


--send xml files to parser
sendFilesToParser::String->IO()
sendFilesToParser directoryPath=
        do
                let directoryfiles = getDirectoryContents directoryPath
                xmlFilesList<-filter(\currfile->takeExtension currfile==".xml")<$>directoryfiles
                mapM_(\currFile-> parser (directoryPath++"/"++currFile) (directoryPath++"/"++removeLastChar (takeBaseName  currFile)++".xml")) xmlFilesList


parser  xmlTokenPath xmlPathDst=
        do      --send to parser
                xmlTokFile<-openFile xmlTokenPath ReadMode
                xmlTokCode<-hGetContents xmlTokFile
                let resultCodeList= parseClass(lines xmlTokCode)
                writeFile  xmlPathDst (unlines resultCodeList) 
                print "n"


--remove the first charactor from the string
removeFirst :: [a] -> [a]
removeFirst (ch:str) = str

--remove // comment
remove1LineComment :: String->String
remove1LineComment str = if str!!0  == '\n' then removeFirst str else remove1LineComment (removeFirst str)

--remove /**/ comment

removeLinesComment:: String->String
removeLinesComment str = if (str!!0 == '*' )&&(str!!1 == '/' )
                                   then removeFirst (removeFirst str)--remove */
                               else removeLinesComment(removeFirst str)

--stringConstant		
stringConstant :: String->[String]
stringConstant ('"':y) = [" </stringConstant>", y]--the string over
stringConstant (x:y) = [[ x] ++ (stringConstant y)!!0, stringConstant y!!1]--the string continue

--integerConstant
integerConstant :: String->[String]
integerConstant (x:y) = if isDigit x then
     [[x] ++ (integerConstant y)!!0,  integerConstant y!!1]--there is more digits
      else [" </integerConstant>", [x] ++ y]--the int over

-- find the current symbol
whichSymbol :: String->[String]
whichSymbol ('&':y) = ["&amp;" ++ " </symbol>" ,y]
whichSymbol ('<':y) = ["&lt;" ++ " </symbol>" ,y]
whichSymbol ('>':y) = ["&gt;" ++ " </symbol>" ,y]
whichSymbol (x:y) = [ x : " </symbol>" ,y]


--return word
returnToken:: String -> [String]
returnToken (ch:str) = if isLetter ch || ch == '_' || isDigit ch then
                           [ch : ( (returnToken str)!!0) ,returnToken str!!1]
                                           else --the word over
											   ["", ch : str ]

keywordOrIdentifier:: String -> [String]  -> [String]
keywordOrIdentifier word keywords= do
                                let keyOrId= returnToken word
                                if (keyOrId!!0) `elem` keywords
                                        then ["<keyword> " ++ keyOrId!!0 ++ " </keyword>", keyOrId!!1]--the last elem is the next code
                                        else ["<identifier> " ++ keyOrId!!0 ++ " </identifier>", keyOrId!!1]






parseClass xmlTokLinesCode=
        do
            let specList=removeLastElem(removeFirst xmlTokLinesCode)   --remove "<class>" "</class>"
            let classVarDec = parseClassVarDec (removeNFirstLines specList 3) 0  --get var of class declarations code
            let subRoutine=(parseSubRoutineDec (removeNFirstLines specList (3 + (read (getLast classVarDec!!0)))) 0)              
            let basicDecleration=[specList!!0] ++ [specList!!1] ++ [specList!!2]
            let lastLine=specList!!((read (getLast classVarDec!!0)) + 3 +(read (getLast subRoutine!!0)))--the symbol }
            do ["<class>"] ++ basicDecleration ++ (removeLastElem classVarDec) ++ (removeLastElem subRoutine) ++
			        		[lastLine] ++ ["</class>"] --all of the xml code


parseClassVarDec xmlTokLinesCode n=
  --n is the num of lines of var declaration 
  do
    let firstTok=words (xmlTokLinesCode!!0)!!1--get word 2 of the first line
     
    if(firstTok=="constructor" || firstTok == "function" || firstTok == "method"||firstTok=="}")--if there is no variables.
      then [show n]
      else
		  do
			  let varList= (removeLastElem (classVarDec xmlTokLinesCode))  --one classVarDec
			  let indexOfCurrentLine=read (getLast  (classVarDec xmlTokLinesCode)!!0)
			  do varList++(parseClassVarDec (removeNFirstLines xmlTokLinesCode (indexOfCurrentLine)) (n+indexOfCurrentLine)) 


classVarDec code=
  do
    let listOfVars = moreVars (removeNFirstLines code 3) 0   --field int a, b, c; 
    let symbol=code!!(3 + (read (getLast listOfVars!!0)))      --get the line after the vars list definition--->symbol ;
    let count=show (4 + (read (getLast listOfVars!!0)))        --count of varlist
	--[code!!0] ++ [code!!1] ++ [code!!2]   -->  field/static type id
    do ["<classVarDec>"]  ++ [code!!0] ++ [code!!1] ++ [code!!2]  ++ (removeLastElem listOfVars)++[symbol]++  ["</classVarDec>"]++[count]



moreVars ::[String]->Int->[String]-- if more then 1 ;
moreVars code n = if(((words (code!!0))!!1) == ";")  --if the list vars ended
						then [(show n)] 
						else [code!!0] ++ [code!!1] ++ (moreVars (removeNFirstLines code 2) (n+2))
            --n is the num of lines of var declaration 


parseSubRoutineDec:: [String]->Int->[String]-- ctor||func||method
parseSubRoutineDec code n = if ((words(code!!0))!!1 == "}" )-- if  doesnt exist 
							then [show(n)]
							else  (removeLastElem (subRoutineDec code)) ++ (parseSubRoutineDec (removeNFirstLines code (read (getLast (subRoutineDec code)!!0))) (n+(read (getLast (subRoutineDec code)!!0)))) 
							
subRoutineDec::[String] ->[String] -- parameterList  + Body
subRoutineDec code = do 
						let routineArguments = argumentsList (removeNFirstLines code 4)--argumentsList of routine
						let body = subroutineBody (removeNFirstLines code (5 + (read ((getLast routineArguments)!!0))))--send code from {
						let argEnd=4 + (read ((getLast routineArguments)!!0))--idx of symbol )
						let bodyEnd=(read ((getLast body)!!0))
						let basicDec=[code!!0]++[code!!1]++[code!!2]++[code!!3]--functon void f(
						do ["<subroutineDec>"]++basicDec ++(removeLastElem routineArguments) ++
							   [code!!argEnd] ++ (removeLastElem body) ++ 
							   ["</subroutineDec>"] ++ [show (5 + (read ((getLast routineArguments)!!0)) +  bodyEnd)] 		
--params of func
argumentsList::[String]->[String] 
argumentsList code = if(((words (code!!0) )!!1)  == ")")-- if there is no arguments 
								then ["<parameterList>"] ++ ["</parameterList>"] ++ ["0"]
									else ["<parameterList>"]++[code!!0] ++[code!!1]++ (removeLastElem (moreArguments (removeNFirstLines code 2) 0)) ++["</parameterList>"] 
										++ [(show (2 + (read ((getLast (moreArguments (removeNFirstLines code 2) 0))!!0) )))]						

--params of func
moreArguments ::[String]->Int->[String] --(int n2, int n3...)
moreArguments code n =  if (((words (code!!0) )!!1) == ")")
						then [(show n)]
					else [code!!0] ++ [code!!1] ++ (moreArguments (removeNFirstLines code 2) (n+2)) 										
							
subroutineBody:: [String]->[String]
subroutineBody code = do
						let localVars = (varsDec(removeNFirstLines code 1) 0)--the local vars should be in begining (send without {)
						let currentStatements = statements (removeNFirstLines code (1+(read ((getLast localVars)!!0) )))
						let routineEnd=code!!((read ((getLast localVars)!!0)) + 1 + (read ((getLast currentStatements)!!0)))--the } symbol
						let endRoutineIdx=2+ read ((getLast localVars)!!0) + read ((getLast currentStatements)!!0)
						let routineBobyCode=[code!!0] ++ (removeLastElem localVars) ++ (removeLastElem  currentStatements) ++ [routineEnd]
						do  ["<subroutineBody>"] ++ routineBobyCode  ++ ["</subroutineBody>"] ++ [show (endRoutineIdx) ]


--local vars of func
varsDec::[String]->Int->[String]
varsDec code n =
	do
		let a=words (code!!0)!!1
		if(a=="let" || a == "if" || a == "while" || a == "do" || a == "return"|| (((words (code!!0) )!!1) == "}"))--if weare in statement or we finished
			then [(show n)]
			else (removeLastElem (onevarDec code)) ++ (varsDec (removeNFirstLines code (read ((getLast  (onevarDec code))!!0))) (n+(read ((getLast  (onevarDec code))!!0))))

--local vars of func
onevarDec::[String]->[String]
onevarDec code = 
	do
		let baseC=[code!!0]  ++ [code!!1]  ++[code!!2]--var type name
		let more=(removeLastElem (moreVars (removeNFirstLines code 3) 0))--more than one var  in dec
		let sym=[code!!(3+(read (getLast (moreVars (removeNFirstLines code 3) 0)!!0) ))]--symbol ;
		let idx=[(show (4 +(read ((getLast (moreVars (removeNFirstLines code 3) 0))!!0))))]
		do   ["<varDec>"]++ baseC  ++ more ++sym ++ 	["</varDec>"] ++ idx
						
								

statements::[String]->[String]
statements code = ["<statements>"]++ (removeLastElem (checkStatements code 0)) ++ ["</statements>"] ++ [ getLast  (checkStatements code 0)!!0 ]
							
--statement*
checkStatements :: [String]->Int->[String]
checkStatements code n = if (((words (code!!0) )!!1) == "}") --if the body ended
							then [(show n)]
							else if (((words (code!!0) )!!1) == "let")--let statment
								then (removeLastElem (letStatement code)) ++ (checkStatements (removeNFirstLines code (read ((getLast (letStatement code))!!0) )) (n + (read ((getLast (letStatement code))!!0))))		
								else if (((words(code!!0) )!!1) == "if") --if statement
										then (removeLastElem(ifStatement code)) ++ (checkStatements (removeNFirstLines code(read ((getLast (ifStatement code))!!0) )) (n + (read ((getLast (ifStatement code))!!0))))
										else if (((words (code!!0))!!1) == "while") --while statement
												then (removeLastElem (whileStatement code)) ++ (checkStatements (removeNFirstLines code (read ((getLast (whileStatement code))!!0))) (n + (read ((getLast (whileStatement code))!!0) )))
											else if (((words (code!!0) )!!1) == "do") --do statment
													then (removeLastElem (doStatement code)) ++ (checkStatements (removeNFirstLines code(read ((getLast (doStatement code))!!0))) (n + (read ((getLast (doStatement code))!!0) )))
												else if (((words (code!!0) )!!1) == "return") --return statement
														then (removeLastElem (returnStatement code)) ++ (checkStatements (removeNFirstLines code (read ((getLast (returnStatement code))!!0))) (n + (read ((getLast (returnStatement code))!!0) )))
													else []



letStatement::[String]->[String]
letStatement code =if(((words (code!!2) )!!1) == "[")--let id[]
						then do
							let dstExpr = (expression (removeNFirstLines code 3)) --id[dstExpr], before =
							let endDstExprIndex = (3+(read ((getLast dstExpr)!!0) ))
							let srcExpr = (expression (removeNFirstLines code (endDstExprIndex + 2)))--after =
							let basicDec=[code!!0] ++[code!!1] ++ [code!!2]--let id[
							["<letStatement>"]++ basicDec++ (removeLastElem dstExpr) ++ 
						 	 [code!!endDstExprIndex] ++ [code!!(endDstExprIndex + 1)] ++ (removeLastElem srcExpr) ++ [code!!(endDstExprIndex + 2 + 
							 (read ((getLast srcExpr)!!0)))] ++ ["</letStatement>"] ++ 
							 [(show (endDstExprIndex + 3 + (read ((getLast srcExpr)!!0))))]	  --let id[dstExpr]=srcExpr
					else do--let a=expr
						let srcExpr = (expression (removeNFirstLines code 3)) 
						let endExpIndex = (3 + (read ((getLast srcExpr)!!0)))
						let basicDec=[code!!0] ++[code!!1] ++ [code!!2] --let a=
						["<letStatement>"]++basicDec++ (removeLastElem srcExpr) ++ 
 						 [code!!endExpIndex] ++ ["</letStatement>"] ++ [(show (endExpIndex + 1))]	  
 

--return expr?;
returnStatement :: [String]->[String]
returnStatement code = do 
						if(((words (code!!1))!!1) == ";" )--return;
							then ["<returnStatement>"] ++[code!!0]++[code!!1]++["</returnStatement>"]++["2"]
						else do --return expr;
							let exp = (expression (removeNFirstLines code 1))
							let endExp = (read ((getLast exp)!!0) ) + 1
							["<returnStatement>"] ++[code!!0]++(removeLastElem exp) ++ [code!!endExp] ++ ["</returnStatement>"]++[(show (endExp + 1))]
						
--do routinecall;
doStatement :: [String]->[String]
doStatement code = do 
					let routineCode = (subRoutineCall (removeNFirstLines code 1))--routine
					let endRoutineIdx = ((read ((getLast routineCode)!!0) ) + 1)--;
					let doCode=[code!!0] ++ (removeLastElem routineCode) ++ [code!!endRoutineIdx]--do call;
					["<doStatement>"] ++ doCode ++ ["</doStatement>"] ++ [(show (endRoutineIdx + 1))]
			

--while(expr){statement}
whileStatement :: [String]->[String]
whileStatement code = do
						let conditionExpr = (expression (removeNFirstLines code 2))--into () 
						let endCondIdx = (read ((getLast conditionExpr)!!0) ) + 2--index of )
						let currentStatement = (statements (removeNFirstLines code (endCondIdx + 2)))
						let endSt =  endCondIdx	+ 2 +(read ((getLast currentStatement)!!0) )--index of }
						let condCode=[code!!0]++[code!!1] ++(removeLastElem conditionExpr)++[code!!endCondIdx]--while(cond)
						let bodyStatementCode=[code!!(endCondIdx+1)]++(removeLastElem currentStatement) ++ [code!!endSt]
						["<whileStatement>"]++ condCode++bodyStatementCode++ ["</whileStatement>"]++[(show(endSt+1))]
				

ifStatement:: [String]->[String]
ifStatement code = do --if(expr){statement} (else{statement})?
						let conditionExpr = (expression (removeNFirstLines code 2))
						let endCondIdx = (read ((getLast conditionExpr)!!0)  ) + 2
						let trueStatement = (statements (removeNFirstLines code (endCondIdx + 2)))--the body if the condition true
						let endTrueStIdx = endCondIdx + 2 + (read ((getLast trueStatement)!!0) )
						if ( ((words (code!!(endTrueStIdx+1)))!!1) == "else")-- if else exist  
							then do
								let elseStatement =  (statements (removeNFirstLines code (endTrueStIdx + 3)))
								let endElseIdx = endTrueStIdx + 3 + (read ((getLast elseStatement)!!0) )
								let ifAndCond=[code!!0]++[code!!1]++ (removeLastElem conditionExpr)++ [code!!endCondIdx]--if(cond)
								let trueBody=[code!!(endCondIdx+1)]++(removeLastElem trueStatement) ++[code!!endTrueStIdx]--{}
								let elseBody=[code!!(endTrueStIdx+1)] ++ [code!!(endTrueStIdx+2)] ++ (removeLastElem elseStatement) ++ [code!!(endElseIdx)]--else{}
								["<ifStatement>"]++ifAndCond ++ trueBody ++ elseBody ++ ["</ifStatement>"]  ++ [show(endElseIdx + 1)]
						else
							do
								let ifAndCond=[code!!0]++[code!!1]++ (removeLastElem conditionExpr)++ [code!!endCondIdx]--if(cond)
								let trueBody=[code!!(endCondIdx+1)]++(removeLastElem trueStatement) ++[code!!endTrueStIdx]--{}
								["<ifStatement>"]++ ifAndCond++ trueBody++["</ifStatement>"]++ [show(endTrueStIdx + 1)]
				
							

--term (op term)*
expression::[String]->[String]
expression code = do
					let firstTerm = term code  --first term
					let endTermIndex = (read ((getLast firstTerm)!!0) )
					let nextTerms=moreTerms(removeNFirstLines code endTermIndex) 0 --(op term)*
					let endGetTerm = (read ((getLast (nextTerms))!!0) )
					["<expression>"]++ (removeLastElem firstTerm) ++ (removeLastElem (nextTerms)) ++["</expression>"] ++[(show (endTermIndex + endGetTerm))]
				
					
moreTerms :: [String]-> Int ->[String] --get term after op or term ended
moreTerms code n = if ( ((words (code!!0))!!1) == "]" || ((words (code!!0))!!1) == ";"|| ((words (code!!0))!!1) == ")" ||((	words (code!!0))!!1) == ",") 
							then [(show n)]
							else do 
								--if code!!0 is op
								let terminal = (term (removeNFirstLines code 1))
								let endTerm = 1 + (read ((getLast terminal)!!0) )
								[code!!0] ++ (removeLastElem terminal) ++ (moreTerms (removeNFirstLines code endTerm) (n + endTerm)) 

term :: [String]->[String]
term code = do
				if (((words (code!!0))!!0) == "<identifier>") -- start id
					then if (((length code) > 1) && (((words (code!!1))!!1) == "[")) --varname[exp]
							then do
							let expIndex = (expression (removeNFirstLines code 2))  --[exp]
							let endDstIndex =(read ((getLast expIndex)!!0) ) + 2 --index of ]
							let letcode= [code!!0] ++ [code!!1] ++ (removeLastElem expIndex) ++ [code!!endDstIndex]
							["<term>"] ++ letcode++ ["</term>"] ++ [(show (endDstIndex+1))]
						 else if (((length code) > 1) && ((((words (code!!1))!!1) == "(") || (((words (code!!1))!!1) == ".")))--subroutinecall
							then do 
								let subRoutine = subRoutineCall code
								let endSrIndex = (getLast subRoutine)!!0 
								["<term>"] ++ (removeLastElem subRoutine) ++ ["</term>"] ++ [endSrIndex]
						 else ["<term>"] ++ [code!!0] ++ ["</term>"] ++ ["1"]									
				else if (((words (code!!0))!!1) == "(")--  (expr)
						then do
							let expr = (expression (removeNFirstLines code 1))
							let endExprIndex  = (read ((getLast expr)!!0)  ) + 1
							["<term>"] ++[code!!0] ++(removeLastElem expr) ++	[code!!endExprIndex] ++ ["</term>"] ++ [(show (endExprIndex+1))]	
				else if ((((words (code!!0))!!1) == "-")||(((words (code!!0))!!1) == "~"))--unary Operation term
						then do 
							let t = (term (removeNFirstLines code 1))
							let endT =(read ((getLast t)!!0))+1
							["<term>"] ++ [code!!0] ++(removeLastElem t) ++ ["</term>"] ++ [(show (endT))]
				else  ["<term>"] ++ [code!!0] ++ ["</term>"] ++ ["1"]--integer/stringconstant | keywordConstant | varname
				
subRoutineCall :: [String]->[String]
subRoutineCall code = if(((words (code!!1))!!1) == "(") --call func f()
							then do 
								let expList = (expressionList (removeNFirstLines code 2))
								let endExpL = 2 + (read ((getLast expList)!!0) )
								[code!!0]++[code!!1] ++ (removeLastElem expList) ++ [code!!endExpL] ++ [(show (endExpL+1))]
							else do                    --class/var.func()
								let expList2 = (expressionList (removeNFirstLines code 4))
								let endExpL2 = 4 + (read ((getLast expList2)!!0) )
								[code!!0]++[code!!1] ++[code!!2]++[code!!3] ++ 
								 (removeLastElem expList2) ++ [code!!endExpL2] ++ [(show (endExpL2 + 1))]

expressionList :: [String] -> [String]  --expr(,expr)*
expressionList code = if (((words (code!!0))!!1) == ")") -- if there is no more expr
						then ["<expressionList>"]++ ["</expressionList>"]++["0"]
						else do
							let firstExpr = (expression code)
							let endExprIndex = (read ((getLast firstExpr)!!0) )
							let exprList = (moreExpressions (removeNFirstLines code (endExprIndex)) 0)
							let endExpListIndex = (read ((getLast exprList)!!0) ) + endExprIndex									
							["<expressionList>"]++ (removeLastElem firstExpr) ++ (removeLastElem exprList) ++["</expressionList>"]++[(show endExpListIndex)]
						

moreExpressions :: [String]-> Int ->[String]--after expr of expressionList
moreExpressions code n = if (((words (code!!0))!!1) /= ")")--if there is comma
								then do 
									let  firstExpr = (expression (removeNFirstLines code 1))--first expr
									let endFirstExprIndex = 1+ (read ((getLast firstExpr)!!0))
									let more=moreExpressions (removeNFirstLines code endFirstExprIndex) (n + endFirstExprIndex)--get next expr if there is
									[code!!0] ++ (removeLastElem firstExpr) ++ more
								else [(show n)]

			
--helperFunctions
removeLastChar ([]) = []
removeLastChar (ch:str) = if((length str) == 0) then [] else ([ch] ++ (removeLastChar str)) 

getLast :: [String] -> [String]
getLast ([])= []
getLast (x:y) = if((length y) == 0) then [x] else getLast y 


removeNFirstLines [] n =[]
removeNFirstLines (line:linesList) n = if n==1 then linesList else removeNFirstLines linesList (n-1)

removeLastElem :: [String] -> [String]
removeLastElem [] = []
removeLastElem (str:strlist) = if(length strlist) == 0 then [] else [str] ++ (removeLastElem strlist)

